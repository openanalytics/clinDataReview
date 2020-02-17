#' Get facetting variables from facettin parameters.
#' @inheritParams medicalMonitoring-common-args
#' @return Character vector with facetting variable
#' @author Laure Cougnaud
#' @export
getFacetVars <- function(facetPars = NULL){
	
	facetVars <- lapply(facetPars[c("facets", "rows", "cols")], function(par)
		if(inherits(par, "formula"))	all.vars(par)	else	par
	)
	facetVars <- unique(unlist(facetVars))
	
	return(facetVars)
	
}

#' Add reference (horizontal/vertical/diagonal) lines
#' to a medical monitoring plot.
#' @param refLinePars (optional) List of parameters for reference line(s):
#' \itemize{
#' \item{aesthetic value(s) or variable(s)
#' (in this case column names of \code{data}) for reference lines.
#' The line position is controlled by the aesthetics supported in
#' \code{\link[ggplot2]{geom_vline}}, \code{\link[ggplot2]{geom_hline}} 
#' and \code{\link[ggplot2]{geom_abline}}.
#' }
#' \item{'label': }{(optional) Logical specifying if the line
#' should be annotated (\code{FALSE} to not annotate the line)
#' or string with annotation label. By default, the value
#' of the horizontal/vertical line is displayed or the equation
#' for diagonal line.
#' }
#' }
#' @inheritParams medicalMonitoring-common-args
#' @return Updated \code{\link[ggplot2]{ggplot}} object.
#' @author Laure Cougnaud
#' @importFrom plyr ddply
#' @export
addReferenceLinesMonitoringPlot <- function(
	gg, data, 
	xVar, yVar, xLim = NULL, yLim = NULL,
	refLinePars = NULL, 
	facetPars = NULL){

	if(!is.null(refLinePars)){
		
		facetVars <- getFacetVars(facetPars)
		
		for(i in seq_along(refLinePars)){
			
			linePar <- refLinePars[[i]]
			
			# extract ggplot function to plot the line:
			ggLineFct <- c(
				xintercept = "vline", yintercept = "hline", 
				slope = "abline", intercept = "abline"
			)
			lineParAes <- linePar[intersect(names(ggLineFct), names(linePar))]
			ggLineFct <- unique(ggLineFct[names(lineParAes)])
			if(length(ggLineFct) == 0 | length(ggLineFct) > 1)
				stop(
					"Line parameters should contain at least",
					"fixed combinaisons of the",
					"'xintercept'/'yintercept'/'slope'/'intercept' parameters."
				)
			geomLineFct <- paste("geom", ggLineFct, sep = "_")
			
			# format input data for lines
			# create data.frame contain values of aesthetic variable (per facet)
			# or aesthetic values directly
			
			# extract aesthetic variable
			isLineAesVar <- sapply(lineParAes, function(x) x %in% colnames(data))
			lineAesVar <- unlist(lineParAes[isLineAesVar])
			
			dataLineAes <- dataLineFix <- NULL
			if(any(isLineAesVar)){ # variable used in aesthetic
				varsLineAes <- c(lineAesVar, if(!is.null(facetPars))	facetVars)
				dataLineAes <- unique(data[, varsLineAes, drop = FALSE])
			}
			if(any(!isLineAesVar)){ # value used in aesthetic
				dataLineFix <- as.data.frame(lineParAes[!isLineAesVar], stringsAsFactors = FALSE)
				dataLineFix <- merge(unique(data[, facetVars, drop = FALSE]), dataLineFix, all.y = TRUE)
			}
			dataLine <- if(!is.null(dataLineAes) && !is.null(dataLineFix)){
				merge(dataLineFix, dataLineAes, all = TRUE, by = facetVars) # full join
			}else	if(!is.null(dataLineAes)){
				dataLineAes
			}else	if(!is.null(dataLineFix)){
				dataLineFix
			}
			
			# create lines
			lineAes <- do.call(aes_string, lineParAes)
			lineParOther <- linePar[setdiff(names(linePar), names(lineParAes))]
			lineArgs <- c(
				list(mapping = lineAes, data = dataLine, show.legend = FALSE), 
				lineParOther
			)
			gg <- gg + do.call(geomLineFct, lineArgs)
			
			## annotate lines
			
			lineLabel <- linePar$label
			
			if(!(is.logical(lineLabel) && !lineLabel)){

				# extract axis limits (to position the text)
				axesLim <- getAxisLimPlot(
					data = data, 
					xVar = xVar, yVar = yVar, 
					xLim = xLim, yLim = yLim, 
					facetPars = facetPars
				)
	
				# create data.frame contain x/y position and label of annotation text:
				dataLineText <- ddply(dataLine, facetVars, function(x){
							
					# names column by line type of aes (instead of variable)
					if(length(lineAesVar) > 0)
						colnames(x)[match(lineAesVar, colnames(x))] <- names(lineAesVar)	
					
					# extract x/y min to position the lines:
					# Note: vjust/hjust not (yet) supported by ggplotly,
					# so moved labels by a percentage of the axis
					dataText <- merge(data, x, by = facetVars, all.x = FALSE, all.y = TRUE)
					axesLimX <- merge(axesLim, x, by = facetVars, all.x = FALSE, all.y = TRUE)
					xMin <- axesLimX$xmin
					xPerc <- diff(range(axesLimX[, c("xmin", "xmax")]))*0.001
					yMin <- axesLimX$ymin
					yPerc <- diff(range(axesLimX[, c("ymin", "ymax")]))*0.02
					
					# extract x/y/label of the lines
					dataText[["label"]] <- if(!is.null(lineLabel) && is.character(lineLabel)){
						lineLabel
					}else{
						switch(ggLineFct,
							'abline' = paste0(
								prettyNum(dataText[, "slope"]), "*x+", 
								prettyNum(dataText[, "intercept"])
							),
							'hline' = prettyNum(dataText[, "yintercept"]),
							'vline' = prettyNum(dataText[, "xintercept"])
						)
					}
					dataText[["x"]] <- switch(ggLineFct,
						'abline' = xMin,
						'hline' = xMin,
						'vline' = dataText[, "xintercept"]
					) + xPerc * nchar(dataText[["label"]])
					dataText[["y"]] <- switch(ggLineFct,
						'abline' = dataText[, "slope"] * xMin + dataText[, "intercept"],
						'hline' = dataText[, "yintercept"],
						'vline' = yMin
					) + yPerc
	
					unique(dataText[, c("x", "y", "label")])
				})
				
				aesLineTextOther <- lineParOther[c("color", "alpha")]
				aesLineTextOther <- aesLineTextOther[!sapply(aesLineTextOther, is.null)]
				
				argsLineText <- list(
					mapping = aes_string(x = "x", y = "y", label = "label"),
					data = dataLineText #, 
					# hjust = 0, vjust = 0
				)
				if(length(aesLineTextOther) > 0)
					argsLineText <- c(argsLineText, aesLineTextOther)
				gg <- gg + do.call(geom_text, argsLineText)
				
			}
			
		}
	}
	
	return(gg)
	
}

#' Get axis limits for a \code{\link[ggplot2]{ggplot}} plot
#' from the input dataset.
#' @return Data.frame with limits of the:
#' \itemize{
#' \item{x-axis: }{'xmin'/'xmax'}
#' \item{y-axis: }{'ymin'/'ymax'}
#' }
#' for each element of the facetting variable (if any).
#' @importFrom plyr ddply
#' @export
getAxisLimPlot <- function(data, 
	xVar, yVar, 
	xLim = NULL, yLim = NULL, 
	facetPars = NULL){
	
	facetVars <- getFacetVars(facetPars)
	
	scalePar <- facetPars$scales
	if(is.null(scalePar))	scalePar <- "fixed"
	
	axisLimits <- ddply(data, facetVars, function(x){
		xRange <- setNames(range(x[, xVar], na.rm = TRUE), paste0("x", c("min", "max")))
		yRange <- setNames(range(x[, yVar], na.rm = TRUE), paste0("y", c("min", "max")))
		c(xRange, yRange)
	})
	if(scalePar %in% c("fixed", "free_y")){
		axisLimits$xmin <- min(axisLimits$xmin)
		axisLimits$xmax <- max(axisLimits$xmax)
	}
	if(scalePar %in% c("fixed", "free_x")){
		axisLimits$ymin <- min(axisLimits$ymin)
		axisLimits$ymax <- max(axisLimits$ymax)
	}
	
	return(axisLimits)
	
}

