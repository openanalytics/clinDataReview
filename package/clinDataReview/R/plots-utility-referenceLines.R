#' Add reference (horizontal/vertical/diagonal) lines
#' to a clinical data plot
#' @inheritParams clinDataReview-common-args
#' @return Updated \code{\link[ggplot2]{ggplot}} object.
#' @author Laure Cougnaud
#' @importFrom plyr ddply
#' @importFrom stats setNames
#' @importFrom ggplot2 geom_text geom_vline geom_hline geom_abline aes_string
#' @export
addReferenceLinesClinDataPlot <- function(
	gg, data, 
	xVar, yVar, xLim = NULL, yLim = NULL,
	refLinePars = NULL, 
	facetPars = NULL){

	if(!is.null(refLinePars)){
		
		facetVars <- getFacetVars(facetPars)
		
		# extract data for each reference line
		refLineData <- getDataReferenceLines(
			refLinePars = refLinePars, 
			data = data, facetPars = facetPars
		)
		
		# extract axis limits (to position the text)
		axesLim <- getAxisLimPlot(
			data = data, 
			xVar = xVar, yVar = yVar, 
			xLim = xLim, yLim = yLim, 
			facetPars = facetPars,
			refLineData = refLineData
		)
		
		# extract function name for each reference line
		refLineFct <- getFctTypeReferenceLines(refLinePars = refLinePars)
		
		for(iPar in seq_along(refLinePars)){
			
			linePar <- refLinePars[[iPar]]
			lineData <- refLineData[[iPar]]
			lineFct <- refLineFct[[iPar]]
			
			if(nrow(lineData) > 0){
			
				lineParNameAes <- getParFctReferenceLines(lineFct)
				lineParNameAes <- intersect(names(linePar), lineParNameAes)
				
				# create lines
				lineParAes <- setNames(as.list(lineParNameAes), lineParNameAes)
				lineAes <- do.call(aes_string, lineParAes)
				lineParOther <- linePar[setdiff(names(linePar), c(lineParNameAes, "label"))]
				lineArgs <- c(
					list(mapping = lineAes, data = lineData, show.legend = FALSE), 
					lineParOther
				)
				geomLineFct <- switch(lineFct,
					vline = geom_vline, 
					hline = geom_hline,
					abline = geom_abline
				)
				gg <- gg + do.call(geomLineFct, lineArgs)
				
				## annotate lines
				
				lineLabel <- linePar$label
				
				if(!(is.logical(lineLabel) && !lineLabel)){
		
					# create data.frame contain x/y position and label of annotation text:
					dataLineText <- ddply(lineData, facetVars, function(x){
						
						# extract x/y min to position the lines:
						# Note: vjust/hjust not (yet) supported by ggplotly,
						# so moved labels by a percentage of the axis
						dataText <- merge(data, x, by = facetVars, all.x = FALSE, all.y = TRUE)
						axesLimX <- merge(axesLim, x, by = facetVars, all.x = FALSE, all.y = TRUE)
						xMin <- axesLimX$xmin
						xPerc <- diff(range(axesLimX[, c("xmin", "xmax")]))*0.01
						yMin <- axesLimX$ymin
						yPerc <- diff(range(axesLimX[, c("ymin", "ymax")]))*0.02
						
						# extract x/y/label of the lines
						dataText[["label"]] <- if(!is.null(lineLabel) && is.character(lineLabel)){
							lineLabel
						}else{
							switch(lineFct,
								'abline' = paste0(
									prettyNum(dataText[, "slope"]), "*x+", 
									prettyNum(dataText[, "intercept"])
								),
								'hline' = prettyNum(dataText[, "yintercept"]),
								'vline' = prettyNum(dataText[, "xintercept"])
							)
						}
						dataText[["x"]] <- switch(lineFct,
							'abline' = xMin,
							'hline' = xMin,
							'vline' = dataText[, "xintercept"]
						) + xPerc * nchar(dataText[["label"]])
						dataText[["y"]] <- switch(lineFct,
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
		
	}
	
	return(gg)
	
}

#' Get the names of the \code{\link[ggplot2]{ggplot}} function
#' to use for the reference lines
#' @inheritParams clinDataReview-common-args
#' @return List of type of each reference lines, among:
#' 'vline', 'hline' and 'abline'.
#' @author Laure Cougnaud
#' @export
getFctTypeReferenceLines <- function(refLinePars){
	
	refLineFct <- vector("list", length = length(refLinePars))
	
	# for each reference line
	for(iPar in seq_along(refLinePars)){
		
		linePar <- refLinePars[[iPar]]
		
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
		
		refLineFct[[iPar]] <- ggLineFct
		
	}

	return(refLineFct)

}

#' Get parameter of function used for reference lines
#' @param type string with line type, either: 'hline',
#' 'abline' or 'vline'.
#' @return Character vector with parameter names of the functions
#' @author Laure Cougnaud
getParFctReferenceLines <- function(type){
	
	ggLineFct <- c(
		xintercept = "vline", yintercept = "hline", 
		slope = "abline", intercept = "abline"
	)
	pars <- names(ggLineFct[ggLineFct == type])
	
	return(pars)
	
}

#' Extract data for the reference lines
#' 
#' This function especially extracts the data if an
#' aesthetic variable is specified in the reference line parameters.
#' @inheritParams clinDataReview-common-args
#' @return List of data for the lines
#' @importFrom stats complete.cases
#' @author Laure Cougnaud
getDataReferenceLines <- function(refLinePars, data, facetPars = NULL){
	
	facetVars <- getFacetVars(facetPars)
	
	refLineData <- vector("list", length = length(refLinePars))
	
	refLineFct <- getFctTypeReferenceLines(refLinePars = refLinePars)
	
	# for each reference line
	for(iPar in seq_along(refLinePars)){
		
		linePar <- refLinePars[[iPar]]
		lineFct <- refLineFct[[iPar]]
		
		# format input data for lines
		# create data.frame contain values of aesthetic variable (per facet)
		# or aesthetic values directly
		
		# extract aesthetic variable
		lineParNameAes <- getParFctReferenceLines(type = lineFct) # possible parameters for ggplot2 fct
		lineParNameAes <- intersect(lineParNameAes, names(linePar)) # only the ones specified by the user
		lineParAes <- linePar[lineParNameAes]
		isLineAesVar <- sapply(lineParAes, function(x) x %in% colnames(data))
		lineAesVar <- unlist(lineParAes[isLineAesVar])
		
		dataLineAes <- dataLineFix <- NULL
		
		# variable used in aesthetic
		if(any(isLineAesVar)){ 
			varsLineAes <- c(lineAesVar, if(!is.null(facetPars))	facetVars)
			dataLineAes <- unique(data[, varsLineAes, drop = FALSE])
			# only keep records with non missing values in aesthetic var
			dataLineAes <- dataLineAes[complete.cases(dataLineAes), , drop = FALSE]
		}
		
		# value used in aesthetic
		if(any(!isLineAesVar)){ 
			dataLineFix <- as.data.frame(lineParAes[!isLineAesVar], stringsAsFactors = FALSE)
			if(length(facetVars) > 0)
				dataLineFix <- merge(x = unique(data[, facetVars, drop = FALSE]), y = dataLineFix, all.y = TRUE)
		}
		
		# combine data if variable and value both specified
		lineData <- if(!is.null(dataLineAes) && !is.null(dataLineFix)){
			merge(x = dataLineFix, y = dataLineAes, all = TRUE, by = facetVars) # full join
		}else	if(!is.null(dataLineAes)){
			dataLineAes
		}else	if(!is.null(dataLineFix)){
			dataLineFix
		}

		# names column by line type of aes (instead of variable)
		if(length(lineAesVar) > 0)
			colnames(lineData)[match(lineAesVar, colnames(lineData))] <- names(lineAesVar)
		
		refLineData[[iPar]] <- lineData
		
	}
	
	return(refLineData)
	
}