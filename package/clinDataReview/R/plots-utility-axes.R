#' Get axis limits for a \code{\link[ggplot2]{ggplot}} plot
#' from the input dataset.
#' @param refLineData Data used for the reference lines, as
#' output of the \code{\link{getDataReferenceLines}} function.
#' @inheritParams clinDataReview-common-args
#' @return Data.frame with limits of the:
#' \itemize{
#' \item{x-axis: }{'xmin'/'xmax'}
#' \item{y-axis: }{'ymin'/'ymax'}
#' }
#' for each element of the facetting variable (if any).
#' @importFrom plyr ddply rbind.fill
#' @importFrom stats na.omit setNames
getAxisLimPlot <- function(data, 
	xVar, yVar, 
	xLim = NULL, yLim = NULL, 
	facetPars = NULL,
	refLineData = NULL){
	
	facetVars <- getFacetVars(facetPars)
	
	scalePar <- facetPars$scales
	if(is.null(scalePar))	scalePar <- "fixed"
	
	if(!is.null(refLineData))
		refLineDataAll <- do.call(rbind.fill, refLineData)
	
	axisLimits <- ddply(data, facetVars, function(x){
		
		if(!is.null(refLineData)){
			refLineDataX <- if(length(facetVars) > 0){
				xToMerge <- unique(x[, facetVars, drop = FALSE])
				merge(xToMerge, refLineDataAll, by = facetVars, all.x = TRUE, all.y = FALSE)
			}else	refLineDataAll
			xRefLine <- na.omit(refLineDataX$xintercept)
			yRefLine <- na.omit(refLineDataX$yintercept)
		}
		xRange <- range(c(x[, xVar], xRefLine), na.rm = TRUE)
		xRange <- setNames(xRange, paste0("x", c("min", "max")))
		yRange <- range(c(x[, yVar], yRefLine), na.rm = TRUE)
		yRange <- setNames(yRange, paste0("y", c("min", "max")))
		c(xRange, yRange)
		
	})

	# free_y -> x-axis is fixed
	if(scalePar %in% c("fixed", "free_y")){
		axisLimits$xmin <- min(axisLimits$xmin)
		axisLimits$xmax <- max(axisLimits$xmax)
	}
	
	# free_x -> y-axis is fixed
	if(scalePar %in% c("fixed", "free_x")){
		axisLimits$ymin <- min(axisLimits$ymin)
		axisLimits$ymax <- max(axisLimits$ymax)
	}
	
	return(axisLimits)
	
}

