#' Format data for interactive plot for medical monitoring.
#' @inheritParams medicalMonitoring-common-args
#' @author Laure Cougnaud
#' @importFrom plyr ddply
#' @importFrom plotly highlight_key
#' @export
formatDataForPlotMonitoring <- function(
	data, xVar, yVar, 
	facetPars = NULL, 
	hoverVar = unique(c(xVar, yVar)),
	hoverLab = getLabelVar(hoverVar, labelVars = labelVars),
	idVar = "USUBJID",
	id = paste0("plotMonitoring", sample.int(n = 1000, size = 1))){

	# extract variables that defines uniquely one point in the plot:
	idVars <- c(xVar, yVar)
	if(!is.null(facetPars)){
		facetVars <- getFacetVars(facetPars)
		idVars <- unique(c(idVars, facetVars))
	}
	
	# create hover variable: combine hover if points have the same x/y coordinates
	# by default in plotly: hover var only displayed for one of the overlapping point
	if(!is.null(hoverVar)){
		data <- ddply(data, idVars, function(dataPoint){
			hoverTextList <- lapply(hoverVar, function(var){
				formatHoverText(
					x = sort(unique(dataPoint[, var])),
					label = hoverLab[var]
				)
			})
			hoverText <- Reduce(function(...) paste(..., sep = "<br>"), hoverTextList)
			cbind.data.frame(dataPoint, hover = unname(hoverText), stringsAsFactors = FALSE)
		})
	}
	
	# SharedData object:
	keyFm <- as.formula(paste("~", idVar))
	group <- paste0("SharedData:", id)
	dataSharedData <- highlight_key(data = data, key = keyFm, group = group)
	
	return(dataSharedData)
	
}
