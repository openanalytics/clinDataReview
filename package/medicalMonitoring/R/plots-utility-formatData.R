#' Format data for interactive plot for medical monitoring.
#' @param hoverByVar Character vector with variables identifying
#' unique elements in the plot, usually x, y, facet variables.
#' These variables are used to identify records with the same position
#' in the plot, their information are combined in the hover.
#' @param keyVar Character vector with key variables, identifying unique
#' group for which the link between the table and the plot should be done.
#' @inheritParams medicalMonitoring-common-args
#' @author Laure Cougnaud
#' @importFrom plyr ddply
#' @importFrom plotly highlight_key
#' @export
formatDataForPlotMonitoring <- function(
	data, 
	hoverVar = NULL,
	hoverLab = getLabelVar(hoverVar, labelVars = labelVars),
	hoverByVar = NULL,
	keyVar = NULL,
	id = paste0("plotMonitoring", sample.int(n = 1000, size = 1)),
	labelVars = NULL){

	# create hover variable: combine hover if points have the same x/y coordinates
	# by default in plotly: hover var only displayed for one of the overlapping point
	if(!is.null(hoverVar)){
		data <- ddply(data, hoverByVar, function(dataPoint){
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
	if(!is.null(keyVar) && length(keyVar) > 1){
		data$key <- do.call(interaction, data[, keyVar])
		keyVar <- "key"
	}
	argsHighlightKey <- list(data = data, group = id)
	if(!is.null(keyVar)){
		keyFm <- as.formula(paste("~", keyVar))
		argsHighlightKey <- c(argsHighlightKey, list(key = keyFm))
	}
	
	dataSharedData <- do.call(highlight_key, argsHighlightKey)
	
	return(dataSharedData)
	
}
