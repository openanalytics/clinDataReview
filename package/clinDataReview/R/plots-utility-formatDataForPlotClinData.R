#' Format data for interactive plot for clinical data
#' @param hoverByVar Character vector with variables identifying
#' unique elements in the plot, usually x, y, facet variables.
#' These variables are used to identify records with the same position
#' in the plot, their information are combined in the hover.
#' @param keyVar String with unique key variable, identifying unique
#' group for which the link between the table and the plot should be done.
#' @inheritParams clinDataReview-common-args
#' @return Updated \code{\link[crosstalk]{SharedData}} with:
#' \itemize{
#' \item{extra column: 'hover' with combined info from \code{hoverVars}}
#' }
#' @author Laure Cougnaud
#' @importFrom plyr ddply
#' @importFrom crosstalk SharedData
#' @export
formatDataForPlotClinData <- function(
	data, 
	hoverVars = NULL,
	hoverLab = getLabelVar(hoverVars, labelVars = labelVars),
	hoverByVar = NULL,
	keyVar = NULL,
	id = paste0("plotClinData", sample.int(n = 1000, size = 1)),
	labelVars = NULL){

	if(length(keyVar) > 1)
		stop("'keyVar' should be of length 1.")

	# create hover variable: combine hover if points have the same x/y coordinates
	# by default in plotly: hover var only displayed for one of the overlapping point
	if(!is.null(hoverVars)){
		data <- ddply(data, hoverByVar, function(dataPoint){
			hoverTextList <- lapply(hoverVars, function(var){
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
	dataSharedData <- crosstalk::SharedData$new(
		data = data, 
		key = if(!is.null(keyVar))	varToFm(keyVar), 
		group = id
	)
	
	return(dataSharedData)
	
}
