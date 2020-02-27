#' Convert static to interactive plot,
#' with possibility to download patient profiles
#' on a click event.
#' @return \code{\link[plotly]{ggplotly}} object.
#' @author Laure Cougnaud
#' @importFrom plotly highlight ggplotly
#' @importFrom htmlwidgets onRender
#' @export
ggplotlyMonitoring <- function(
	gg, data,
	width = NULL, height = NULL,
	idVar = "USUBJID", 
	pathVar = NULL,
	hoverVar = NULL){

	# convert to interactive plot
	pl <- ggplotly(
		p = gg, 
		width = width, height = height, 
		tooltip = if(!is.null(hoverVar))	"text"
	)
	# turn-off selection by double-clicking on the graph
	pl <- pl %>% highlight(on = "plotly_click", off = "plotly_doubleclick")
	
	# to check attributes available in the plotly object:
	#	plotly_json(pl)
	
	if(!is.null(pathVar)){
		
		dataPPDf <- unique(data[, c(idVar, pathVar)])
		if(any(duplicated(dataPPDf[, idVar])))
			stop("Duplicated ", idVar, " for specific ", pathVar, ".")
		dataPP <- dataPPDf[, c(idVar, pathVar)]
		colnames(dataPP) <- c("key", "path")
		pl <- pl %>% onRender(
			jsCode = JS("function(el, x, data){downloadPatientProfilesPlotly(el, x, data);}"),
			data = dataPP
		)
	
	}
	
	return(pl)
	
}