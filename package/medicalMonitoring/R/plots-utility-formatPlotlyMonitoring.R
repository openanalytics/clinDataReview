#' Format interactive plot,
#' with possibility to download patient profiles
#' on a click event.
#' @param idVar String with variable of \code{data}
#' containing plot element.
#' @param idVarPlot String with variable in the \code{\link[plotly]{plotly}}
#' output containing IDs.
#' @param idFromDataPlot Logical, if TRUE (by default) \code{idVarPlot}
#' is extracted from the data of the plot object, otherwise
#' directly from the plot object.
#' @param pl \code{\link[plotly]{ggplotly}} object.
#' @param highlightOn String with event to turn on the selection
#' (\code{on} parameter of \code{\link[plotly]{highlight}}),
#' 'plotly_click' by default.
#' @param highlightOff String with event to turn off the selection
#' (\code{off} parameter of \code{\link[plotly]{highlight}}),
#' 'plotly_doubleclick' by default.
#' @param pathVar String with variable of \code{data} containing path
#' to a subject-specific report (e.g. patient profiles).
#' @param verbose Logical, if TRUE report progress messages
#' during execution (included in the browser 'Console').
#' @inheritParams formatDataForPlotMonitoring
#' @inheritParams medicalMonitoring-common-args
#' @return Updated \code{\link[plotly]{plotly}} object.
#' @author Laure Cougnaud
#' @importFrom plotly highlight
#' @importFrom htmlwidgets onRender JS
#' @export
formatPlotlyMonitoring <- function(
	pl, data,
	idVar = "USUBJID", pathVar = NULL,
	idFromDataPlot = TRUE, idVarPlot = "key",
	highlightOn = "plotly_click",
	highlightOff = "plotly_doubleclick",
	id = paste0("plotMonitoring", sample.int(n = 1000, size = 1)),
	verbose = FALSE){

	# turn-off selection by double-clicking on the graph
	pl <- pl %>% highlight(on = highlightOn, off = highlightOff)
	
	# to check attributes available in the plotly object:
	#	plotly_json(pl)
	
	if(!is.null(pathVar)){
		
		dataPPDf <- unique(data[, c(idVar, pathVar)])
		if(any(duplicated(dataPPDf[, idVar])))
			stop("Duplicated ", idVar, " for specific ", pathVar, ".")
		dataPP <- dataPPDf[, c(idVar, pathVar)]
		colnames(dataPP) <- c("key", "path")
		pl <- pl %>% onRender(
			jsCode = JS("function(el, x, data){",
				paste0("downloadPatientProfilesPlotly(el, x, data,",
					"fromdata=", tolower(idFromDataPlot), ",",
					"idvar=", sQuote(idVarPlot), ",",
					"label=", sQuote(id), ",",
					"verbose=", tolower(verbose),
					");"
				),
				"}"),
			data = dataPP
		)
	
	}
	
	return(pl)
	
}