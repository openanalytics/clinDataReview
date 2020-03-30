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
#' @param pathDownload Logical, if TRUE (by default) the subject-specific report(s)
#' are downloaded in a zip compressed file.
#' If FALSE (only available if unique report per \code{idVarPlot}),
#' each report is opened in a new window.
#' @param verbose Logical, if TRUE report progress messages
#' during execution (included in the browser 'Console').
#' @param labelVarPlot String with plotly variable used to
#' extract label to build the file name of the zip compressed
#' file containing patient report.
#' @inheritParams formatDataForPlotMonitoring
#' @inheritParams medicalMonitoring-common-args
#' @return Updated \code{\link[plotly]{plotly}} object.
#' @author Laure Cougnaud
#' @importFrom plotly highlight
#' @importFrom htmlwidgets onRender JS
#' @export
formatPlotlyMonitoring <- function(
	pl, data,
	idVar = "USUBJID", 
	pathVar = NULL, pathDownload = TRUE,
	idFromDataPlot = TRUE, 
	idVarPlot = "key", labelVarPlot = NULL,
	highlightOn = "plotly_click",
	highlightOff = "plotly_doubleclick",
	id = paste0("plotMonitoring", sample.int(n = 1000, size = 1)),
	verbose = FALSE){

	# change plotly default on click event in the legend
	# single click: selected item is visible
	# double click: select item is hidden
	# plotly default is to display all excepted item when clicked on a legend
	pl <- pl %>% layout(
		legend = 
			list(
				itemclick = "toggleothers",
				itemdoubleclick = "toggle"
			)
	)

	# turn-off selection by double-clicking on the graph
	pl <- pl %>% highlight(on = highlightOn, off = highlightOff)
	
	# to check attributes available in the plotly object:
	#	plotly_json(pl)
	
	if(!is.null(pathVar)){
		
		if(!is.null(idVar) && length(idVar) > 1){
			data$key <- do.call(interaction, data[, idVar])
			idVar <- "key"
		}
		
		dataPPDf <- unique(data[, c(idVar, pathVar)])
		
		# in case 'pathVar' is formatted as URL, only extract the path
		dataPPDf[, pathVar] <- getPathHyperlink(dataPPDf[, pathVar])
		
		if(any(duplicated(dataPPDf[, idVar])))
			stop("Duplicated ", idVar, " for specific ", pathVar, ".")
		dataPP <- dataPPDf[, c(idVar, pathVar)]
		colnames(dataPP) <- c("key", "path")
		
		# Important: parameters should be in the same order
		# than specified in function definition 
		# (fct call by parameter doesn't exists natively in Javascript)
		jsPatientProfiles <- JS("function(el, x, data){",
			paste0("getPatientProfilesPlotly(el, x, data,",
				"fromdata=", tolower(idFromDataPlot), ",",
				"idvar=", sQuote(idVarPlot), ",",
				"labelplot=", sQuote(id), ",",
				"labelvar=", ifelse(is.null(labelVarPlot), 'null', sQuote(labelVarPlot)), ",",
				"download=", tolower(pathDownload), ",",
				"verbose=", tolower(verbose), 
				");"
			),
		"}") 
		
		pl <- pl %>% onRender(jsCode = jsPatientProfiles, data = dataPP)
	
	}
	
	return(pl)
	
}