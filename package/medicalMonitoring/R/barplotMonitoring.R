#' Barplot interactive plot.
#' 
#' Note: the table and plot are not (yet) linked.
#' @param colorVar (optional) String with color variable.
#' @inheritParams medicalMonitoring-common-args
#' @inheritParams tableMonitoring
#' @inherit scatterplotMonitoring return
#' @example inst/examples/barplotMonitoring-example.R
#' @import plotly
#' @importFrom stats as.formula
#' @author Laure Cougnaud
#' @export
barplotMonitoring <- function(
	data, 
	# x/y variables:
	xVar, yVar, 
	xLab = getLabelVar(xVar, labelVars = labelVars),
	yLab = getLabelVar(yVar, labelVars = labelVars), 
	# aesthetic
	colorVar = NULL,
	# general plot:
	titleExtra = NULL,
	title = paste(paste(yLab, "vs", xLab, titleExtra), collapse = "<br>"),
	labelVars = NULL,
	# interactivity:
	width = NULL, height = NULL,
	hoverVar = unique(c(xVar, yVar, colorVar)), 
	hoverLab = getLabelVar(hoverVar, labelVars = labelVars),
	pathVar = NULL, pathLab = getLabelVar(pathVar, labelVars = labelVars),
	table = FALSE, 
	tableVars, tableLab,
	tableButton = TRUE, tablePars = list(),
	id = paste0("plotMonitoring", sample.int(n = 1000, size = 1)),
	verbose = FALSE){
	
	idVar <- xVar
	
	# format data to: 'SharedData' object
	dataSharedData <- formatDataForPlotMonitoring(
		data = data, 
		hoverVar = hoverVar, hoverLab = hoverLab,
		hoverByVar = idVar,
		keyVar = idVar, id = id,
		labelVars = labelVars
	)
	
	# use plotly rather than ggplot -> ggplotly implementation
	# because 'label' used to extract path report is numeric
	# rather than character vector with element when converted to ggplotly
	# so makes mapping selected bar <-> path report more tricky
	pl <- plot_ly(
		data = dataSharedData, 
		x = varToFm(xVar), y = varToFm(yVar), 
		color = if(!is.null(colorVar))	varToFm(colorVar),
		type = "bar",
		hovertemplate = varToFm("hover"),
		width = width, height = height
	)
	pl <- pl %>% layout(
		title = title,
		xaxis = list(title = xLab),
		yaxis = list(title = yLab)
	)
		
	# specific formatting for medical monitoring
	if(missing(hoverVar)){
		hoverVar <- c(xVar, yVar)
		hoverLab <- setNames(c(xLab, yLab), hoverVar)
	}else	if(missing(hoverLab)){
		hoverLab <- getLabelVar(hoverVar, labelVars = labelVars)
	}
	pl <- formatPlotlyMonitoring(
		data = dataPlot, pl = pl,
		idVar = idVar, pathVar = pathVar,
		idFromDataPlot = FALSE, idVarPlot = "label",
		id = id, 
		verbose = verbose,
		labelVarPlot = "label"
	)
	
	# create associated table
	if(table){
		
		if(missing(tableVars)){
			tableVars <- c(xVar, yVar)
			tableLab <- setNames(c(xLab, yLab), tableVars)
		}else	if(missing(tableLab)){
			tableLab <- getLabelVar(tableVars, labelVars = labelVars)
		}
		
		table <- tableMonitoring(
			data = data, 
			idVar = idVar, 
			pathVar = pathVar, pathLab = pathLab,
			pathExpand = TRUE,
			tableVars = tableVars,
			tableLab = tableLab,
			tableButton = tableButton, tablePars = tablePars,
			id = id, 
			labelVars = labelVars
		)
		res <- list(plot = pl, table = table)
		
		class(res) <- c("medicalMonitoring", class(res))
		
	}else res <- pl
	
	return(res)
	
}