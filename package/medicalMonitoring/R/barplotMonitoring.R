#' Barplot interactive plot.
#' @param colorVar (optional) String with color variable.
#' @param colorLab String with label for \code{colorVar}.
#' @inheritParams medicalMonitoring-common-args-summaryStatsVis
#' @inheritParams medicalMonitoring-common-args
#' @inheritParams tableMonitoring
#' @inherit scatterplotMonitoring return
#' @example inst/examples/barplotMonitoring-example.R
#' @family visualizations of summary statistics for medical monitoring
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
	colorVar = NULL, colorLab = getLabelVar(colorVar, labelVars = labelVars),
	# general plot:
	titleExtra = NULL,
	title = paste(paste(yLab, "vs", xLab, titleExtra), collapse = "<br>"),
	labelVars = NULL,
	# interactivity:
	width = NULL, height = NULL,
	hoverVars, hoverLab,
	pathVar = NULL, pathLab = getLabelVar(pathVar, labelVars = labelVars),
	table = FALSE, 
	tableVars, tableLab,
	tableButton = TRUE, tablePars = list(),
	id = paste0("plotMonitoring", sample.int(n = 1000, size = 1)),
	verbose = FALSE){
	
	idVar <- xVar
	
	# format data to: 'SharedData' object
	if(missing(hoverVars)){
		hoverVars <- c(xVar, yVar, colorVar)
		hoverLab <- setNames(c(xLab, yLab, colorLab), hoverVars)
	}else	if(missing(hoverLab)){
		hoverLab <- getLabelVar(hoverVars, labelVars = labelVars)
	}
	hoverVars <- unique(hoverVars)
	dataSharedData <- formatDataForPlotMonitoring(
		data = data, 
		hoverVars = hoverVars, hoverLab = hoverLab,
		hoverByVar = idVar,
		keyVar = idVar, id = id,
		labelVars = labelVars
	)
	
	# get plot dim
	dimPlot <- getSizePlotMonitoring(
		width = width, height = height,
		legend = !is.null(colorVar)
	)
	width <- unname(dimPlot["width"])
	height <- unname(dimPlot["height"])
	
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
		xaxis = list(title = xLab, tickangle = 45),
		yaxis = list(title = yLab)
	)
		
	# specific formatting for medical monitoring
	pl <- formatPlotlyMonitoring(
		data = data, pl = pl,
		idVar = idVar, pathVar = pathVar,
		# extract ID from 'label' column directly the plot output object
		idFromDataPlot = FALSE, idVarPlot = "label",
		# patient prof filename based on the 'y' label
		labelVarPlot = "label",
		id = id, 
		verbose = verbose
	)
	
	# create associated table
	if(table){
		
		if(missing(tableVars)){
			tableVars <- c(xVar, yVar, colorVar)
			tableLab <- setNames(c(xLab, yLab, colorLab), tableVars)
		}else	if(missing(tableLab)){
			tableLab <- getLabelVar(tableVars, labelVars = labelVars)
		}
		tableVars <- unique(tableVars)
		
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