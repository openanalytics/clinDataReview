#' Barplot interactive plot.
#' @param colorVar (optional) String with color variable.
#' @param colorLab String with label for \code{colorVar}.
#' @param barmode String with type of barplot, either:
#' 'group' or 'stack' (see parameter in \code{\link[plotly]{layout}}).
#' @param colorPalette (optional) Named character vector with color palette.
#' If not specified, the GLPG color palette is used.
#' @inheritParams medicalMonitoring-common-args-summaryStatsVis
#' @inheritParams medicalMonitoring-common-args
#' @inheritParams tableMonitoring
#' @inherit scatterplotMonitoring return
#' @example inst/examples/barplotMonitoring-example.R
#' @family visualizations of summary statistics for medical monitoring
#' @import plotly
#' @importFrom stats as.formula
#' @importFrom glpgStyle getGLPGColorPalette
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
	colorPalette = NULL,
	barmode = "group",
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

	idVars <- c(xVar, colorVar)
	data$idEl <- interaction(data[, idVars, drop = FALSE])
	
	# format data to: 'SharedData' object
	if(missing(hoverVars)){
		hoverVars <- c(xVar, colorVar, yVar)
		hoverLab <- setNames(c(xLab, colorLab, yLab), hoverVars)
	}else	if(missing(hoverLab)){
		hoverLab <- getLabelVar(hoverVars, labelVars = labelVars)
	}
	hoverVars <- unique(hoverVars)
	dataSharedData <- formatDataForPlotMonitoring(
		data = data, 
		hoverVars = hoverVars, hoverLab = hoverLab,
		hoverByVar = "idEl",
		keyVar = "idEl", id = id,
		labelVars = labelVars
	)
	
	# get plot dim
	dimPlot <- getSizePlotMonitoring(
		width = width, height = height,
		legend = !is.null(colorVar)
	)
	width <- unname(dimPlot["width"])
	height <- unname(dimPlot["height"])
	
	if(is.null(colorPalette)){
		if(!is.null(colorVar)){
			colorPalette <- getGLPGColorPalette(x = data[, colorVar])
		}else	colorPalette <- getGLPGColorPalette(n = 1)
	}
	
	# use plotly rather than ggplot -> ggplotly implementation
	# because 'label' used to extract path report is numeric
	# rather than character vector with element when converted to ggplotly
	# so makes mapping selected bar <-> path report more tricky
	pl <- plot_ly(
		data = dataSharedData, 
		x = varToFm(xVar), y = varToFm(yVar), 
		color = if(!is.null(colorVar))	varToFm(colorVar)	else	I(colorPalette), 
		colors = if(!is.null(colorVar))	colorPalette,
		type = "bar",
		hovertemplate = varToFm("hover"),
		width = width, height = height
	)
	pl <- pl %>% layout(
		title = title,
		xaxis = list(title = xLab, tickangle = 45),
		yaxis = list(title = yLab),
		barmode = barmode
	)
		
	# specific formatting for medical monitoring
	pl <- formatPlotlyMonitoring(
		data = data, pl = pl,
		idVar = "idEl", pathVar = pathVar,
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
			tableVars <- c(xVar, colorVar, yVar)
			tableLab <- setNames(c(xLab, colorLab, yLab), tableVars)
		}else	if(missing(tableLab)){
			tableLab <- getLabelVar(tableVars, labelVars = labelVars)
		}
		tableVars <- unique(tableVars)
		
		table <- tableMonitoring(
			data = data, 
			keyVar = "idEl", idVar = xVar,
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