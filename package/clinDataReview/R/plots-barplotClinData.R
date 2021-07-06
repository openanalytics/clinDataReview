#' Barplot interactive plot.
#' @param colorVar (optional) String with color variable.
#' @param colorLab String with label for \code{colorVar}.
#' @param barmode String with type of barplot, either:
#' 'group' or 'stack' (see parameter in \code{\link[plotly]{layout}}).
#' @param textVar (optional) String with a text variable,
#' that will be displayed outside of each bar.
#' @inheritParams clinDataReview-common-args-summaryStatsVis
#' @inheritParams clinDataReview-common-args
#' @inheritParams tableClinData
#' @inherit scatterplotClinData return
#' @example inst/examples/barplotClinData-example.R
#' @family visualizations of summary statistics for clinical data
#' @import plotly
#' @importFrom stats as.formula
#' @importFrom clinUtils getColorPalette
#' @author Laure Cougnaud
#' @export
barplotClinData <- function(
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
	textVar = NULL, 
	pathVar = NULL, pathLab = getLabelVar(pathVar, labelVars = labelVars),
	table = FALSE, 
	tableVars, tableLab,
	tableButton = TRUE, tablePars = list(),
	id = paste0("plotClinData", sample.int(n = 1000, size = 1)),
	verbose = FALSE){

	# store input parameter values for further use
	plotArgs <- c(as.list(environment()))

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
	
	dataSharedData <- formatDataForPlotClinData(
		data = data, 
		hoverVars = hoverVars, hoverLab = hoverLab,
		hoverByVar = "idEl",
		keyVar = "idEl", id = id,
		labelVars = labelVars
	)
	
	# get plot dim
	dimPlot <- getSizePlotClinData(
		width = width, height = height,
		legend = !is.null(colorVar),
		legendPosition = "bottom"
	)
	width <- unname(dimPlot["width"])
	height <- unname(dimPlot["height"])
	
	if(is.null(colorPalette)){
		colorPaletteOpt <- getOption("clinDataReview.colors")
		if(!is.null(colorVar)){
			colorPalette <- getColorPalette(
				x = data[, colorVar], 
				palette = colorPaletteOpt
			)
		}else	colorPalette <- getColorPalette(n = 1, palette = colorPaletteOpt)
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
		width = width, height = height,
		# include text/label if specified
		text = if(!is.null(textVar))	varToFm(textVar), 
		textposition = ifelse(barmode == "group", "outside", 'auto'),
		textfont = if(barmode != "group") list(color = '#ffffff')
	)
	
	## layout option
	
	xaxisArgs <- list(title = xLab, tickangle = 45)
	
	# in case x-var is not nested within color variable
	# when elements are selected in the legend legend selection,
	# corresponding elements are not filtered in the x-axis (bar is only removed)
	# this is a fix:
	if(!is.null(colorVar) && !is.numeric(data[, xVar]) && barmode == "stack"){
		
		nColorsByX <- tapply(data[, colorVar], data[, xVar], function(x) length(unique(x)))
		
		if(any(nColorsByX > 1, na.rm = TRUE)){
		
			xEl <- if(is.factor(data[, xVar])){
				levels(data[, xVar])
			}else	sort(unique(data[, xVar]))
			xaxisArgs <- c(xaxisArgs, 
				list(
					# text displayed at the ticks position
					ticktext = xEl,
					# values at which the ticks on the axis appear
					tickvals = xEl
				)
			)
			warning(paste(
				"X-variable is not nested within the color variable.\n",
				"In order to have proper filtering of the x-axis based on legend selection,",
				"the ordering of the x-variable might be based on the color (not the x) variable."
			))
			
		}
	}
	pl <- pl %>% layout(
		title = title,
		xaxis = xaxisArgs,
		yaxis = list(title = yLab), 
		barmode = barmode
	)
	
	pl <- pl %>% layout(legend = 
		list(
			orientation = "h",
			x = 0.5, xanchor = "center",
			y = 1
		)
	)
		
	# specific formatting for clinical data
	pl <- formatPlotlyClinData(
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
		
		tableVars <- getPlotTableVars(
			plotFunction = "barplotClinData", 
			plotArgs = plotArgs
		)
		tableLab <- attr(tableVars, "tableLab")
		
		table <- tableClinData(
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
		
		class(res) <- c("clinDataReview", class(res))
		
	}else res <- pl
	
	return(res)
	
}