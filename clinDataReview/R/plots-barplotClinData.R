#' Barplot visualization of clinical data.
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
	xLab = getLabelVar(xVar, labelVars = labelVars), xLabVar = NULL,
	yLab = getLabelVar(yVar, labelVars = labelVars), yLabVar = NULL,
	# aesthetic
	colorVar = NULL, colorLab = getLabelVar(colorVar, labelVars = labelVars),
	colorPalette = NULL,
	barmode = "group",
	# general plot:
	title = paste(c(paste(yLab, "vs", xLab), titleExtra), collapse = "<br>"),
	titleExtra = NULL,
	caption = NULL, subtitle = NULL,
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
	# selection
	selectVars = NULL, selectLab = getLabelVar(selectVars, labelVars = labelVars),
	watermark = NULL,
	verbose = FALSE){

	# store input parameter values for further use
	plotArgs <- c(as.list(environment()))
	
	# drop unused factor levels as plotly default
	if(is.factor(data[, xVar]))
		data[, xVar] <- droplevels(data[, xVar])

	idVars <- c(xVar, colorVar)
	data$idEl <- interaction(data[, idVars, drop = FALSE])
	keyVar <- "idEl"
	
	# format data to: 'SharedData' object
	if(missing(hoverVars)){
		hoverVars <- c(xVar, xLabVar, colorVar, yVar, yLabVar, selectVars)
		hoverLab <- c(
		  getLabelVar(var = xVar, label = xLab, labelVars = labelVars),
		  getLabelVar(var = xLabVar, labelVars = labelVars),
		  getLabelVar(var = colorVar, label = colorLab, labelVars = labelVars),
		  getLabelVar(var = yVar, label = yLab, labelVars = labelVars),
		  getLabelVar(var = yLabVar, labelVars = labelVars),
		  getLabelVar(var = selectVars, label = selectLab, labelVars = labelVars)
		)
	}else	if(missing(hoverLab)){
		hoverLab <- getLabelVar(hoverVars, labelVars = labelVars)
	}
	hoverVars <- unique(hoverVars)
	
	dataSharedData <- formatDataForPlotClinData(
		data = data, 
		hoverVars = hoverVars, hoverLab = hoverLab,
		hoverByVar = keyVar,
		keyVar = keyVar, id = id,
		labelVars = labelVars
	)
	
	# include xLabVar and yLabVar in the axes
	xAxisLab <- getAxisLab(axisVar = xVar, axisLab = xLab, labVar = xLabVar, 
	  data = data, labelVars = labelVars)
	yAxisLab <- getAxisLab(axisVar = yVar, axisLab = yLab, labVar = yLabVar, 
	  data = data, labelVars = labelVars)
	
	# get plot dim
	dimPlot <- getSizePlot(
		width = width, height = height,
		includeLegend = !is.null(colorVar),
		legendPosition = "top",
		title = title,
		caption = caption,
		subtitle = subtitle,
		xLab = xAxisLab
	)
	width <- dimPlot[["width"]]
	height <- dimPlot[["height"]]
	
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
	xaxisArgs <- list(
	  tickangle = 45, 
	  # to have x-axis reset when a group from selectVars is selected
	  categoryorder = "trace"
	)
	
	# in case x-var is not nested within color variable
	# when elements are selected in the legend,
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
				  type = "array",
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
	
	pl <- layoutClinData(
		p = pl,
		xLab = xAxisLab,
		yLab = yAxisLab,
		title = title,
		caption = caption, 
		subtitle = subtitle,
		includeLegend = !is.null(colorVar),
		legendPosition = "top",
		legend = list(title = list(text = colorLab)),
		width = width,
		height = height,
		watermark = watermark,
		# extra params passed to plotly::layout
		xaxis = xaxisArgs,
		barmode = barmode
	)
		
	# specific formatting for clinical data
	res <- formatPlotlyClinData(
		data = data, pl = pl,
		idVar = keyVar, pathVar = pathVar,
		# extract ID from 'label' column directly the plot output object
		idFromDataPlot = FALSE, idVarPlot = "label",
		# patient prof filename based on the 'y' label
		labelVarPlot = "label",
		id = id, 
		verbose = verbose,
		# selection
		selectVars = selectVars, selectLab = selectLab, labelVars = labelVars,
		keyVar = keyVar
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
			keyVar = keyVar, idVar = xVar,
			pathVar = pathVar, pathLab = pathLab,
			pathExpand = TRUE,
			tableVars = tableVars,
			tableLab = tableLab,
			tableButton = tableButton, tablePars = tablePars,
			id = id, 
			labelVars = labelVars
		)
		res <- c(
		  if(inherits(res, "plotly")){list(plot = res)}else{res}, 
		  list(table = table)
		)
		
	}
	
	if(!inherits(res, "plotly"))
	  class(res) <- c("clinDataReview", class(res))
	
	return(res)
	
}