#' Visualize time intervals across subjects/parameters.
#' @param paramVar Character vector with variable of \code{data}
#' to represent in the y-axis.
#' @param paramLab (optional) String with label for \code{paramVar}.
#' @param paramVarSep (optional) String with separator used to combined \code{paramVar}
#' if multiple.
#' @param paramGroupVar (optional) Character vector with variable(s) to group/order
#' the \code{paramVar} elements in the y-axis.
#' @param timeStartVar String with variable with the start of the time interval.
#' @param timeStartLab (optional) String with label for \code{timeStartVar}.
#' @param timeEndVar String with variable with the end of the time interval.
#' @param timeEndLab (optional) String with label for \code{timeEndVar}.
#' @param timeStartShapeVar (optional) String with variable used for the shape
#' of the start of the time interval.
#' @param timeStartShapeLab (optional) String with label for \code{timeStartShapeVar}.
#' @param timeEndShapeVar (optional) String with variable used for the shape
#' of the end of the time interval.
#' @param timeEndShapeLab (optional) String with label for \code{timeEndShapeVar}.
#' @param shapePalette (optional) Character vector with shape palette for
#' \code{timeStartShapeVar} and \code{timeEndShapeVar}.
#' @param xLab,yLab (optional) String with labels for the x/y-axis.
#' @param alpha (optional) Numeric with transparency, 1 by default.
#' @inheritParams clinDataReview-common-args
#' @inheritParams tableClinData
#' @inherit scatterplotClinData return
#' @example inst/examples/timeProfileIntervalPlot-example.R
#' @author Laure Cougnaud
#' @family Clinical data visualization of individual profiles.
#' @import plotly
#' @importFrom clinUtils formatVarForPlotLabel getColorPalette getShapePalette
#' @importFrom crosstalk bscols filter_select
#' @importFrom plyr dlply
#' @export
timeProfileIntervalPlot <- function(data,
	paramVar, paramLab = getLabelVar(paramVar, labelVars = labelVars),
	paramVarSep = " - ",
	paramGroupVar = NULL,
	timeStartVar, timeStartLab = getLabelVar(timeStartVar, labelVars = labelVars),
	timeEndVar, timeEndLab = getLabelVar(timeEndVar, labelVars = labelVars),
	# shape
	timeStartShapeVar = NULL, timeStartShapeLab = getLabelVar(timeStartShapeVar, labelVars = labelVars),
	timeEndShapeVar = NULL, timeEndShapeLab = getLabelVar(timeEndShapeVar, labelVars = labelVars),
	shapePalette = NULL,
	# color
	colorVar = NULL, colorLab = getLabelVar(colorVar, labelVars = labelVars),
	colorPalette = NULL,
	# transparency
	alpha = 1,
	# labels
	yLab = NULL, #paste(paramLab, collapse = "\n"),
	xLab = paste(c(timeStartLab, timeEndLab), collapse = " and "),
	title = NULL,
	subtitle = NULL, caption = NULL,
	labelVars = NULL,
	# interactivity:
	width = 800, height = NULL,
	hoverVars, hoverLab,
	# path to subject-specific report
	idVar = "USUBJID", idLab = getLabelVar(idVar, labelVars = labelVars),
	pathVar = NULL, pathLab = getLabelVar(pathVar, labelVars = labelVars),
	id = paste0("plotClinData", sample.int(n = 1000, size = 1)),
	# selection
	selectVars = NULL, selectLab = getLabelVar(selectVars, labelVars = labelVars),
	# table
	table = FALSE, 
	tableVars, tableLab,
	tableButton = TRUE, tablePars = list(),
	verbose = FALSE){

	# store input parameter values for further use
	plotArgs <- c(as.list(environment()))

	## Format data
	
	# fill missing time:
#	timeStartVar <- paste0(timeStartVar, "Plot")
#	timeEndVarPlot <- paste0(timeEndVar, "Plot")
#	data <- formatTimeInterval(
#		data = data, 
#		timeStartVar = timeStartVar, timeEndVar = timeEndVar, 
#		timeStartVarNew = timeStartVar, timeEndVarNew = timeEndVarPlot,
#		subjectVar = subjectVar,
#		timeLim = timeLim, timeLimData = timeLimData, 
#		timeLimStartVar = timeLimStartVar, timeLimEndVar = timeLimEndVar
#	)
#	if(!all(data[, c(timeStartVar, timeEndVar)] == data[, c(timeStartVar, timeEndVarPlot)], na.rm = TRUE))
#		stop("Issue of extraction of missing time.")
	
	# concatenate variable(s) if multiple are specified
	if(length(paramVar) > 1){
		data$paramVar <- apply(data[, paramVar], 1, paste, collapse = paramVarSep)	
	}else{
		data$paramVar <- data[, paramVar]
	}
	
	# include the selection variable in the y-axis as well
	# to have range of the y axis reset to only contain selected parameters
	if(!is.null(selectVars)){
	  data$yVar <- apply(data[, c(selectVars, "paramVar")], 1, paste, collapse = paramVarSep)	
	}else{
	  data$yVar <- data[, "paramVar"]
	}
	
	
	# order y-variable based on selection, grouping and parameter variables
	data$yVar <- formatVarForPlotLabel(
		data = data, 
		paramVar = "yVar", paramGroupVar = unique(c(selectVars, paramGroupVar, paramVar)), 
		width = 20
	)
	
	# in order to specify layout/yaxis/range
	if(is.null(selectVars))
	  data$yVar <- as.numeric(data$yVar)
	
	# variables used to uniquely identify a record
	# between the table and the plot
	# issue when key variable is too long in highlight_key
	# so use row IDs
	data$key <- as.character(seq_len(nrow(data)))
	keyVar <- "key"
	
	# To have correct symbols matching in plotly
	# - shape variable must be factor
	# - if multiple shape vars, each var should contain all levels
	if(!is.null(timeStartShapeVar) | !is.null(timeEndShapeVar)){
		getLevels <- function(x)
			if(is.factor(x))	levels(x)	else	unique(x)
		shapeLevels <- c(
			if(!is.null(timeStartShapeVar))	getLevels(data[, timeStartShapeVar]), 
			if(!is.null(timeEndShapeVar))	getLevels(data[, timeEndShapeVar])
		)
		shapeLevels <- unique(shapeLevels)
		if(!is.null(timeStartShapeVar))
			data[, timeStartShapeVar] <- factor(data[, timeStartShapeVar], levels = shapeLevels)
		if(!is.null(timeEndShapeVar))
			data[, timeEndShapeVar] <- factor(data[, timeEndShapeVar], levels = shapeLevels)
	}
	
	# format data to: 'SharedData' object
	if(missing(hoverVars)){
		
		hoverVars <- c(paramVar, 
      timeStartVar, timeStartShapeVar, timeEndVar, timeEndShapeVar,
      colorVar, selectVars
		)
		hoverLab <- c(
			getLabelVar(var = paramVar, label = paramLab, labelVars = labelVars),
			getLabelVar(var = timeStartVar, label = timeStartLab, labelVars = labelVars),
			getLabelVar(var = timeEndVar, label = timeEndLab, labelVars = labelVars),
			getLabelVar(var = colorVar, label = colorLab, labelVars = labelVars),
			getLabelVar(var = timeStartShapeVar, label = timeStartShapeLab, labelVars = labelVars),
			getLabelVar(var = timeEndShapeVar, label = timeEndShapeLab, labelVars = labelVars),
			getLabelVar(var = selectVars, label = selectLab, labelVars = labelVars)
		)
		
	}else	if(missing(hoverLab)){
		hoverLab <- getLabelVar(hoverVars, labelVars = labelVars)
	}
	hoverVars <- unique(hoverVars);hoverLab <- hoverLab[hoverVars]
	
	## Convert to SharedData
	convertToSharedDataIntPlot <- function(data)
		formatDataForPlotClinData(
			data = data, 
			id = id,
			keyVar = keyVar,
			labelVars = labelVars,
			hoverVars = hoverVars, hoverLab = hoverLab,
			hoverByVar = c(selectVars, paramVar, timeStartVar, timeEndVar)
		)
	dataPlotSharedData <- convertToSharedDataIntPlot(data)
	
	if(is.null(colorPalette)){
		colorPaletteOpt <- getOption("clinDataReview.colors")
		if(!is.null(colorVar)){
			colorPalette <- getColorPalette(
				x = data[, colorVar],
				palette = colorPaletteOpt
			)
		}else	colorPalette <- getColorPalette(n = 1, palette = colorPaletteOpt)
	}

	# extract y-values and corresponding labels
	dimPlot <- getSizePlot(
		width = width, height = height,
		title = title, 
		subtitle = subtitle,
		caption = caption,
		xLab = xLab,
		includeLegend = !is.null(colorVar), 
		legendPosition = "bottom",
		# take the maximum plot height across selection groups
		y = dlply(data, selectVars, function(x) droplevels(as.factor(x[, "paramVar"])))
	)
	width <- dimPlot[["width"]]
	height <- dimPlot[["height"]]
	
	pl <- plot_ly(width = width, height = height)

	## markers for symbols
	# always display symbols to:
	# 1) have a hover (not possible in rectangle)
	# 2) have a symbol when start and end is the same
	if(!is.null(timeStartShapeVar) | !is.null(timeEndShapeVar)){
		if(is.null(shapePalette)){
			shapePalette <- getShapePalette(
				x = shapeLevels, 
				palette = getOption("clinDataReview.shapes")
			)
		}else	shapePalette <- shapePalette[shapeLevels]
	}else	shapePalette <- NULL

	addMarkers <- function(p, showlegend = FALSE, ...){
		add_markers(
			data = dataPlotSharedData, 
			p = p,
			y = varToFm("yVar"),
			color = if(!is.null(colorVar))	varToFm(colorVar)	else	I(colorPalette), 
			colors = if(!is.null(colorVar))	colorPalette,
			hovertemplate = varToFm("hover"),
			showlegend = showlegend,
			opacity = alpha,
			...
		)
	}
	
	pl <- addMarkers(
		p = pl,
		x = varToFm(timeStartVar), 
		symbol = if(!is.null(timeStartShapeVar))	varToFm(timeStartShapeVar),
		symbols = if(!is.null(shapePalette))	I(shapePalette)
	)

	pl <- addMarkers(
		p = pl,
		x = varToFm(timeEndVar), 
		symbol = if(!is.null(timeEndShapeVar))	varToFm(timeEndShapeVar),
		symbols = if(!is.null(shapePalette))	I(shapePalette)
	)
	
	# create legend only with color
	# because plotly doesn't support multiple legend
	if(!is.null(colorVar))
		pl <- addMarkers(
			p = pl, x = varToFm(timeStartVar), 
			showlegend = TRUE, visible = "legendonly"
		)
	
	## segments for time-interval
	# plotly returns an error when no non-missing values in start/end time vars
	# and 'crossed' segments if start value == end value
	idxSegments <- which(
		rowSums(is.na(data[, c(timeStartVar, timeEndVar)])) != length(c(timeStartVar, timeEndVar)) &
		data[, timeStartVar] != data[, timeEndVar]
	)
	if(length(idxSegments) > 0){
		dataPlotSharedDataSeg <- convertToSharedDataIntPlot(data = data[idxSegments, , drop = FALSE])
		pl <- pl %>% add_segments(
			data = dataPlotSharedDataSeg,
			x = varToFm(timeStartVar), 
			xend = varToFm(timeEndVar),
			y = varToFm("yVar"),
			yend = varToFm("yVar"),
			color = if(!is.null(colorVar))	varToFm(colorVar)	else	I(colorPalette), 
			colors = if(!is.null(colorVar))	colorPalette,
			showlegend = FALSE,
			opacity = alpha
		)
	}
	
	# set layout
	yData <- unique(data[order(data[, "yVar"]), c("yVar", "paramVar")])
	pl <- layoutClinData(
		p = pl,
		title = title,
		xLab = xLab,
		yLab = yLab,
		caption = caption,
		subtitle = subtitle,
		includeLegend = !is.null(colorVar),
		legendPosition = "bottom",
		width = width,
		height = height,
		# extra params passed to plotly::layout
		legend = list(title = list(text = colorLab)),
		yaxis = c(
		  list(
  			showgrid = TRUE,
  			title = list(text = yLab),
  			type = "array",
  			tickvals = yData[, "yVar"], 
  			ticktext = yData[, "paramVar"],
  			tickangle = 0
  		),
		  # reduce padding axis <-> plot & plot <-> title
		  if(is.null(selectVars))
		    list(range = c(0.5, nrow(yData)+0.5))
		),
		hovermode = "closest"
	)
	
	# specific formatting for clinical data
  res <- formatPlotlyClinData(
    data = data, pl = pl,
		# extract patient profile based on the 'key' variable
		# (not yVar because different records could be grouped in the same yVar)
		# (should be included in the plot)
		idVar = keyVar, pathVar = pathVar,
		# extract ID from 'y' column in the plot output object directly
		idVarPlot = "key", idFromDataPlot = TRUE, 
		# no label for patient prof filename because y is a numeric (not informative)
#		labelVarPlot = "y",
    id = id, 
    # selection
    selectVars = selectVars, selectLab = selectLab, 
    labelVars = labelVars,
    keyVar = keyVar, 
		verbose = verbose,
		pathDownload = FALSE # open in new tab
	)
	
	# create associated table
	if(table){
		
		tableVars <- getPlotTableVars(
			plotFunction = "timeProfileIntervalPlot", 
			plotArgs = plotArgs
		)
		tableLab <- attr(tableVars, "tableLab")
		
		table <- tableClinData(
			data = data, 
			keyVar = keyVar, idVar = idVar, 
			pathVar = pathVar, pathLab = pathLab,
			tableVars = tableVars,
			tableLab = tableLab,
			tableButton = tableButton, tablePars = tablePars,
			id = id, 
			labelVars = labelVars,
			verbose = verbose
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
