#' Visualize time intervals across subjects/parameters.
#' @param paramVar Character vector with variable of \code{data}
#' to represent in the y-axis.
#' @param paramVarSep (optional) String with separator used to combined \code{paramVar}
#' if multiple.
#' @param paramGroupVar (optional) Character vector with variable(s) to group/order
#' the \code{paramVar} elements in the y-axis.
#' @param timeStartVar String with variable with the start of the time interval.
#' @param timeEndVar String with variable with the end of the time interval.
#' @param timeStartShapeVar (optional) String with variable used for the shape
#' of the start of the time interval.
#' @param timeEndShapeLab (optional) String with label for \code{timeStartShapeVar}.
#' @param timeEndShapeVar (optional) String with variable used for the shape
#' of the end of the time interval.
#' @param timeEndShapeLab (optional) String with label for \code{timeEndShapeVar}.
#' @param shapePalette (optional) Character vector with shape palette for
#' \code{timeStartShapeVar} and \code{timeEndShapeVar}.
#' @param xLab,yLab (optional) String with labels for the x/y-axis.
#' @param colorVar (optional) String with color variable.
#' @param colorLab (optional) String with label for \code{colorVar}
#' @param alpha (optional) Numeric with transparency, 1 by default.
#' @inheritParams medicalMonitoring-common-args
#' @inheritParams tableMonitoring
#' @inherit scatterplotMonitoring return
#' @author Laure Cougnaud
#' @import plotly
#' @importFrom glpgUtilityFct getGLPGColorPalette getGLPGShapePalette
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
	colorVar = NULL, colorLab = getLabelVar(timeStartShapeVar, labelVars = labelVars),
	colorPalette = NULL,
	# transparency
	alpha = 1,
	# labels
	yLab = paste(paramLab, collapse = "\n"),
	xLab = paste(c(timeStartLab, timeEndLab), collapse = " and "),
	title = NULL,
	labelVars = NULL,
	# interactivity:
	width = 800, height = NULL,
	hoverVars, hoverLab,
	# path to subject-specific report
	idVar = "USUBJID", idLab = getLabelVar(idVar, labelVars = labelVars),
	pathVar = NULL, pathLab = getLabelVar(pathVar, labelVars = labelVars),
	# table
	table = FALSE, 
	tableVars, tableLab,
	tableButton = TRUE, tablePars = list(),
	id = paste0("plotMonitoring", sample.int(n = 1000, size = 1)),
	verbose = FALSE){

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
		data$yVar <- apply(data[, paramVar], 1, paste, collapse = paramVarSep)	
	}else{
		data$yVar <- data[, paramVar]
	}
	
	if(!is.null(paramGroupVar)){
		idxOrder <- order(data[, paramGroupVar, drop = FALSE])
		yValueOrdered <- unique(as.character(data[idxOrder, 'yVar']))
		data$yVar <- factor(data$yVar, levels = yValueOrdered)
	}
	
	# variables used to uniquely identify a record
	# between the table and the plot
#	keyVars <- c(paramVar, timeStartVar, timeEndVarPlot)
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
		
		hoverVars <- c(paramVar, timeStartVar, timeStartShapeVar, timeEndVar, timeEndShapeVar)
		hoverLab <- c(
			getLabelVar(var = paramVar, label = paramLab, labelVars = labelVars),
			getLabelVar(var = timeStartVar, label = timeStartLab, labelVars = labelVars),
			getLabelVar(var = timeEndVar, label = timeEndLab, labelVars = labelVars),
			getLabelVar(var = timeStartShapeVar, label = timeStartShapeLab, labelVars = labelVars),
			getLabelVar(var = timeEndShapeVar, label = timeEndShapeLab, labelVars = labelVars)
		)
		
	}else	if(missing(hoverLab)){
		hoverLab <- getLabelVar(hoverVars, labelVars = labelVars)
	}
	hoverVars <- unique(hoverVars);hoverLab <- hoverLab[hoverVars]
	
	## Convert to SharedData
	dataPlotSharedData <- formatDataForPlotMonitoring(
		data = data, 
		id = id,
		keyVar = keyVar,
		labelVars = labelVars,
		hoverVars = hoverVars, hoverLab = hoverLab,
		hoverByVar = c(paramVar, timeStartVar, timeEndVar)
	)
	
	if(is.null(colorPalette)){
		if(!is.null(colorVar)){
			colorPalette <- getGLPGColorPalette(x = data[, colorVar])
		}else	colorPalette <- getGLPGColorPalette(n = 1)
	}

	# get plot dim
	if(is.null(height)){
		height <- 
			length(unique(data$yVar)) * 10 + 
			ifelse(!is.null(colorVar), 40, 0)
	}
	
	pl <- plot_ly(
		data = dataPlotSharedData, 
		width = width, height = height
	)

	# markers for symbols
	isShape <- !is.null(timeStartShapeVar) | !is.null(timeEndShapeVar)
	if(isShape){

		if(is.null(shapePalette)){
			shapePalette <- getGLPGShapePalette(x = shapeLevels)
		}else	shapePalette <- shapePalette[shapeLevels]
	
		addMarkers <- function(p, ...){
			add_markers(
				p = p,
				y = varToFm("yVar"),
				color = if(!is.null(colorVar))	varToFm(colorVar)	else	I(colorPalette), 
				colors = if(!is.null(colorVar))	colorPalette,
				showlegend = FALSE,
				hovertemplate = varToFm("hover"),
				opacity = alpha,
				...
			)
		}
		
		if(!is.null(timeStartShapeVar)){
			pl <- addMarkers(
				p = pl,
				x = varToFm(timeStartVar), 
				symbol = varToFm(timeStartShapeVar),
				symbols = I(shapePalette)
			)
		}
	
		if(!is.null(timeEndShapeVar)){
			pl <- addMarkers(
				p = pl,
				x = varToFm(timeEndVar), 
				symbol = varToFm(timeEndShapeVar),
				symbols = I(shapePalette)
			)
		}
	
	}
	
	# segments for time-interval
	pl <- pl %>% add_segments(
		x = varToFm(timeStartVar), 
		xend = varToFm(timeEndVar),
		y = varToFm("yVar"),
		yend = varToFm("yVar"),
		color = if(!is.null(colorVar))	varToFm(colorVar)	else	I(colorPalette), 
		colors = if(!is.null(colorVar))	colorPalette,
		hoverinfo = ifelse(!isShape, 'text', 'none'),
		hovertemplate = if(!isShape)	varToFm("hover"),
		opacity = alpha
	)

#	xMax <- max(data[, c(timeStartVar, timeEndVar)], na.rm = TRUE)
	pl <- pl %>% layout(
		title = title,
		xaxis = list(title = list(text = xLab)),
#			rangeslider = list(range = c(0, xMax)), 
		yaxis = list(
			showticklabels = FALSE, ticks = "", 
			title = list(text = yLab),
			showgrid = FALSE
		),
		legend = list(orientation = "h", xanchor = "center", x = 0.5),
		hovermode = "closest"
	)
	
	# specific formatting for medical monitoring
	pl <- formatPlotlyMonitoring(
		data = data, pl = pl,
		# extract patient profile based on the 'yVar' variable
		# (should be included in the plot)
		idVar = "yVar", pathVar = pathVar,
		idVarPlot = "y", idFromDataPlot = FALSE, 
		id = id, 
		verbose = verbose,
		labelVarPlot = "y"
	)
	
	# create associated table
	if(table){
		
		if(missing(tableVars)){
			
			tableVars <- c(paramVar, timeStartVar, timeEndVar)
			tableLab <- c(
				getLabelVar(var = paramVar, label = paramLab, labelVars = labelVars),
				getLabelVar(var = timeStartVar, label = timeStartLab, labelVars = labelVars),
				getLabelVar(var = timeEndVar, label = timeEndLab, labelVars = labelVars)
			)

		}else	if(missing(tableLab)){
			tableLab <- getLabelVar(tableVars, labelVars = labelVars)
		}
		tableVars <- unique(tableVars);tableLab <- tableLab[tableVars]
		
		table <- tableMonitoring(
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
		res <- list(plot = pl, table = table)
		
		class(res) <- c("medicalMonitoring", class(res))
		
	}else res <- pl
	
	return(res)
	
	
	
}
