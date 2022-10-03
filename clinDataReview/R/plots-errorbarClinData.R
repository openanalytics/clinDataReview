#' Interactive plot of confidence interval/error interval of clinical data.
#' 
#' This plot is designed to display summary statistics
#' of a continuous variable with (confidence) intervals.\cr
#' The intervals are either displayed:
#' \itemize{
#' \item{vertically if \code{yErrorVar} is specified}
#' \item{horizontally if \code{xErrorVar} is specified}
#' }
#' Error bars can visualized by group, via the color variable parameter.\cr
#' Different symbols are set for each central point of the error bar
#' via the shape variable parameter.
#' @param xErrorVar,yErrorVar String with variable of \code{data}
#' containing the width of the interval (from
#' the center of the interval) for
#' horizontal or vertical intervals.
#' @param xErrorLab,yErrorLab String with labels
#' for \code{xErrorVar}/\code{yErrorVar} variables.
#' @param xAxisLab,yAxisLab Label for the x/y-axis.
#' @param xLabVars (vertical error bars) 
#' Character vector with variable(s) to be displayed 
#' as the labels of the ticks in the x-axis.\cr
#' By default, \code{xVar} is displayed.\cr
#' In case the variable(s) contain different elements 
#' by \code{xVar}, they are combined
#' and displayed below each other.
#' @param mode String with the mode of the plot,
#' 'markers' by default, so only data points are displayed. \cr
#' This can also be set to 'lines' to include a line connecting
#' the center of the error bars instead; or 'lines+markers' 
#' to include both a marker and a line.\cr
#' See \code{mode} attribute for plotly scatter.
#' @param shapeVar (optional) String with shape variable.
#' @param shapePalette (optional) Named character vector with 
#' shape palette, \code{\link[clinUtils]{clinShapes}}
#' by default.
#' @param shapeLab String with label for \code{shapeVar}.
#' @param size Integer with size of markers in pixels, 
#' 6 by default.
#' @param legendPosition String with position of the legend,
#' among: 'top'/'left'/'bottom'/'right', 'bottom' by default.
#' @inheritParams clinDataReview-common-args-summaryStatsVis
#' @inheritParams clinDataReview-common-args
#' @inheritParams tableClinData
#' @inherit scatterplotClinData return
#' @example inst/examples/errorbarClinData-example.R
#' @family visualizations of summary statistics for clinical data
#' @import plotly
#' @importFrom clinUtils getColorPalette
#' @author Laure Cougnaud
#' @export
errorbarClinData <- function(
	data, 
	# plot variables:
	xVar, xLab = getLabelVar(xVar, labelVars = labelVars),
	yVar, yLab = getLabelVar(yVar, labelVars = labelVars),
	yErrorVar = NULL, yErrorLab = getLabelVar(yErrorVar, labelVars = labelVars),
	xErrorVar = NULL, xErrorLab = getLabelVar(xErrorVar, labelVars = labelVars),
	xLabVars = NULL,
	xAxisLab = paste(c(xLab, xErrorLab), collapse = " and "),
	yAxisLab = paste(c(yLab, yErrorLab), collapse = " and "),
	colorVar = NULL, colorLab = getLabelVar(colorVar, labelVars = labelVars),
	colorPalette = NULL,
	shapeVar = NULL, shapeLab = getLabelVar(shapeVar, labelVars = labelVars),
	shapePalette = NULL,
	size = 6,
	titleExtra = NULL,
	title = paste(c(
		paste(yAxisLab, "vs", xAxisLab),
		titleExtra
		), 
		collapse = "<br>"
	),
	subtitle = NULL, caption = NULL,
	labelVars = NULL,
	mode = "markers",
	legendPosition = "bottom",
	# interactivity:
	width = NULL, height = NULL,
	pathVar = NULL, pathLab = getLabelVar(pathVar, labelVars = labelVars),
	hoverVars, hoverLab,
	id = paste0("plotClinData", sample.int(n = 1000, size = 1)),
	# selection
	selectVars = NULL, selectLab = getLabelVar(selectVars, labelVars = labelVars),
	# table
	table = FALSE, 
	tableVars,
	tableLab,
	tableButton = TRUE, tablePars = list(),
	verbose = FALSE){
	
	# store input parameter values for further use
	plotArgs <- c(as.list(environment()))
	
	if(!is.null(xErrorVar) & !is.null(yErrorVar))
		stop(paste("Variable with width of the interval",
			"cannot be specified both on the x or the y axis."))
	if(is.null(xErrorVar) & is.null(yErrorVar))
		stop(paste("Variable with width of the interval",
			"error should be specified."))
	
	if(!is.null(selectVars) & !is.null(xLabVars)){
	  warning(paste("The specification of the x label variable(s) is not",
	    "compatible with the specification of the filtering variable(s)",
	    "so the x label variables are not considered."))
	  xLabVars <- NULL
	}
	
	# extract variable without error bar
	groupAxis <- ifelse(!is.null(yErrorVar), "x", "y")
	groupVar <- switch(groupAxis, x = xVar, y = yVar)
	
	# extract unique ID for each plot element
	idVars <- c(groupVar, colorVar, selectVars)
	data$idEl <- interaction(data[, idVars, drop = FALSE])
	
	# extract default hover variables
	if(missing(hoverVars)){
		hoverVars <- c(xVar, xErrorVar, colorVar, yVar, yErrorVar, shapeVar, selectVars)
		hoverLab <- setNames(
			c(xLab, xErrorLab, colorLab, yLab, yErrorLab, shapeLab, selectLab), 
			hoverVars
		)
	}else	if(missing(hoverLab)){
		hoverLab <- getLabelVar(hoverVars, labelVars = labelVars)
	}
	hoverVars <- unique(hoverVars)
	
	if(groupAxis == "y"){
		
		# split labels if too long and revert order
		# otherwise first levels will appear
		# at the bottom of the axis
		data[, groupVar] <- formatVarForPlotLabel(
			data = data, 
			paramVar = groupVar, 
			width = 20, revert = TRUE
		)
		
	}
	
	# drop unused factor levels as plotly default
	# otherwise might have issues to assign xLabVars
	if(is.factor(data[, groupVar]))
		data[, groupVar] <- droplevels(data[, groupVar])
	if(is.character(data[, groupVar]))
		data[, groupVar] <- factor(data[, groupVar])

	changeAxisGroupVar <- !is.null(xLabVars) | !is.null(colorVar)
	
	# convert group var to numeric if:
	# - a color var is specified -> add jitter in case of multiple color groups
	if(changeAxisGroupVar){
		
		if(is.character(data[, colorVar]))
			data[, colorVar] <- factor(data[, colorVar])
		
		data$groupJitter <- getJitterVar(
			data = data, 
			var = groupVar, byVar = colorVar
		)	
			
		groupVarPlot <- "groupJitter"
		
	}else{
		groupVarPlot <- groupVar
	}
	
	# format data to: 'SharedData' object
	keyVar <- "idEl"
	dataSharedData <- formatDataForPlotClinData(
		data = data, 
		hoverVars = hoverVars, hoverLab = hoverLab,
		hoverByVar = "idEl",
		keyVar = keyVar, id = id,
		labelVars = labelVars
	)
	
	# get plot dim
	dimPlot <- getSizePlot(
		width = width, height = height,
		title = title, 
		subtitle = subtitle,
		caption = caption,
		xLab = xAxisLab, 
		includeLegend = !is.null(colorVar), 
		legendPosition = legendPosition,
		y = if(groupAxis == "y")
			data[, groupVar]
	)
	width <- dimPlot[["width"]]
	height <- dimPlot[["height"]]
	
	# extract color palette
	if(is.null(colorPalette)){
		colorPaletteOpt <- getOption("clinDataReview.colors")
		if(!is.null(colorVar)){
			colorPalette <- getColorPalette(
				x = data[, colorVar], 
				palette = colorPaletteOpt
			)
		}else	colorPalette <- getColorPalette(n = 1, palette = colorPaletteOpt)
	}
	
	# extract shape palette
	if(is.null(shapePalette)){
		shapePaletteOpt <- getOption("clinDataReview.shapes")
		if(!is.null(shapeVar)){
			shapePalette <- getShapePalette(
				x = data[, shapeVar], 
				palette = shapePaletteOpt
			)
		}else	shapePalette <- getShapePalette(n = 1, palette = shapePaletteOpt)
	}
	
	# build the plot
	argsPlotly <- list(
		data = dataSharedData, 
		ids = varToFm("idEl"),
		color = if(!is.null(colorVar))	varToFm(colorVar)	else	I(colorPalette), 
		colors = if(!is.null(colorVar))	colorPalette,
		symbol = if(!is.null(shapeVar))	varToFm(shapeVar)	else	I(shapePalette),
		symbols = if(!is.null(shapePalette))	shapePalette,
		type = "scatter",
		mode = mode,
		hovertemplate = varToFm("hover"),
		width = width, height = height,
		marker = list(size = size)
	)
	
	argsPlotlyAxis <- switch(groupAxis,
		x = list(
			x = varToFm(groupVarPlot),
			y = varToFm(yVar),
			error_y = list(
				array = varToFm(yErrorVar),
				type = "data"
			)
		),
		y = list(
			x = varToFm(xVar),
			error_x = list(
				array = varToFm(xErrorVar),
				type = "data"
			),
			y = varToFm(groupVarPlot)
		)
	)
	argsPlotly <- c(argsPlotly, argsPlotlyAxis)
	pl <- do.call(plot_ly, argsPlotly)
	
	## set axes
	xAxisArgs <- yAxisArgs <- NULL
	if(!is.null(xLabVars) && groupAxis != "x")
		stop(paste(
			"Variables for the x-axis labels",
			"can only be specified if the error bars",
			"are included in the y-direction."
		))
	# set axis labels
	if(changeAxisGroupVar){
		
		axisArgs <- getAxisLabs(
			data = data, 
			var = groupVar, 
			labVars = if(!is.null(xLabVars)){
				xLabVars
			}else	groupVar
		)
		axisArgs <- c(
		  list(
		    type = "array",
			  tickvals = seq_along(axisArgs), 
			  ticktext = axisArgs
			),
			if(is.null(selectVars))
			  list(range = c(0.5, length(axisArgs)+0.5))
		)
		switch(groupAxis,
			`x` = {
				xAxisArgs <- axisArgs
				# fix for automatic shrink of figure when select box
				if(!is.null(selectVars))
				  xAxisArgs$automargin <- FALSE
			},
			`y` = {
				yAxisArgs <- axisArgs
			}
		)
	}
	
	# set layout
	pl <- layoutClinData(
		p = pl,
		title = title,
		xLab = xAxisLab,
		yLab = yAxisLab,
		caption = caption,
		subtitle = subtitle,
		includeLegend = !is.null(colorVar),
		legendPosition = legendPosition,
		width = width,
		height = height,
		# extra params passed to plotly::layout
		legend = list(title = list(text = colorLab)),
		xaxis = xAxisArgs,
		yaxis = yAxisArgs
	)
	
	# specific formatting for clinical data
	res <- formatPlotlyClinData(
		data = data, pl = pl,
		idVar = "idEl", pathVar = pathVar,
		# extract ID from 'id' column directly the plot output object
		idFromDataPlot = FALSE, idVarPlot = "id",
		id = id, 
		# selection
		selectVars = selectVars, selectLab = selectLab, labelVars = labelVars,
		keyVar = keyVar, 
		verbose = verbose
	)
	
	# create associated table
	if(table){
		
		tableVars <- getPlotTableVars(
			plotFunction = "errorbarClinData", 
			plotArgs = plotArgs
		)
		tableLab <- attr(tableVars, "tableLab")
		
		table <- tableClinData(
			data = data, 
			keyVar = keyVar, idVar = groupVar,
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

#' Add jitter to the variable of the plot,
#' based on the different groups of a grouping variable
#' @param var String with variable to add a jitter to.
#' @param byVar String with variable containing the
#' groups to jitter by.
#' @inheritParams clinDataReview-common-args
#' @return Numeric vector of length \code{nrow(data)}
#' containing the jittered variable.
#' @author Laure Cougnaud
getJitterVar <- function(data, var, byVar){
	
	if(is.character(data[, var]))
		stop("The x-variable should be a numeric or factor.")
	if(is.character(data[, byVar]))
		stop("The color-variable should be a numeric or factor.")
	
	varNum <- as.numeric(data[, var])
	
	byVarNum <- as.numeric(data[, byVar])
	nGroups <- length(unique(byVarNum))
	
	if(nGroups > 1){
		
		jitter <- ifelse(
			is.numeric(data[, var]), 
			ifelse(length(unique(data[, var])) == 1, 0.1, min(diff(sort(unique(data[, var])))) * 0.1), 
			0.3
		)
		
		scale <- 1/(jitter/(nGroups-1))
		byVarNumJitter <- c(scale(byVarNum, center = TRUE, scale = scale))
		varNum <- varNum + byVarNumJitter
	}
	
	return(varNum)
	
}
