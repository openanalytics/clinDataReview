#' Interactive plot of 'count' data
#' 
#' Note: the table and plot are not (yet) linked.
#' @param typePlot String with plot type, 'treemap' or 'sunburst'.
#' @param colorVar (optional) String with coloring variable
#' (NULL by default).
#' By default, the treemap is colored based by section.
#' @param colorLab (optional) String with label for \code{colorVar}.
#' @param colorRange (optional) Numeric vector of length 2 with range 
#' for the color variable, in case it is a numeric variable.
#' @inheritParams medicalMonitoring-common-args-summaryStatsVis
#' @inheritParams medicalMonitoring-common-args
#' @inheritParams tableMonitoring
#' @inherit scatterplotMonitoring return
#' @family visualizations of summary statistics for medical monitoring
#' @import plotly
#' @importFrom stats as.formula
#' @importFrom utils head tail
#' @importFrom RColorBrewer brewer.pal
#' @importFrom glpgUtilityFct getGLPGColorPalette
#' @author Laure Cougnaud
plotCountMonitoring <- function(
	data, 
	# plot variables:
	vars, varsLab = getLabelVar(vars, labelVars = labelVars),
	valueVar, valueLab = getLabelVar(valueVar, labelVars = labelVars),
	colorVar = NULL, colorLab = getLabelVar(valueVar, labelVars = labelVars),
	colorRange = NULL,
	# general plot:
	valueType = "total",
	titleExtra = NULL,
	title = paste(
		paste(valueLab, "by", paste(varsLab, collapse = " and "), 
			titleExtra), collapse = "<br>"
	),
	labelVars = NULL,
	# interactivity:
	width = NULL, height = NULL,
	pathVar = NULL, pathLab = getLabelVar(pathVar, labelVars = labelVars),
	hoverVars = c(vars, valueVar, colorVar), 
	hoverLab = getLabelVar(hoverVars, labelVars = labelVars),
	table = FALSE, 
	tableVars,
	tableLab,
	tableButton = TRUE, tablePars = list(),
	id = paste0("plotMonitoring", sample.int(n = 1000, size = 1)),
	verbose = FALSE,
	typePlot = c("sunburst", "treemap")){
	
	typePlot <- match.arg(typePlot)
	
	# for data to hierarchical format
	dataPlot <- formatToHierarchicalData(data = data, vars = vars)
	varsPlot <- attr(dataPlot, "metadata")
	varID <- varsPlot$varID
	varLabel <- varsPlot$varLabel
	varParent <- varsPlot$varParent
	
	# check that sum (counts children) <= count(parent)
	valueType <- checkValueType(
		data = dataPlot,
		vars = c(varParent, varID), 
		valueVar = valueVar,
		valueType = valueType,
		labelVars = labelVars
	)
	
	# format data to: 'SharedData' object
	dataSharedData <- formatDataForPlotMonitoring(
		data = dataPlot,
		keyVar = varID, 
		id = id,
		labelVars = labelVars,
		hoverVars = hoverVars, hoverLab = hoverLab,
		hoverByVar = varID
	)
	
	# get plot dim
	dimPlot <- getSizePlotMonitoring(
		width = width, height = height,
		legend = FALSE
	)
	width <- unname(dimPlot["width"])
	height <- unname(dimPlot["height"])
	
	# get color vector
	if(!is.null(colorVar)){
		if(is.numeric(dataPlot[, colorVar])){
			if(is.null(colorRange))	colorRange <- range(data[, colorVar], na.rm = TRUE)
			colorsX <- tapply(dataPlot[, colorVar], dataPlot[, varID], mean)
			colorGroups <- cut(colorsX, breaks = c(-Inf, seq(from = colorRange[1], to = colorRange[2], length.out = 8), +Inf))
			colorPalette <- RColorBrewer::brewer.pal(n = nlevels(colorGroups), name = "YlOrRd")
			names(colorPalette) <- levels(colorGroups)
		}else{
			colorGroups <- dataPlot[, colorVar][order(dataPlot[, varID])]
			if(!is.factor(colorGroups))	colorGroups <- factor(colorGroups)	
			colorPalette <- getGLPGColorPalette(x = colorGroups)
		}
		colors <- unname(colorPalette[as.character(colorGroups)])
	}
	# create interactive plot:
	pl <- plot_ly(
		data = dataSharedData, 
		ids = varToFm(varID),
		parents = varToFm(varParent), 
		labels = varToFm(varLabel), 
		values = varToFm(valueVar), 
		marker = if(!is.null(colorVar))	list(colors = colors),
		type = typePlot,
		branchvalues = valueType,
		hovertemplate = varToFm("hover"),
		width = width, height = height,
		textinfo = "label+value"
	)
	pl <- pl %>% layout(
		title = title, 
		# remove the axis labels, included when color is specified in the treemap:
		xaxis = list(showticklabels = FALSE),
		yaxis = list(showticklabels = FALSE),
		legend = list(title = list(text = colorLab))# not yet available in plotly for treemap?
	)
	
	# current hovered element identified by d.points[0].label
	
	# specific formatting for medical monitoring
	pl <- formatPlotlyMonitoring(
		data = dataPlot, pl = pl,
		idVar = varID, pathVar = pathVar,
		idFromDataPlot = FALSE, idVarPlot = "label",
		# click and double-click events already used to zoom/unzoom in sunburst
		highlightOn = "plotly_selected",
		highlightOff = "plotly_relayout",
		id = id, 
		verbose = verbose,
		labelVarPlot = "label"
	)
	
	# create associated table
	if(table){
		
		if(missing(tableVars)){
			tableVars <- c(vars, valueVar)
			tableLab <- setNames(c(varsLab, valueLab), tableVars)
		}else	if(missing(tableLab)){
			tableLab <- getLabelVar(tableVars, labelVars = labelVars)
		}
		
		tablePars <- c(tablePars, list(barVar = valueVar))
		
		table <- tableMonitoring(
			data = dataPlot, 
			idVar = varID, 
			pathVar = pathVar, pathLab = pathLab,
			pathExpand = TRUE,
			tableVars = tableVars,
			tableLab = tableLab,
			tableButton = tableButton, tablePars = tablePars,
			# link table <-> plot not functional for sunburst
			# so 'break' temporarily this link (until this functionality is fixed)
			id = paste0(id, "-table"), 
			labelVars = labelVars,
			verbose = verbose
		)
		res <- list(plot = pl, table = table)
		
		class(res) <- c("medicalMonitoring", class(res))
		
	}else res <- pl
	
	return(res)
	
}