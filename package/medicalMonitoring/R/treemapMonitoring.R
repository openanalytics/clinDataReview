#' Treemap interactive plot.
#' 
#' Note: the table and plot are not (yet) linked.
#' @param parentVar,parentLab String with variable of \code{data} containing parent nodes,
#' and associated label.
#' @param childVar,childLab String with variable of \code{data} containing child nodes,
#' and associated label.
#' @param valueVar,valueLab String with variable of \code{data} containing node value,
#' and associated label.
#' @inheritParams medicalMonitoring-common-args-summaryStatsVis
#' @inheritParams medicalMonitoring-common-args
#' @inheritParams tableMonitoring
#' @inherit scatterplotMonitoring return
#' @example inst/examples/treemapMonitoring-example.R
#' @family visualizations of summary statistics for medical monitoring
#' @import plotly
#' @importFrom stats as.formula
#' @author Laure Cougnaud
#' @export
treemapMonitoring <- function(
	data, 
	# plot variables:
	parentVar, parentLab = getLabelVar(parentVar, labelVars = labelVars),
	childVar, childLab = getLabelVar(childVar, labelVars = labelVars),
	valueVar, valueLab = getLabelVar(valueVar, labelVars = labelVars),
	# general plot:
	titleExtra = NULL,
	title = paste(
		paste(valueLab, "by", paste(c(parentLab, childLab), collapse = " and "), 
		titleExtra), collapse = "<br>"
	),
	labelVars = NULL,
	# interactivity:
	width = NULL, height = NULL,
	pathVar = NULL, pathLab = getLabelVar(pathVar, labelVars = labelVars),
	hoverVars, hoverLab,
	table = FALSE, 
	tableVars,
	tableLab,
	tableButton = TRUE, tablePars = list(),
	id = paste0("plotMonitoring", sample.int(n = 1000, size = 1)),
	verbose = FALSE){

	idVar <- "key"

	# for plot, consider the child element as the key:
	dataPlot <- data
	dataPlot$key <- dataPlot[, childVar]

	# format data to: 'SharedData' object
	if(missing(hoverVars)){
		hoverVars <- c(childVar, valueVar)
		hoverLab <- setNames(c(childLab, valueLab), hoverVars)
	}else	if(missing(hoverLab)){
		hoverLab <- getLabelVar(hoverVars, labelVars = labelVars)
	}
	dataSharedData <- formatDataForPlotMonitoring(
		data = dataPlot,
		keyVar = idVar, id = id,
		labelVars = labelVars,
		hoverVars = hoverVars, hoverLab = hoverLab,
		hoverByVar = idVar
	)
	
	# get plot dim
	dimPlot <- getSizePlotMonitoring(
		width = width, height = height,
		legend = FALSE
	)
	width <- unname(dimPlot["width"])
	height <- unname(dimPlot["height"])
	
	# create interactive plot:
	pl <- plot_ly(
		data = dataSharedData, 
		parents = varToFm(parentVar), labels = varToFm(childVar), 
		values = varToFm(valueVar), 
		type = "treemap",
		hovertemplate = varToFm("hover"),
		width = width, height = height
	)
	pl <- pl %>% layout(title = title)
	
	# current hovered element identified by d.points[0].label
	
	# specific formatting for medical monitoring
	pl <- formatPlotlyMonitoring(
		data = dataPlot, pl = pl,
		idVar = idVar, pathVar = pathVar,
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
			tableVars <- c(parentVar, childVar, valueVar)
			tableLab <- setNames(
				c(parentLab, childLab, valueLab), 
				c(parentVar, childVar, valueVar)
			)
		}else	if(missing(tableLab)){
			tableLab <- getLabelVar(tableVars, labelVars = labelVars)
		}
		
		dataTable <- data
		dataTable$key <- ifelse(
			dataTable[, parentVar] %in% dataTable[, childVar],
			dataTable[, parentVar],
			dataTable[, childVar]
		)
		
		tablePars <- c(tablePars, list(barVar = valueVar))
		
		table <- tableMonitoring(
			data = dataTable, 
			idVar = idVar, 
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