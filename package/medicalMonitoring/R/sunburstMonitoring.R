#' Sunburst interactive plot.
#' 
#' Note: the table and plot are not (yet) linked.
#' @param valueType String with type of values in \code{valueVar}
#' (\code{branchvalues} of the \code{\link[plotly]{plot_ly}}) function),
#' among others: 'relative' (default), or 'total' (only if sum(child) <= to parent).
#' @inheritParams medicalMonitoring-common-args-summaryStatsVis
#' @inheritParams medicalMonitoring-common-args
#' @inheritParams tableMonitoring
#' @inherit scatterplotMonitoring return
#' @example inst/examples/sunburstMonitoring-example.R
#' @family visualizations of summary statistics for medical monitoring
#' @import plotly
#' @importFrom stats as.formula
#' @author Laure Cougnaud
#' @export
sunburstMonitoring <- function(
	data, 
	# plot variables:
	vars, varsLab = getLabelVar(vars, labelVars = labelVars),
	valueVar, valueLab = getLabelVar(valueVar, labelVars = labelVars),
	valueType = "relative",
	# general plot:
	titleExtra = NULL,
	title = paste(
		paste(valueLab, "by", paste(varsLab, collapse = " and "), 
		titleExtra), collapse = "<br>"
	),
	labelVars = NULL,
	# interactivity:
	width = NULL, height = NULL,
	pathVar = NULL, pathLab = getLabelVar(pathVar, labelVars = labelVars),
	hoverVars = childVar, 
	hoverLab = getLabelVar(hoverVars, labelVars = labelVars),
	table = FALSE, 
	tableVars,
	tableLab,
	tableButton = TRUE, tablePars = list(),
	id = paste0("plotMonitoring", sample.int(n = 1000, size = 1)),
	verbose = FALSE){

	data <- formatToHierarchicalData(data = data, vars = vars)
	
	# child variable: last variable specified in: 'vars'
	childVar <- tail(vars, 1)
	parentVar <- head(vars, -1)
	
	# In case values are 'total' and parent < sum(child)
	# plotly creates an empty plot
	# so revert back to: 'relative' in this case and returns a warning
	if(valueType == "total"){
		groupTest <- sapply(unique(data[, parentVar]), function(group){
			nChild <- sum(data[which(data[, parentVar] == group), valueVar])
			idxParent <- which(data[, childVar] == group)
			if(length(idxParent)){
				nParent <- sum(data[idxParent, valueVar])
				nChild > nParent
			}else FALSE
		})
		if(any(groupTest)){
			warning("Parent node(s): ", toString(names(which(groupTest))), 
				" are smaller than the sum of their children, ",
				"so 'valueType' is set to 'relative' (instead of 'total')."
			)
			valueType <- "relative"
		}
	}

	# format data to: 'SharedData' object
	# specific formatting for medical monitoring
	if(missing(hoverVars)){
		hoverVars <- c(vars, valueVar)
		hoverLab <- setNames(c(varsLab, valueLab), hoverVars)
	}else	if(missing(hoverLab)){
		hoverLab <- getLabelVar(hoverVars, labelVars = labelVars)
	}
	dataSharedData <- formatDataForPlotMonitoring(
		data = data,
		keyVar = vars, id = id,
		labelVars = labelVars,
		hoverVars = hoverVars, hoverLab = hoverLab,
		hoverByVar = vars
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
		parents = varToFm(parentVar), 
		labels = varToFm(childVar), 
		values = varToFm(valueVar), 
		type = "sunburst",
		branchvalues = valueType,
		hovertemplate = varToFm("hover"),
		width = width, height = height
	)
	pl <- pl %>% layout(title = title)
	
	# current hovered element identified by d.points[0].label
	
	# specific formatting for medical monitoring
	pl <- formatPlotlyMonitoring(
		data = data, pl = pl,
		idVar = vars, pathVar = pathVar, 
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
		
#		dataTable <- data
#		dataTable$key <- ifelse(
#			dataTable[, parentVar] %in% dataTable[, childVar],
#			dataTable[, parentVar],
#			dataTable[, childVar]
#		)
		
		table <- tableMonitoring(
			data = data, 
			idVar = vars, 
			pathVar = pathVar, pathLab = pathLab,
			pathExpand = TRUE,
			tableVars = tableVars,
			tableLab = tableLab,
			tableButton = tableButton, tablePars = tablePars,
			# link table <-> plot not functional for sunburst
			# so 'break' temporarily this link (until this functionality is fixed)
			id = paste0(id, "-table"), 
			labelVars = labelVars
		)
		res <- list(plot = pl, table = table)
		
		class(res) <- c("medicalMonitoring", class(res))
		
	}else res <- pl
	
	return(res)
	
}