#' Sunburst interactive plot.
#' 
#' Note: the table and plot are not (yet) linked.
#' @param parentVar,parentLab String with variable of \code{data} containing parent nodes,
#' and associated label.
#' @param childVar,childLab String with variable of \code{data} containing child nodes,
#' and associated label.
#' @param valueVar,valueLab String with variable of \code{data} containing node value,
#' and associated label.
#' @param valueType String with type of values in \code{valueVar}
#' (\code{branchvalues} of the \code{\link[plotly]{plot_ly}}) function),
#' among others: 'relative' (default), or 'total' (only if sum(child) <= to parent).
#' @inheritParams medicalMonitoring-common-args
#' @inheritParams tableMonitoring
#' @inherit scatterplotMonitoring return
#' @example inst/examples/sunburstMonitoring-example.R
#' @import plotly
#' @importFrom stats as.formula
#' @author Laure Cougnaud
#' @export
sunburstMonitoring <- function(
	data, 
	# plot variables:
	parentVar, parentLab = getLabelVar(parentVar, labelVars = labelVars),
	childVar, childLab = getLabelVar(childVar, labelVars = labelVars),
	valueVar, valueLab = getLabelVar(valueVar, labelVars = labelVars),
	valueType = "relative",
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
	hoverVars = childVar, 
	hoverLab = getLabelVar(hoverVars, labelVars = labelVars),
	table = FALSE, 
	tableVars,
	tableLab,
	tableButton = TRUE, tablePars = list(),
	id = paste0("plotMonitoring", sample.int(n = 1000, size = 1)),
	verbose = FALSE){
	
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

	idVar <- "key"

	# for plot, consider the child element as the key:
	dataPlot <- data
	dataPlot$key <- dataPlot[, childVar]

	# format data to: 'SharedData' object
	# specific formatting for medical monitoring
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
		
		table <- tableMonitoring(
			data = dataTable, 
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