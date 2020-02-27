#' Sunburst interactive plot
#' @inheritParams medicalMonitoring-common-args
#' @inherit scatterplotMonitoring return
#' @import plotly
#' @importFrom stats as.formula
#' @author Laure Cougnaud
#' @example inst/examples/scatterplotMonitoring-example.R
#' @export
sunburstMonitoring <- function(
	data, 
	# plot variables:
	parentVar, parentLab = getLabelVar(parentLab, labelVars = labelVars),
	childVar, childLab = getLabelVar(childVar, labelVars = labelVars),
	valueVar, valueLab = getLabelVar(valueVar, labelVars = labelVars),
	# general plot:
	titleExtra = NULL,
	title = paste(
		paste(valueLab, "of", paste(c(parentLab, childLab), collapse = " and "), 
		titleExtra), collapse = "<br>"
	),
	labelVars = NULL,
	# interactivity:
	width = NULL, height = NULL,
	pathVar = NULL,
	table = FALSE, 
	tableVars,
	tableLab,
	tableButton = TRUE, tablePars = list(),
	id = paste0("sunburstMonitoring", sample.int(n = 1000, size = 1))){
	
	idVar <- "key"

	# for plot, consider the child element as the key:
	dataPlot <- data
	dataPlot$key <- dataPlot[, childVar]

	# format data to: 'SharedData' object
	dataSharedData <- formatDataForPlotMonitoring(
		data = dataPlot, 
		keyVar = idVar, id = id
	)
	
	# create interactive plot:
	toFm <- function(var)	as.formula(paste0("~", var))
	pl <- plot_ly(
		data = dataSharedData, 
		parents = toFm(parentVar), labels = toFm(childVar), values = toFm(valueVar), 
		type = "sunburst",
		branchvalues = 'remainder',
		width = width, height = height
	)
	
	# convert static to interactive plot
	pl <- formatPlotlyMonitoring(
		data = dataPlot, pl = pl,
		idVar = idVar, pathVar = pathVar,
		# click and double-click events already used to zoom/unzoom in sunburst
		highlightOn = "plotly_selected",
		highlightOff = "plotly_relayout"
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
			pathVar = pathVar,
			tableVars = tableVars,
			tableLab = tableLab,
			tableButton = tableButton, tablePars = tablePars,
			id = id
		)
		res <- list(plot = pl, table = table)
		
		class(res) <- c("medicalMonitoring", class(res))
		
	}else res <- pl
	
	return(res)
	
}