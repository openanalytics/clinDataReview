#' Scatterplot of variables of interest for medical monitoring.
#' @param id String with general id for the plot:
#' \itemize{
#' \item{'SharedData:[id]' is used as \code{group} for the \code{\link[crosstalk]{SharedData}}}
#' \item{'button:[id]' is used as button ID if \code{table} is TRUE}
#' }
#' If not specified, a random id, as 'scatterplotMonitoringX' is used.
#' @inheritParams scatterplotMonitoringStatic
#' @inheritParams medicalMonitoring-common-args
#' @return If a \code{table} is requested:
#' \itemize{
#' \item{a list with the 'plot' (\code{\link[plotly]{plotly}} object) and 'table'
#' (\code{\link[DT]{datatable}} object with extra class: \code{medicalMonitoringTable})}
#' \item{\code{\link[plotly]{plotly}} object}
#' }
#' @import plotly
#' @importFrom plyr ddply
#' @importFrom htmlwidgets onRender JS
#' @importFrom glpgUtilityFct toDTGLPG getLabelVar
#' @importFrom stats as.formula
#' @author Laure Cougnaud
#' @example inst/examples/scatterplotMonitoring-example.R
#' @export
scatterplotMonitoring <- function(
	data, 
	# x/y variables:
	xVar, yVar, 
	xLab = getLabelVar(xVar, labelVars = labelVars),
	yLab = getLabelVar(yVar, labelVars = labelVars), 
	# aesthetics specifications
	aesPointVar = list(), aesLineVar = list(),
	aesLab,
	# axis specification:
	xTrans = "identity", yTrans = "identity",
	xPars = list(), yPars = list(),
	yLim = NULL, xLim = NULL, 
	# general plot:
	titleExtra = NULL,
	title = paste(paste(yLab, "vs", xLab, titleExtra), collapse = "<br>"),
	facetPars = list(), facetType = c("wrap", "grid"),
	themePars = list(legend.position = "bottom"),
	refLinePars = NULL,
	labelVars = NULL,
	# interactivity:
	width = NULL, height = NULL,
	hoverVar = unique(c(xVar, yVar, unlist(c(aesPointVar, aesLineVar)))), 
	hoverLab = getLabelVar(hoverVar, labelVars = labelVars),
	idVar = "USUBJID", 
	pathVar = NULL,
	table = FALSE, 
	tableVars = unique(c(idVar, xVar, yVar, unlist(c(aesPointVar, aesLineVar)))),
	tableLab = getLabelVar(tableVars, labelVars = labelVars),
	tableButton = TRUE, tablePars = list(),
	id = paste0("scatterplotMonitoring", sample.int(n = 1000, size = 1))){
	
	facetType <- match.arg(facetType)

	# format data to: 'SharedData' object
	dataSharedData <- formatDataForPlotMonitoring(
		data = data, xVar = xVar, yVar = yVar, 
		facetPars = facetPars, 
		hoverVar = hoverVar, hoverLab = hoverLab,
		idVar = idVar, id = id
	)
	
	# create static plot:
	gg <- scatterplotMonitoringStatic(
		data = dataSharedData, 
		# x/y variables:
		xVar = xVar, yVar = yVar, 
		xLab = xLab, yLab = yLab, 
		# aesthetics specifications
		aesPointVar = aesPointVar, aesLineVar = aesLineVar,
		aesLab = aesLab,
		# axis specification:
		xTrans = xTrans, yTrans = yTrans,
		xPars = xPars, yPars = yPars,
		yLim = yLim, xLim = xLim, 
		# general plot:
		titleExtra = titleExtra,
		title = title,
		facetPars = facetPars, facetType = facetType,
		themePars = themePars,
		refLinePars = refLinePars,
		labelVars = labelVars,
		hoverVar = hoverVar
	)

	# convert static to interactive plot
	pl <- ggplotlyMonitoring(
		data = data, gg = gg,
		width = width, height = height,
		idVar = idVar, pathVar = pathVar,
		hoverVar = hoverVar
	)
	
	# create associated table
	if(table){
		
		table <- tableMonitoring(
			data = data, 
			idVar = idVar, 
			pathVar = pathVar ,
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