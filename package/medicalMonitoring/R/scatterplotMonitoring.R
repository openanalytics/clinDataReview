#' Scatterplot of variables of interest for medical monitoring.
#' @param id String with general id for the plot:
#' \itemize{
#' \item{'id' is used as \code{group} for the \code{\link[crosstalk]{SharedData}}}
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
#' @importFrom glpgUtilityFct getLabelVar
#' @importFrom plotly ggplotly
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
	
	# extract variables that defines uniquely one point in the plot:
	idVars <- c(xVar, yVar)
	if(!is.null(facetPars)){
		facetVars <- getFacetVars(facetPars)
		idVars <- unique(c(idVars, facetVars))
	}

	# format data to: 'SharedData' object
	dataSharedData <- formatDataForPlotMonitoring(
		data = data, 
		hoverVar = hoverVar, hoverLab = hoverLab,
		hoverByVar = idVars,
		keyVar = idVar, id = id
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
	
	# convert to interactive plot
	pl <- ggplotly(
		p = gg, 
		width = width, height = height, 
		tooltip = if(!is.null(hoverVar))	"text"
	)

	# convert static to interactive plot
	pl <- formatPlotlyMonitoring(
		data = data, pl = pl,
		idVar = idVar, pathVar = pathVar,
		id = id
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