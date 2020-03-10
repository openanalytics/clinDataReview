#' Scatterplot of variables of interest for medical monitoring.
#' @inheritParams staticScatterplotMonitoring
#' @inheritParams medicalMonitoring-common-args
#' @inheritParams tableMonitoring
#' @return If a \code{table} is requested:
#' \itemize{
#' \item{a list with the 'plot' (\code{\link[plotly]{plotly}} object) and 'table'
#' (\code{\link[DT]{datatable}} object with extra class: \code{medicalMonitoringTable})}
#' \item{\code{\link[plotly]{plotly}} object}
#' }
#' @example inst/examples/scatterplotMonitoring-example.R
#' @importFrom glpgUtilityFct getLabelVar
#' @importFrom plotly ggplotly
#' @author Laure Cougnaud
#' @export
scatterplotMonitoring <- function(
	data, 
	# x/y variables:
	xVar, yVar, 
	xLab = getLabelVar(xVar, labelVars = labelVars),
	yLab = getLabelVar(yVar, labelVars = labelVars), 
	# aesthetics specifications
	aesPointVar = list(), aesLineVar = list(),
	aesLab = getLabelVar(unique(unlist(c(aesPointVar, aesLineVar))), labelVars = labelVars),
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
	hoverVars, hoverLab,
	idVar = "USUBJID", idLab = getLabelVar(idVar, labelVars = labelVars),
	pathVar = NULL,
	table = FALSE, 
	tableVars,
	tableLab,
	tableButton = TRUE, tablePars = list(),
	id = paste0("plotMonitoring", sample.int(n = 1000, size = 1)),
	verbose = FALSE){
	
	facetType <- match.arg(facetType)
	
	# extract variables that defines uniquely one point in the plot:
	idVars <- c(xVar, yVar)
	if(!is.null(facetPars)){
		facetVars <- getFacetVars(facetPars)
		idVars <- unique(c(idVars, facetVars))
	}

	# format data to: 'SharedData' object
	if(missing(hoverVars)){
		aesVar <- unlist(c(aesPointVar, aesLineVar))
		hoverVars <- c(xVar, yVar, aesVar)
		hoverLab <- setNames(c(xLab, yLab, aesLab[aesVar]), hoverVars)
	}else	if(missing(hoverLab)){
		hoverLab <- getLabelVar(hoverVars, labelVars = labelVars)
	}
	hoverVars <- unique(hoverVars)
	hoverLab <- hoverLab[!duplicated(hoverLab)]
	
	dataSharedData <- formatDataForPlotMonitoring(
		data = data, 
		hoverVars = hoverVars, hoverLab = hoverLab,
		hoverByVar = idVars,
		keyVar = idVar, id = id,
		labelVars = labelVars
	)
	
	# create static plot:
	gg <- staticScatterplotMonitoring(
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
		hoverVars = hoverVars,
		geomType = "point"
	)
	
	# convert to interactive plot
	pl <- ggplotly(
		p = gg, 
		width = width, height = height, 
		tooltip = if(!is.null(hoverVars))	"text"
	)

	# convert static to interactive plot
	pl <- formatPlotlyMonitoring(
		data = data, pl = pl,
		idVar = idVar, pathVar = pathVar,
		id = id, verbose = verbose
	)
	
	# create associated table
	if(table){
		
		if(missing(tableVars)){
			aesVar <- unlist(c(aesPointVar, aesLineVar))
			if(missing(aesLab))
				aesLab <- setNames(getLabelVar(aesVar, labelVars = labelVars), aesVar)
			tableVars <- unique(c(idVar, xVar, yVar, aesVar))
			tableLab <- setNames(
				c(idLab, xLab, yLab, aesLab[aesVar]), 
				c(idVar, xVar, yVar, aesVar)
			)
		}else	if(missing(tableLab)){
			tableLab <- getLabelVar(tableVars, labelVars = labelVars)
		}
		tableVars <- tableVars[!duplicated(tableVars)]
		tableLab <- tableLab[!duplicated(tableLab)]
		
		table <- tableMonitoring(
			data = data, 
			idVar = idVar, idLab = idLab,
			pathVar = pathVar,
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