#' Scatterplot of variables of interest for clinical data visualization.
#' 
#' The parameters for this visualization
#' are based on \code{ggplot2} (aesthetic, scale, ...), parameter specification,
#' unlike the other visualizations of the package.
#' @param pathVar String with variable of \code{data} containing
#' path to a subject-specific report. The report info should be unique 
#' for each element of \code{idVar}.
#' The report will be:
#' \itemize{
#' \item{opened in a different window in the browser if the user clicks on the 'p' (a.k.a 'profile') key
#' when hovering on a point of the plot}
#' \item{opened in the browser via hyperlink in the table}
#' }
#' @param pathExpand Logical, if FALSE (by default)
#' the path to subject-report is included in an hyperlink in the table,
#' otherwise a collapsed row is created.
#' This should be set to TRUE only if multiple paths 
#' are included for each row in \code{pathVar}
#' (e.g. in case of summary table).
#' @inheritParams staticScatterplotClinData
#' @inheritParams clinDataReview-common-args
#' @inheritParams tableClinData
#' @return Either:
#' \itemize{
#' \item{if a \code{table} is requested: }{
#' a \code{clinDataReview} object, a.k.a a list with the 
#' 'plot' (\code{\link[plotly]{plotly}} object) and 'table'
#' (\code{\link[DT]{datatable}} object)}
#' \item{otherwise: }{a \code{\link[plotly]{plotly}} object}
#' }
#' @example inst/examples/scatterplotClinData-example.R
#' @importFrom clinUtils getLabelVar
#' @import plotly
#' @author Laure Cougnaud
#' @family Clinical data visualization of individual profiles.
#' @export
scatterplotClinData <- function(
	data, 
	# x/y variables:
	xVar, yVar, 
	xLab = getLabelVar(xVar, labelVars = labelVars),
	yLab = getLabelVar(yVar, labelVars = labelVars), 
	# aesthetics specifications
	aesPointVar = list(), 
	aesLineVar = list(), lineInclude = length(aesLineVar) > 0,
	aesLab,
	# axis specification:
	xTrans = "identity", yTrans = "identity",
	xPars = list(), yPars = list(),
	xLabVars = NULL,
	yLim = NULL, xLim = NULL, 
	yLimExpandData = TRUE, xLimExpandData = TRUE,
	# general plot:
	titleExtra = NULL,
	title = paste(paste(yLab, "vs", xLab, titleExtra), collapse = "<br>"),
	caption = NULL, subtitle = NULL,
	facetPars = list(), facetType = c("wrap", "grid"),
	scalePars = list(),
	themePars = list(legend.position = "bottom"),
	refLinePars = NULL,
	labelVars = NULL,
	# interactivity:
	width = NULL, height = NULL,
	hoverVars, hoverLab,
	idVar = "USUBJID", idLab = getLabelVar(idVar, labelVars = labelVars),
	pathVar = NULL, pathExpand = FALSE,
	table = FALSE, 
	tableVars,
	tableLab,
	tableButton = TRUE, tablePars = list(),
	id = paste0("plotClinData", sample.int(n = 1000, size = 1)),
	verbose = FALSE){

	if(missing(aesLab)){
		
		aesVar <- unlist(c(aesPointVar, aesLineVar))
		aesLab <- getLabelVar(aesVar, labelVars = labelVars)
		names(aesLab) <- names(aesVar)
		aesLab <- aesLab[!duplicated(names(aesLab))]
		
	}
	
	# store input parameter values for further use
	plotArgs <- c(as.list(environment()))

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
		hoverLab <- c(
			getLabelVar(var = xVar, label = xLab, labelVars = labelVars),
			getLabelVar(var = yVar, label = yLab, labelVars = labelVars),
			getLabelVar(var = aesVar, label = aesLab, labelVars = labelVars)
		)
	}else	if(missing(hoverLab)){
		hoverLab <- getLabelVar(hoverVars, labelVars = labelVars)
	}
	hoverVars <- unique(hoverVars);hoverLab <- hoverLab[hoverVars]
	
	# remove records with missing x or y variable
	idxNonMissing <- which(!(is.na(data[, xVar]) | is.na(data[, yVar])))
	if(length(idxNonMissing) == 0){
		warning(paste0("Empty dataset after filtering of missing values in ", 
			sQuote(xVar), " and ", sQuote(yVar), " for plot: ", id, ".")
		)
		return(invisible())
	}
	data <- data[idxNonMissing, ]
	
	dataSharedData <- formatDataForPlotClinData(
		data = data, 
		keyVar = idVar, id = id,
		labelVars = labelVars,
		hoverVars = hoverVars, hoverLab = hoverLab,
		hoverByVar = idVars
	)
	
	# create static plot:
	gg <- staticScatterplotClinData(
		data = dataSharedData, 
		# x/y variables:
		xVar = xVar, yVar = yVar, 
		xLab = xLab, yLab = yLab, 
		# aesthetics specifications
		aesPointVar = aesPointVar, 
		aesLineVar = aesLineVar, lineInclude = lineInclude,
		aesLab = aesLab,
		scalePars = scalePars,
		# axis specification:
		xTrans = xTrans, yTrans = yTrans,
		xPars = xPars, yPars = yPars,
		xLabVars = xLabVars,
		yLim = yLim, xLim = xLim, 
		yLimExpandData = yLimExpandData, xLimExpandData = xLimExpandData,
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
	
	# set plot dimensions:
	legendPosition <- themePars$legend.position
	if(is.null(legendPosition)) 	legendPosition <- "right"
	dimPlot <- getSizePlot(
		width = width, 
		height = height, 
		gg = gg,
		title = title,
		caption = caption,
		subtitle = subtitle,
		xLab = xLab,
		facet = length(facetPars) > 0,
		includeLegend = length(c(aesPointVar, aesLineVar)) > 0,
		legendPosition = legendPosition
	)
	width <- dimPlot[["width"]]
	height <- dimPlot[["height"]]
	
	# convert to interactive plot
	pl <- ggplotly(
		p = gg, 
		width = width, height = height, 
		tooltip = if(!is.null(hoverVars))	"text"
	)
	
	plotDim <- getDimGgplot(gg = gg)
	nrow <- plotDim[["nrow"]]
	ncol <- plotDim[["ncol"]]
	pl <- layoutClinData(
		p = pl,
		xLab = xLab,
		title = title,
		caption = caption, 
		subtitle = subtitle,
		includeLegend = length(c(aesPointVar, aesLineVar)) > 0,
		legendPosition = legendPosition,
		facet = length(facetPars) > 0,
		width = width,
		height = height,
		nrow = nrow, ncol = ncol
	)

	# convert static to interactive plot
	pl <- formatPlotlyClinData(
		data = data, pl = pl,
		idVar = idVar, pathVar = pathVar,
		id = id, verbose = verbose,
		# extract ID from 'key' column in 'data' of the plot output object
		idFromDataPlot = TRUE, idVarPlot = "key",
		pathDownload = FALSE # open in new tab
	)
	
	# create associated table
	if(table){
		
		tableVars <- getPlotTableVars(
			plotFunction = "scatterplotClinData", 
			plotArgs = plotArgs
		)
		tableLab <- attr(tableVars, "tableLab")
		table <- tableClinData(
			data = data, 
			idVar = idVar, idLab = idLab,
			keyVar = idVar, keyLab = idLab,
			pathVar = pathVar, pathExpand = pathExpand,
			tableVars = tableVars,
			tableLab = tableLab,
			tableButton = tableButton, 
			tablePars = tablePars,
			id = id,
			labelVars = labelVars
		)
		res <- list(plot = pl, table = table)
	
		class(res) <- c("clinDataReview", class(res))
		
	}else res <- pl
		
	return(res)

}