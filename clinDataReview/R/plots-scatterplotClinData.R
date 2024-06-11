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
#' \item opened in a different window in the browser if the user clicks on the 
#' 'p' (a.k.a 'profile') key when hovering on a point of the plot
#' \item opened in the browser via hyperlink in the table
#' }
#' @param pathExpand Logical, if FALSE (by default)
#' the path to subject-report is included in an hyperlink in the table,
#' otherwise a collapsed row is created.
#' This should be set to TRUE only if multiple paths 
#' are included for each row in \code{pathVar}
#' (e.g. in case of summary table).
#' @param idHighlightBox Logical, if TRUE (FALSE by default) a selectize box
#' is included to highlight selected element(s) of the ID variable (\code{idVar}).
#' @inheritParams staticScatterplotClinData
#' @inheritParams clinDataReview-common-args
#' @inheritParams tableClinData
#' @return Either:
#' \itemize{
#' \item if a \code{table} is requested: a \code{clinDataReview} object, 
#' a.k.a a list with the 'plot' (\code{\link[plotly]{plotly}} object) and 'table'
#' (\code{\link[DT]{datatable}} object)
#' \item otherwise: a \code{\link[plotly]{plotly}} object
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
	xLab = getLabelVar(var = xVar, labelVars = labelVars),
	xLabVar = NULL, 
	yLab = getLabelVar(var = yVar, labelVars = labelVars), 
	yLabVar = NULL, 
	# aesthetics specifications
	aesPointVar = list(),
	pointPars = list(),
	aesLineVar = list(),
	linePars = list(),
	lineInclude = length(aesLineVar) > 0,
	aesSmoothVar = list(),
	smoothPars = list(),
	smoothInclude = length( c( aesSmoothVar, smoothPars ) ) > 0,
	aesLab,
	# axis specification:
	xTrans = "identity", yTrans = "identity",
	xPars = list(), yPars = list(),
	xLabVars = NULL,
	yLim = NULL, xLim = NULL, 
	yLimExpandData = TRUE, xLimExpandData = TRUE,
	# general plot:
	title = paste(c(paste(yLab, "vs", xLab), titleExtra), collapse = "<br>"),
	titleExtra = NULL,
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
	idHighlightBox = FALSE,
	pathVar = NULL, pathExpand = FALSE,
	id = paste0("plotClinData", sample.int(n = 1000, size = 1)),
	# selection
	selectVars = NULL, selectLab = getLabelVar(selectVars, labelVars = labelVars),
	# table
	table = FALSE, 
	tableVars,
	tableLab,
	tableButton = TRUE, tablePars = list(),
	watermark = NULL,
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
		hoverVars <- c(xVar, xLabVar, yVar, yLabVar, aesVar, selectVars)
		hoverLab <- c(
			getLabelVar(var = xVar, label = xLab, labelVars = labelVars),
			getLabelVar(var = xLabVar, labelVars = labelVars),
			getLabelVar(var = yVar, label = yLab, labelVars = labelVars),
			getLabelVar(var = yLabVar, labelVars = labelVars),
			getLabelVar(var = aesVar, label = aesLab, labelVars = labelVars),
			getLabelVar(var = selectVars, label = selectLab, labelVars = labelVars)
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
	
	# include xLabVar and yLabVar in the axes
	xAxisLab <- getAxisLab(axisVar = xVar, axisLab = xLab, labVar = xLabVar, 
    data = data, labelVars = labelVars)
	yAxisLab <- getAxisLab(axisVar = yVar, axisLab = yLab, labVar = yLabVar, 
    data = data, labelVars = labelVars)
	
	# create static plot
	gg <- staticScatterplotClinData(
		data = dataSharedData, 
		# x/y variables:
		xVar = xVar,
		yVar = yVar, 
		xLab = xAxisLab,
		yLab = yAxisLab,
		# aesthetics specifications
		aesPointVar = aesPointVar,
		pointPars = pointPars,
		aesLineVar = aesLineVar,
		linePars = linePars,
		lineInclude = lineInclude,
		aesSmoothVar = aesSmoothVar,
		smoothPars = smoothPars,
		smoothInclude = smoothInclude,
		aesLab = aesLab,
		# axis specification:
		xTrans = xTrans,
		yTrans = yTrans,
		xPars = xPars,
		yPars = yPars,
		xLabVars = xLabVars,
		yLim = yLim,
		xLim = xLim, 
		yLimExpandData = yLimExpandData,
		xLimExpandData = xLimExpandData,
		# general plot:
		titleExtra = titleExtra,
		title = title,
		facetPars = facetPars,
		facetType = facetType,
		scalePars = scalePars,
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
		xLab = xAxisLab,
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
		xLab = xAxisLab,
		title = title,
		caption = caption, 
		subtitle = subtitle,
		includeLegend = length(c(aesPointVar, aesLineVar)) > 0,
		legendPosition = legendPosition,
		facet = length(facetPars) > 0,
		width = width,
		height = height,
		nrow = nrow, ncol = ncol,
		watermark = watermark
	)

	# convert static to interactive plot
	res <- formatPlotlyClinData(
		data = data, pl = pl,
		idVar = idVar, pathVar = pathVar,
		id = id, verbose = verbose,
		# extract ID from 'key' column in 'data' of the plot output object
		idFromDataPlot = TRUE, idVarPlot = "key",
		pathDownload = FALSE, # open in new tab
		# selection
		selectVars = selectVars, selectLab = selectLab, 
		labelVars = labelVars,
		keyVar = idVar, keyHighlightBox = idHighlightBox
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
		res <- c(
		  if(inherits(res, "plotly")){list(plot = res)}else{res}, 
		  list(table = table)
		)
		
	}
	
	if(!inherits(res, "plotly"))
	  class(res) <- c("clinDataReview", class(res))
		
	return(res)

}
