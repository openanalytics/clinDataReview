#' Scatterplot of variables of interest for medical monitoring.
#' @param width, height Width/height of the plot in pixels.
#' @param hoverVar Character vector with variables to be displayed in the hover,
#' by default \code{xVar}, \code{yVar} and any aesthetic variables.
#' @param hoverLab Named character vector with labels for \code{hoverVar}.
#' @param idVar Character vector with variable containing subject ID.
#' @param pathVar String with variable of \code{data} containing path
#' to a subject-specific report (e.g. patient profiles).
#' This report will be downloaded if the user clicks on the 'Ctrl'+'Enter' key
#' when hovering on a point.
#' @param table Logical, if TRUE (FALSE by default)
#' returns also a \code{datatable} containing the plot data.
#' @param tableButton Logical, if TRUE (by default)
#' the table is included within an HTML button.
#' @param tablePars List with parameters passed to the
#' \code{\link[glpgUtilityFct]{toDTGLPG}} function.
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
#' @importFrom htmlwidgets onRender
#' @importFrom glpgUtilityFct toDTGLPG
#' @importFrom crosstalk bscols
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
	table = FALSE, tableButton = TRUE, tablePars = list(),
	id){
	
	facetType <- match.arg(facetType)
	
	if(missing(id)){
		id <- paste0("scatterplotMonitoring", sample.int(n = 1000, size = 1))
	}

	## checks:

#	# only one variable per aesthetic:
#	aesVarCommon <- intersect(names(aesPointVar), names(aesLineVar))
#	if(length(aesVarCommon) > 0){
#		if(!identical(aesPointVar[aesVarCommon], aesLineVar[aesVarCommon]))
#			stop("Different variables are set for the same aesthetic (", toString(aesVarCommon), ").")
#	}
	
	# extract variables that defines uniquely one point in the plot:
	idVars <- c(xVar, yVar)
	if(!is.null(facetPars)){
		facetVars <- getFacetVars(facetPars)
		idVars <- unique(c(idVars, facetVars))
	}
	
	# create hover variable: combine hover if points have the same x/y coordinates
	# by default in plotly: hover var only displayed for one of the overlapping point
	if(!is.null(hoverVar)){
		data <- ddply(data, idVars, function(dataPoint){
			hoverTextList <- lapply(hoverVar, function(var){
				formatHoverText(
					x = sort(unique(dataPoint[, var])),
					label = hoverLab[var]
				)
			})
			hoverText <- Reduce(function(...) paste(..., sep = "<br>"), hoverTextList)
			cbind.data.frame(dataPoint, hover = hoverText, stringsAsFactors = )
		})
	}
	
	# SharedData object:
	keyFm <- as.formula(paste("~", idVar))
	group <- paste0("SharedData:", id)
	dataPlot <- highlight_key(data = data, key = keyFm, group = group)
	
	## create static plot:
	gg <- scatterplotMonitoringStatic(
		data = dataPlot, 
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
	

	## interactive plot
	pl <- ggplotly(
		p = gg, 
		width = width, height = height, 
		tooltip = if(!is.null(hoverVar))	"text"
	)
	
	# to check attributes available in the plotly object:
#	plotly_json(pl)
	
	if(!is.null(pathVar)){
		
		dataPPDf <- unique(data[, c(idVar, pathVar)])
		if(any(duplicated(dataPPDf[, idVar])))
			stop("Duplicated ", idVar, " for specific ", pathVar, ".")
		dataPP <- dataPPDf[, c(idVar, pathVar)]
		pl <- pl %>% onRender(
			jsCode = "function(el, x, data){downloadPatientProfilesPlotly(el, x, data);}",
			data = dataPPDf
		)
		
	}
	
	if(table){
		
		# create table
		argsToDTGLPG <- c(list(data = dataPlot), tablePars)
		table <- do.call(toDTGLPG, argsToDTGLPG)
		
		if(tableButton){
			idButton <- paste0("button:", id)
#			class(table) <- c("medicalMonitoringTable", class(table))
			attributes(table)$metadata <- list(
				button = tableButton, 
				buttonId = idButton,
				buttonTitle = "Click to show or hide the data associated to the plot"
			)
		}
		res <- list(plot = pl, table = table)
	
		class(res) <- c("medicalMonitoring", class(res))
		
	}else res <- pl
		
	return(res)

}