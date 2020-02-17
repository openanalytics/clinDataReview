#' Scatterplot of variables of interest for medical monitoring.
#' @param data Data.frame with input data.
#' @param xLab String with label for \code{xVar}.
#' @param yLab String with label for \code{xVar}.
#' @param aesPointVar List with specification of aesthetic variable(s),
#' for the point, passed to the \code{mapping} parameter of \code{\link[ggplot2]{geom_point}},
#' e.g. \code{list(color = "TRTP")}.
#' @param aesLineVar List with specification of aesthetic variable(s),
#' for the line, passed to the \code{mapping} parameter of \code{\link[ggplot2]{geom_point}},
#' e.g. \code{list(group = "USUBJID")}.
#' @param aesLab Named character vector with labels for each aesthetic variable.
#' @param xTrans,yTrans Transformation for the x/y- variables,
#' passed to the \code{trans} parameter of \code{\link[ggplot2]{scale_x_continuous}}/
#' \code{\link[ggplot2]{scale_y_continuous}}.
#' @param xPars,yPars List with extra parameters for x/y axis, passed to the
#' \code{\link[ggplot2]{scale_x_continuous}}/
#' \code{\link[ggplot2]{scale_y_continuous}} functions,
#' besides \code{trans} and \code{limits}.
#' @param  List with extra parameters for the \code{\link[ggplot2]{scale_x_continuous}},
#' besides \code{trans} and \code{limits}.
#' @param title String with title for the plot.
#' @param titleExtra String with extra title for the plot (appended after \code{title}).
#' @param facetType String with facetting type, either:
#' \itemize{
#' \item{'wrap': }{\code{\link[ggplot2]{facet_wrap}}}
#' \item{'grid': }{\code{\link[ggplot2]{facet_grid}}}
#' }
#' @param themePars List with general theme parameters 
#' (see \code{\link[ggplot2]{theme}}).
#' @param labelVars Named character vector containing variable labels,
#' used by default for all labels in the plot.
#' @param width, height Width/height of the plot in pixels.
#' @param hoverVar Character vector with variables to be displayed in the hover,
#' by default \code{xVar}, \code{yVar} and any aesthetic variables.
#' @param hoverLab Named character vector with labels for \code{hoverVar}.
#' @param idVar Character vector with variable containing subject ID.
#' @param pathVar String with variable of \code{data} containing path
#' to a subject-specific report (e.g. patient profiles).
#' This report will be downloaded if the user clicks on the 'Alt'+'P' key
#' when hovering on a point.
#' @inheritParams medicalMonitoring-common-args
#' @return \code{\link[plotly]{plotly}} object
#' @import ggplot2
#' @import plotly
#' @importFrom plyr ddply
#' @importFrom htmlwidgets onRender
#' @author Laure Cougnaud
#' @example inst/examples/filterData-example.R
#' @export
scatterplotMonitoring <- function(
	data, 
	# x/y variables:
	xVar, yVar, 
	xLab = getLabelVar(xVar, labelVars = labelVars),
	yLab = getLabelVar(yVar, labelVars = labelVars), 
	# aesthetics specifications
	aesPointVar = list(), aesLineVar = list(),
	aesLab = getLabelVar(unlist(c(aesPointVar, aesLineVar)), labelVars = labelVars),
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
	pathVar = NULL){
	
	facetType <- match.arg(facetType)

	## checks:

	# only one variable per aesthetic:
	aesVarCommon <- intersect(names(aesPointVar), names(aesLineVar))
	if(length(aesVarCommon) > 0){
		if(!identical(aesPointVar[aesVarCommon], aesLineVar[aesVarCommon]))
			stop("Different variables are set for the same aesthetic (", toString(aesVarCommon), ").")
	}
	
	# limits are of length 2
	checkAxis <- function(paramName)
		if(!is.null(get(paramName)) && length(get(paramName)) != 2)
			stop("When specified, '", paramName, "' parameter should be of length 2.")
	checkAxis("yLim");checkAxis("xLim")
	
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
	dataPlot <- highlight_key(data = data, key = keyFm)
	
	## create static plot:
	
	# base plot
	# specify data in 'ggplot' call, e.g. to have line from variable correctly facetted
	aesBase <- list(x = xVar, y = yVar)
	gg <- ggplot(data = dataPlot, mapping = do.call(aes_string, aesBase))
		
	# line
	if(length(aesLineVar) > 0){
		if(!"group" %in% names(aesLineVar)){
			warning("'group' should be specified in the 'aesLineVar'; no line is created.")
		}else{
			argsGeomLine <- if(length(aesLineVar)){
				list(mapping = do.call(aes_string, aesLineVar))
			}
			gg <- gg + do.call(geom_line, argsGeomLine)
		}
	}
	
	# scatter
	aesPoint <- c(aesPointVar, if(!is.null(hoverVar))	list(text = "hover"))
	argsGeomPoint <- if(length(aesPoint)){
		list(mapping = do.call(aes_string, aesPoint))
	}
	gg <- gg + do.call(geom_point, argsGeomPoint)
	
	# axis specification
	setAxis <- function(gg, trans, pars, lims, axis){
		res <- if(trans != "identity" | length(pars) > 0){
			argsScale <- c(list(trans = trans, limits = lims), pars)
			scaleFct <- get(paste("scale", axis, "continuous", sep = "_"))
			gg <- gg + do.call(scaleFct, argsScale)
		}else	gg
		return(res)
	}
	gg <- setAxis(gg = gg, trans = xTrans, pars = xPars, lims = xLim, axis = "x")
	gg <- setAxis(gg = gg, trans = yTrans, pars = yPars, lims = yLim, axis = "y")
	
	# labels
	labsArgs <- c(list(x = xLab, y = yLab), as.list(aesLab))
	labsArgs <- labsArgs[!sapply(labsArgs, is.null)]
	if(length(labsArgs) > 0)
		gg <- gg + do.call(labs, labsArgs)
	
	# add reference lines (if any)
	gg <- addReferenceLinesMonitoringPlot(
		gg = gg, 
		data = data, 
		xVar = xVar, yVar = yVar, 
		refLinePars = refLinePars, facetPars = facetPars
	)
	
	if(!is.null(title))
		gg <- gg + ggtitle(title)
		
	# facetting:
	if(length(facetPars) > 0){
		facetFct <- get(paste("facet", facetType, sep = "_"))
		gg <- gg + do.call(facetFct, facetPars)
	}
	
	# theme:
	gg <- gg + theme_bw()
	if(length(themePars) > 0)
		gg <- gg + do.call(theme, themePars)
	

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
		dataPP <- as.list(setNames(dataPPDf[, pathVar], dataPPDf[, idVar]))
		pl <- pl %>% onRender(
			jsCode = "function(el, x, data){downloadPatientProfilesPlotly(el, x, data);}",
			data = dataPPDf
		)
		
	}
	return(pl)

}
