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
#' @param facetType String with facetting type, either:
#' \itemize{
#' \item{'wrap': }{\code{\link[ggplot2]{facet_wrap}}}
#' \item{'grid': }{\code{\link[ggplot2]{facet_grid}}}
#' }
#' @param themePars List with general theme parameters 
#' (see \code{\link[ggplot2]{theme}}).
#' @param hoverVar Character vector with variables to be displayed in the hover,
#' by default \code{xVar}, \code{yVar} and any aesthetic variables.
#' @param geomType String with type of the geom used, either:
#' 'point' or 'col'
#' @inheritParams medicalMonitoring-common-args
#' @return \code{\link[ggplot2]{ggplot}} object
#' @import ggplot2
#' @importFrom glpgUtilityFct getLabelVar
#' @importFrom stats setNames
#' @author Laure Cougnaud
staticPlotMonitoring <- function(
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
	hoverVar = NULL,
	geomType = c("point", "col")){

	facetType <- match.arg(facetType)
	geomType <- match.arg(geomType)
	
	isSharedData <- inherits(x = data, what = "SharedData")
	dataContent <- if(isSharedData){
		data$origData()
	}else	data
	
	# limits are of length 2
	checkAxis <- function(paramName)
		if(!is.null(get(paramName)) && length(get(paramName)) != 2)
			stop("When specified, '", paramName, "' parameter should be of length 2.")
	checkAxis("yLim");checkAxis("xLim")
	
	if(missing(aesLab)){
		aesVar <- unique(unlist(c(aesPointVar, aesLineVar)))
		aesLab <- setNames(getLabelVar(aesVar, labelVars = labelVars), names(aesVar))
	}

	# base plot
	# specify data in 'ggplot' call, e.g. to have line from variable correctly facetted
	aesBase <- list(x = xVar, y = yVar)
	gg <- ggplot(data = data, mapping = do.call(aes_string, aesBase))
	
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
	aesGeom <- c(aesPointVar, if(!is.null(hoverVar))	list(text = "hover"))
	argsGeom <- if(length(aesGeom)){
		list(mapping = do.call(aes_string, aesGeom))
	}
	geomFct <- match.fun(paste("geom", geomType, sep = "_"))
	gg <- gg + do.call(geomFct, argsGeom)
	
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
	labsArgs <- c(list(x = unname(xLab), y = unname(yLab)), as.list(aesLab))
	labsArgs <- labsArgs[!sapply(labsArgs, is.null)]
	if(length(labsArgs) > 0)
		gg <- gg + do.call(labs, labsArgs)
	
	# add reference lines (if any)
	gg <- addReferenceLinesMonitoringPlot(
		gg = gg, 
		data = dataContent, 
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
	
	return(gg)
	
}
