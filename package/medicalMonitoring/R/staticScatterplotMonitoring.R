#' Scatterplot of variables of interest for medical monitoring.
#' @param data Data.frame with input data.
#' @param facetPars List with facetting parameters, passed to the facetting function.
#' @param aesPointVar List with specification of aesthetic variable(s),
#' for the point, passed to the \code{mapping} parameter of \code{\link[ggplot2]{geom_point}},
#' e.g. \code{list(color = "TRTP")}.
#' @param aesLineVar List with specification of aesthetic variable(s),
#' for the line, passed to the \code{mapping} parameter of \code{\link[ggplot2]{geom_point}},
#' e.g. \code{list(group = "USUBJID")}.
#' @param lineInclude Logical, if TRUE (by default if \code{aesLineVar} is specified)
#' include a scatterplot.
#' @param aesLab Named character vector with labels for each aesthetic variable.
#' @param xTrans,yTrans Transformation for the x/y- variables,
#' passed to the \code{trans} parameter of \code{\link[ggplot2]{scale_x_continuous}}/
#' \code{\link[ggplot2]{scale_y_continuous}}.
#' @param xPars,yPars List with extra parameters for x/y axis, passed to the
#' \code{\link[ggplot2]{scale_x_continuous}}/
#' \code{\link[ggplot2]{scale_y_continuous}} functions,
#' besides \code{trans} and \code{limits}.
#' @param xLimExpandData,yLimExpandData Logical (TRUE by default), should the
#' limits specified via \code{xLim}/\code{yLim} be 
#' expanded to include any data points outside of these
#' limits?
#' Please note that the same limits are set for all facets.
#' @param facetType String with facetting type, either:
#' \itemize{
#' \item{'wrap': }{\code{\link[ggplot2]{facet_wrap}}}
#' \item{'grid': }{\code{\link[ggplot2]{facet_grid}}}
#' }
#' @param themePars List with general theme parameters 
#' (see \code{\link[ggplot2]{theme}}).
#' @param hoverVars Character vector with variables to be displayed in the hover,
#' by default \code{xVar}, \code{yVar} and any aesthetic variables.
#' @param geomType String with type of the geom used, either:
#' \itemize{
#' \item{'point': }{scatterplot with \code{\link[ggplot2]{geom_point}} is created}
#' \item{'col': }{barplot with \code{\link[ggplot2]{geom_col}} is created}
#' }
#' @inheritParams medicalMonitoring-common-args
#' @return \code{\link[ggplot2]{ggplot}} object
#' @import ggplot2
#' @importFrom glpgUtilityFct getLabelVar
#' @importFrom stats setNames
#' @author Laure Cougnaud
staticScatterplotMonitoring <- function(
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
	yLim = NULL, xLim = NULL, 
	yLimExpandData = TRUE, xLimExpandData = TRUE,
	# general plot:
	titleExtra = NULL,
	title = paste(paste(yLab, "vs", xLab, titleExtra), collapse = "<br>"),
	facetPars = list(), facetType = c("wrap", "grid"),
	themePars = list(legend.position = "bottom"),
	refLinePars = NULL,
	labelVars = NULL,
	hoverVars = NULL,
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
	
	if(!is.null(xLim) && xLimExpandData){
		xLim <- range(c(xLim, dataContent[, xVar]), na.rm = TRUE)
	}
	if(!is.null(yLim) && yLimExpandData){
		yLim <- range(c(yLim, dataContent[, yVar]), na.rm = TRUE)
	}
	
	if(missing(aesLab)){
		aesVar <- unique(unlist(c(aesPointVar, aesLineVar)))
		aesLab <- setNames(getLabelVar(aesVar, labelVars = labelVars), names(aesVar))
	}

	# base plot
	# specify data in 'ggplot' call, e.g. to have line from variable correctly facetted
	aesBase <- list(x = xVar, y = yVar)
	gg <- ggplot(data = data, mapping = do.call(aes_string, aesBase))
	
	# line
	if(lineInclude){
#		if(!"group" %in% names(aesLineVar)){
#			warning("'group' should be specified in the 'aesLineVar'; no line is created.")
#		}else{
			argsGeomLine <- list(mapping = do.call(aes_string, aesLineVar))
			gg <- gg + do.call(geom_line, argsGeomLine)
#		}
	}
	
	# scatter
	aesGeom <- c(aesPointVar, if(!is.null(hoverVars))	list(text = "hover"))
	argsGeom <- list(mapping = do.call(aes_string, aesGeom))
	geomFct <- match.fun(paste("geom", geomType, sep = "_"))
	gg <- gg + do.call(geomFct, argsGeom)
	
	# axis specification
	setAxis <- function(gg, trans, pars, lims, axis){
		if(trans != "identity"){
			if("trans" %in% names(pars))
				warning(paste0("'trans' in parameters for ", axis, " axis ",
					"are ignored, because specified in dedicated '", axis, "Trans' parameter."))
			pars$trans <- trans
		}
		if(!is.null(lims)){
			if("limits" %in% names(pars))
				warning(paste0("'limits' in parameters for ", axis, " axis",
					 " are ignored, because specified in dedicated '", axis, "Lim' parameter."))
			pars$limits <- lims
		}
		if(length(pars) > 0){
			scaleFct <- get(paste("scale", axis, "continuous", sep = "_"))
			gg <- gg + do.call(scaleFct, pars)
		}
		return(gg)
	}
	gg <- setAxis(gg = gg, trans = xTrans, pars = xPars, lims = xLim, axis = "x")
	gg <- setAxis(gg = gg, trans = yTrans, pars = yPars, lims = yLim, axis = "y")
	
	# labels
	labsArgs <- c(list(x = unname(xLab), y = unname(yLab)), as.list(aesLab))
	labsArgs <- labsArgs[!sapply(labsArgs, is.null)]
	if(length(labsArgs) > 0)
		gg <- gg + do.call(labs, labsArgs)
	
	if(!is.null(title))
		gg <- gg + ggtitle(title)
	
	# facetting:
	if(length(facetPars) > 0){
		
		if(facetType == "wrap")
			facetPars <- setFacetLayoutWrap(data = dataContent, facetPars = facetPars)
		
		facetFct <- get(paste("facet", facetType, sep = "_"))
		gg <- gg + do.call(facetFct, facetPars)
	}
	
	# theme:
	gg <- gg + theme_bw()
	if(length(themePars) > 0)
		gg <- gg + do.call(theme, themePars)
	
	# add reference lines (if any)
	gg <- addReferenceLinesMonitoringPlot(
		gg = gg, 
		data = dataContent, 
		xVar = xVar, yVar = yVar, 
		refLinePars = refLinePars, facetPars = facetPars
	)
	
	return(gg)
	
}
