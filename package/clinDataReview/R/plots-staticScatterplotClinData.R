#' Scatterplot of variables of interest for clinical data visualization
#' @param data Data.frame with input data.
#' @param facetPars List with facetting parameters, passed to the facetting function.
#' @param aesPointVar List with specification of aesthetic variable(s),
#' for the point, passed to the \code{mapping} parameter of \code{\link[ggplot2]{geom_point}},
#' e.g. \code{list(color = "TRTP")}.\cr
#' Please note by default symbols with fill and color are used.
#' Color is used for the outside of the points, fill for the inside
#' and the hover. Usually, you might want to specify both
#' filling and coloring.
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
#' @inheritParams clinDataReview-common-args
#' @inheritParams setPaletteStaticScatterplotClinData
#' @return \code{\link[ggplot2]{ggplot}} object
#' @importFrom ggplot2 sym ggplot aes geom_point geom_col geom_line 
#' scale_discrete_manual scale_x_continuous scale_y_continuous 
#' labs ggtitle facet_wrap facet_grid theme_bw theme
#' @importFrom clinUtils getLabelVar
#' @importFrom stats setNames
#' @author Laure Cougnaud
staticScatterplotClinData <- function(
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
	facetPars = list(), 
	facetType = c("wrap", "grid"),
	scalePars = list(),
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
		aesVar <- unlist(c(aesPointVar, aesLineVar))
		aesLab <- getLabelVar(aesVar, labelVars = labelVars)
		names(aesLab) <- names(aesVar)
		aesLab <- aesLab[!duplicated(names(aesLab))]
	}
	
	# get default palettes
	geomAes <- list()
	for(aesType in c("color", "fill", "linetype", "shape")){
		aesOpt <- ifelse(aesType == "fill", "color", aesType)
		aesTypeVar <- if(aesType == "color"){c("color", "colour")}else{aesType}
		aesVar <- unique(unlist(c(aesPointVar[aesTypeVar], aesLineVar[aesTypeVar])))
		resPalette <- setPaletteStaticScatterplotClinData(
			data = dataContent,
			var = aesVar, aes = aesType, 
			palette = getOption(paste0("clinDataReview.", aesOpt, "s")),
			scalePars = scalePars, geomAes = geomAes
		)
		scalePars <- resPalette$scalePars
		geomAes <- resPalette$geomAes
	}
	
	## base plot
	# specify data in 'ggplot' call, e.g. to have line from variable correctly facetted
	aesBase <- list(x = sym(xVar), y = sym(yVar))
	gg <- ggplot(data = data, mapping = do.call(aes, aesBase))
	
	## line plot
	if(lineInclude){
#		if(!"group" %in% names(aesLineVar)){
#			warning("'group' should be specified in the 'aesLineVar'; no line is created.")
#		}else{
			aesLineVar <- sapply(aesLineVar, sym, simplify = FALSE)
			argsGeomLine <- c(
				list(mapping = do.call(aes, aesLineVar)),
				geomAes[c("color", "colour", "linetype")]
			)
			argsGeomLine <- Filter(Negate(is.null), argsGeomLine)
			gg <- gg + do.call(geom_line, argsGeomLine)
#		}
	}
	
	## scatterplot
	if(length(aesPointVar) > 0)
		aesPointVar <- sapply(aesPointVar, sym, simplify = FALSE)
	aesGeom <- c(aesPointVar, if(!is.null(hoverVars))	list(text = sym("hover")))
	argsGeom <- c(
		list(mapping = do.call(aes, aesGeom)),
		geomAes[c("color", "colour", "fill", "shape")]
	)
	argsGeom <- Filter(Negate(is.null), argsGeom)
	geomFct <- switch(geomType, point = geom_point, col = geom_col)
	gg <- gg + do.call(geomFct, argsGeom)
	
	# aesthetic scales
	for(scaleParsI in scalePars){
		gg <- gg + do.call(scale_discrete_manual, scaleParsI)
	}
	
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
			scaleFct <- switch(axis,
				x = scale_x_continuous,
				y = scale_y_continuous
			)
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
		
		facetFct <- switch(facetType,
			wrap = facet_wrap,
			grid = facet_grid
		)
		gg <- gg + do.call(facetFct, facetPars)
	}
	
	# theme:
	gg <- gg + theme_bw()
	if(length(themePars) > 0)
		gg <- gg + do.call(theme, themePars)
	
	# add reference lines (if any)
	gg <- addReferenceLinesClinDataPlot(
		gg = gg, 
		data = dataContent, 
		xVar = xVar, yVar = yVar, 
		refLinePars = refLinePars, facetPars = facetPars
	)
	
	return(gg)
	
}

#' Get standard palette for the \code{staticScatterplotClinData}
#' function.
#' @param data Data.frame with data for the plot.
#' @param var Character vector with variable(s) to consider.
#' If multiple, currently only the first one is considered.
#' @param aes String with aesthetic, either:
#' 'color', 'shape' or 'linetype'.
#' @param scalePars List with parameters to customize
#' scales. Each sublist should contains a set of parameters
#' passed to the \code{\link[ggplot2]{scale_discrete_manual}}
#' function.\cr
#' If palette(s) are not specified, default palettes are used
#' (see \link[clinUtils]{getColorPalette}, 
#' \link[clinUtils]{getShapePalette}, 
#' \link[clinUtils]{getLinetypePalette}
#' )
#' @param geomAes List with aesthetic for each geom.
#' @param ... Any extra parameters than \code{x} and \code{n}
#' for the default palette fcts.
#' @return List with: \code{scalePars} and \code{geomAes},
#' each of those potentially updated with default palette(s).
#' @importFrom clinUtils getColorPalette getShapePalette getLinetypePalette
#' @importFrom utils hasName
#' @author Laure Cougnaud
setPaletteStaticScatterplotClinData <- function(
	data, var, aes, 
	scalePars, geomAes,
	...){

	aesType <- if(aes %in% c("color", "colour")){c("color", "colour")}else{aes}

	isAes <- sapply(scalePars, function(x)
		any(aesType %in% x[["aesthetic"]])
	)

	isPaletteSpecified <- if(!is.null(scalePars) && any(isAes)){
		any(hasName(scalePars[[which(isAes)]], c("palette", "values")))	
	}else	any(hasName(geomAes, aesType))

	# if palette not specified by the user
	if(!isPaletteSpecified){
	
		if(length(var) > 1){
			warning(paste("Multiple variables for the", aes, "aesthetic",
				"the first one is considered to set default palette."))
			var <- var[1]
		}
		
		getAesPalette <- switch(aes,
			color = getColorPalette,
			colour = getColorPalette,
			fill = getColorPalette,
			shape = getShapePalette,
			linetype = getLinetypePalette			
		)
		
		# aesthetic is mapped to a variable:
		if(!is.null(var)){
			
			palette <- getAesPalette(x = data[[var]], ...)
			if(any(isAes)){
				scalePars[[which(isAes)]][["values"]] <- palette
			}else	scalePars <- c(scalePars, list(list(aesthetic = aes, values = palette)))
			
		
		}else{
			palette <- getAesPalette(n = 1, ...)
			geomAes <- c(geomAes, setNames(list(palette), aes))
		}
		
	}
	
	return(list(scalePars = scalePars, geomAes = geomAes))
	
}
