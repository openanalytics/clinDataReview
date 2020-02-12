#' Scatterplot of variables of interest for medical monitoring.
#' @import ggplot2
#' @importFrom plotly highlight_key ggplotly
#' @importFrom plyr ddply
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
	xLog = FALSE, yLog = FALSE,
	idVar = "USUBJID",
	# general plot:
	titleExtra = NULL,
	title = paste(paste(yLab, "vs", xLab, titleExtra), collapse = "<br>"),
	# interactivity:
	width = NULL, height = NULL,
	hoverVar = unique(c(xVar, yVar, unlist(c(aesPointVar, aesLineVar)))), 
	hoverLab = getLabelVar(hoverVar, labelVars = labelVars),
	lineVar = NULL,
	splitVar = NULL, 
	hLine = NULL, threshold = NULL, 
	labelVars = NULL,
	yLim = NULL, 
	xLim = NULL, 
	abline = NULL, ablineType = "solid",
	footnote = NULL,
	facetPars = list(), facetType = c("wrap", "grid"),
	themePars = list(legend.position = "bottom"),
	urlVar = NULL){
	
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
		facetVars <- lapply(facetPars[c("facets", "rows", "cols")], function(par)
			if(inherits(par, "formula"))	all.vars(par)	else	par
		)
		facetVars <- unique(unlist(facetVars))
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
			cbind(dataPoint, hover = hoverText)
		})
	}
	
	# SharedData object:
	dataPlot <- highlight_key(data = data, key = idVar)
	
	## create static plot:
	
	# base plot
	aesBase <- c(
		list(x = xVar, y = yVar),
		if(!is.null(hoverVar))	list(label = "hover")
	)
	gg <- ggplot(data = dataPlot, mapping = do.call(aes_string, aesBase))
		
	# line
	if(length(aesLineVar) > 0){
		if(!"group" %in% names(aesLineVar)){
			warning("'group' should be specified in the 'aesLineVar'; no line is created.")
		}else{
			argsGeomLine <- if(length(aesLineVar))	list(mapping = do.call(aes_string, aesLineVar))
			gg <- gg + do.call(geom_line, argsGeomLine)
		}
	}
	
	# scatter
	argsGeomPoint <- if(length(aesPointVar))	list(mapping = do.call(aes_string, aesPointVar))
	gg <- gg + do.call(geom_point, argsGeomPoint)
	
	# axis specification
	setAxis <- function(gg, trans, pars){
		res <- if(trans != "identity" | length(pars) > 0){
			argsScale <- c(list(trans = "identity"), pars)
			gg <- do.call(scale_x_continuous, argsScale)
		}else	gg
		return(res)
	}
	gg <- setAxis(gg = gg, trans = xTrans, pars = xPars)
	gg <- setAxis(gg = gg, trans = yTrans, pars = yPars)
	
	# labels
	labsArgs <- c(list(x = xLab, y = yLab), as.list(aesLab))
	labsArgs <- labsArgs[!sapply(labsArgs, is.null)]
	if(length(labsArgs) > 0)
		gg <- gg + do.call(labs, labsArgs)
	
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
	res <- ggplotly(
		p = gg, 
		width = width, height = height, 
		tooltip = if(!is.null(hoverVar))	"label"
	)

	return(res)

}
