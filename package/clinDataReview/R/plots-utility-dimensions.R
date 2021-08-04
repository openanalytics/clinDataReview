#' Get facetting variables from facet parameters.
#' @inheritParams clinDataReview-common-args
#' @return Character vector with facetting variable
#' @author Laure Cougnaud
getFacetVars <- function(facetPars = list()){
	
	facetVars <- lapply(facetPars[c("facets", "rows", "cols")], function(par)
		if(inherits(par, "formula"))	all.vars(par)	else	par
	)
	facetVars <- unique(unlist(facetVars))
	
	return(facetVars)
	
}

#' Set facetting layout for 'wrap' facetting.
#' 
#' By default, the number of columns is 2.
#' @inheritParams clinDataReview-common-args
#' @return Updated \code{facetPars}.
#' @author Laure Cougnaud
setFacetLayoutWrap <- function(data, facetPars = list()){
	
	facetVars <- getFacetVars(facetPars)
	facetVarComb <- do.call(interaction, data[, facetVars, drop = FALSE])
	nPlots <- nlevels(droplevels(facetVarComb))
	
	ncolDef <- 2
	
	if(is.null(facetPars$ncol) & is.null(facetPars$nrow)){
		facetPars$ncol <- min(ncolDef, nPlots)
		facetPars$nrow <- ceiling(nPlots/facetPars$ncol)
	}else	if(is.null(facetPars$ncol)){
		facetPars$ncol <- ceiling(nPlots/facetPars$nrow)
	}else	if(is.null(facetPars$nrow)){
		facetPars$nrow <- ceiling(nPlots/facetPars$ncol)
	}
	
	return(facetPars)

}

#' Get plot dimensions
#' @param gg \code{\link[ggplot2]{ggplot}}
#' @return Numeric vector with number of rows ('nrow')
#' and columns ('ncol') of the plot
#' @importFrom ggplot2 ggplot_build wrap_dims
#' @author Laure Cougnaud
getDimGgplot <- function(gg){
	
	ggFacetPars <- ggplot_build(gg)$layout$facet_params
	nrow <- ggFacetPars$nrow
	ncol <- ggFacetPars$ncol
	
	nData <- sapply(ggplot_build(gg)$data, function(x){
		length(unique(x$PANEL))
	})
	nData <- max(nData)
	dim <- wrap_dims(n = nData, nrow = nrow, ncol = ncol)
	names(dim) <- c("nrow", "ncol")
	
	return(dim)

}

#' Get dimensions for a clinical data plot
#' @param gg \code{\link[ggplot2]{ggplot}}
#' @param nrow single-length integer specifying the 
#'   number of facet rows in the plot. (default = 1) 
#'   Overwritten if \code{gg} is specified. 
#' @param ncol single-length integer specifying the 
#'   number of facet columns in the plot. (default = 1)
#'   Overwritten if \code{gg} is specified. 
#' @param legend Logical, if TRUE (by default)
#' a legend is available in the plot.
#' @param legendPosition String with position of the legend,
#' 'right' by default.
#' @inheritParams clinDataReview-common-args
#' @return Numeric vector with width ('width')
#' and height ('height') of the plot
#' in pixels.
#' @author Laure Cougnaud
getSizePlotClinData <- function(
	width = NULL, height = NULL,
	gg = NULL,
	nrow = 1L,
	ncol = 1L,
	legend = TRUE, 
	legendPosition = "right",
	caption = NULL,
	subtitle = NULL){
	
	isWidthSpec <- !is.null(width)
	isHeightSpec <- !is.null(height)

	widthDef <- 800
	heightDef <- 500

	# extract layout (in case facetting)
	if(!is.null(gg)){
		
		plotDim <- getDimGgplot(gg = gg)
		nrow <- unname(plotDim["nrow"])
		ncol <- unname(plotDim["ncol"])
		
	}
	
	if(!isHeightSpec){
		height <- heightDef * nrow
	}
	if(!isWidthSpec){
		width <- widthDef
	}
	
	# add space for legend
	# (only if width/height not specified)
	if(legend && !(!is.null(legendPosition) && legendPosition == "none")){
		if(!isHeightSpec && legendPosition %in% c("bottom", "top"))
			height <- height + height/nrow*0.2
		if(!isWidthSpec && legendPosition %in% c("left", "right"))
			width <- width + width/ncol*0.2
	}
	
	# add space for caption & subtitle
	# (only if height is not specified)
	if(!isHeightSpec){
		if(!is.null(caption)){
			height <- height + getHeightLabs(caption)
		}
		if(!is.null(subtitle)){
			height <- height + getHeightLabs(subtitle)
		}
	}
	
	dim <- c(width = width, height = height)
	
	return(dim)
	
}

#' Get height of labs of the plot,
#' i.e.g caption or subtitle
#' @param lab String with label.
#' @return Integer with height in pixels 
#' for this element.
#' @author Laure Cougnaud
getHeightLabs <- function(lab){
	
	nLines <- countNLines(lab)
	height <- 15*nLines
	
	return(height)
	
}
