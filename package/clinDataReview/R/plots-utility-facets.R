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
	legend = TRUE, legendPosition = "right"){
	
	widthDef <- 800

	# extract layout (in case facetting)
	if(!is.null(gg)){
		
		plotDim <- getDimGgplot(gg = gg)
		nrow <- unname(plotDim["nrow"])
		ncol <- unname(plotDim["ncol"])
		
	}else	nrow <- ncol <- 1
	
	if(is.null(width) & is.null(height)){
		width <- widthDef
		plotSize <- width/ncol
		height <- plotSize*nrow
	}else	if(is.null(height)){
		plotSize <- width/ncol
		height <- plotSize*nrow
	}else	if(is.null(width)){
		plotSize <- height/nrow
		width <- plotSize*ncol
	}
	
	# add space for legend
	if(legend && !(!is.null(legendPosition) && legendPosition == "none")){
		if(legendPosition %in% c("bottom", "top"))
			height <- height + height/nrow*0.2
		if(legendPosition %in% c("left", "right"))
			width <- width + width/ncol*0.2
	}
	
	dim <- c(width = width, height = height)
	
	return(dim)
	
}
