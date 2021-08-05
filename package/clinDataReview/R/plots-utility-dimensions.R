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
#' 
#' This function set sensitive defaults dimensions
#' for a plot in the package.
#' This includes:
#' \itemize{
#' \item{setting a default width for a figure to 
#' fit in a standard clinical data review report}
#' \item{increasing the figure height:}{
#' \itemize{
#' \item{for facetted plot, ensuring that each 
#' facet is relatively squared}
#' \item{if a caption or a subtitle is specified}
#' \item{if a legend is set at the bottom or the top
#' of the plot}
#' }
#' \item{increasing the figure width if a legend
#' is set at the left or the right of the plot}
#' }
#' }
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
#' @param y Character vector or factor with elements in the y-axis.
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
	title = NULL, 
	subtitle = NULL,
	caption = NULL,
	xLab = NULL,
	facet = FALSE,
	includeLegend = TRUE, 
	legendPosition = "right",
	y = NULL){
	
	isWidthSpec <- !is.null(width)
	isHeightSpec <- !is.null(height)

	widthDef <- 800
	
	# if y is specified, the height is based
	# on number of lines
	if(!is.null(y)){
		if(is.factor(y)){
			yUnique <- levels(y)
		}else	yUnique <- unique(y)
		nLinesY <- countNLines(yUnique)
		nLinesY <- sum(nLinesY)
		heightDef <- sum(nLinesY) * 20
	# otherwise set default height
	}else	heightDef <- 500

	# extract layout (in case facetting)
	if(!is.null(gg)){
		plotDim <- getDimGgplot(gg = gg)
		nrow <- plotDim[["nrow"]]
		ncol <- plotDim[["ncol"]]
	}
	
	if(!isWidthSpec)
		width <- widthDef
	
	if(!isHeightSpec){
		
		if(nrow == 1){
			height <- heightDef
		}else{
			plotSize <- width/ncol
			height <- plotSize * nrow
		}
	
		# add space for margins
		margins <- getMargins(
			title = title, subtitle = subtitle,
			xLab = xLab, caption = caption, 
			facet = facet,
			includeLegend = includeLegend,
			legendPosition = legendPosition
		)
		height <- height + margins$t + margins$b
		
	}
	
	dim <- c(width = width, height = height)
	
	return(dim)
	
}

#' Get margins for a clinical data plot
#' @inheritParams getSizePlotClinData
#' @return List with margins
#' @author Laure Cougnaud
#' @export
getMargins <- function(
	title = NULL, subtitle = NULL,
	xLab = NULL, caption = NULL, 
	facet = FALSE,
	includeLegend = TRUE,
	legendPosition = "right"
	){
	
	topMargin <- 30
	bottomMargin <- 20
	
	if(!is.null(title))
		topMargin <- topMargin + 20
	
	if(!is.null(subtitle))
		topMargin <- topMargin + getHeightSubtitle(subtitle)
	
	if(!is.null(xLab))
		bottomMargin <- bottomMargin + 20
	
	if(!is.null(caption))
		bottomMargin <- bottomMargin + getHeightCaption(caption)
	
	if(facet)
		topMargin <- topMargin + 20
		
	if(includeLegend){
		switch(legendPosition,
			`top` = {
				topMargin <- topMargin + 20
			},
			`bottom` = {
				bottomMargin <- bottomMargin + 20
			}
		)
	}
	
	return(list(t = topMargin, b = bottomMargin))
	
}

#' Get height of subtitle
#' @inheritParams clinDataReview-common-args
#' @return Integer with height in pixels 
#' for this element.
#' @author Laure Cougnaud
getHeightSubtitle <- function(subtitle){
	
	nLines <- countNLines(subtitle)
	height <- 15*nLines
	return(height)
	
}

#' Get height of caption
#' @inheritParams clinDataReview-common-args
#' @inherit getHeightSubtitle return
#' @author Laure Cougnaud
getHeightCaption <- function(caption){
	
	nLines <- countNLines(caption)
	height <- 20*nLines
	return(height)
	
}
