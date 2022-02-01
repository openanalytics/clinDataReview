#' Set layout for a clinical data plot.
#' @param facet Logical (FALSE by default), 
#' does the plot contains facets?
#' @param ... Any parameters for the \code{\link[plotly]{layout}}
#' function. This should contain at least the plot object.
#' @inheritParams clinDataReview-common-args
#' @inheritParams getSizePlot
#' @return The updated \code{plotly} object
#' @importFrom plotly layout
#' @author Laure Cougnaud
layoutClinData <- function(
	xLab = NULL,
	yLab = NULL, 
	title = NULL,
	caption = NULL, 
	subtitle = NULL,
	includeLegend = FALSE, 
	legendPosition = "right",
	facet = FALSE,
	nrow = 1L, ncol = 1L,
	width, height,
	...){

	args <- list(...)
	
	# get margins
	sizeDetails <- getPositionAndMargins(
		title = title, subtitle = subtitle,
		xLab = xLab, caption = caption, 
		facet = facet,
		includeLegend = includeLegend,
		legendPosition = legendPosition
	)
	margins <- sizeDetails[["margin"]]
	
	# height of the plotting region
	# used to set position in normalized coordinates
	heightPlot <- height-margins$b-margins$t 
	
	positions <- sizeDetails[["position"]]
	
	if(!is.null(title)){
		
		args$title$text <- title
		
		args$title$xref <- "container"
		args$title$x <- 0.5 # default
		args$title$xanchor <- "center"
		
		# fix title at the top of the top margin 
		# (vertical center by default)
		# otherwise might overlap with subtitle or top legend
		args$title$yref <- "container"
		args$title$y <- 1
		args$title$yanchor <- "top"
		args$title$pad <- list(t = 10)
		
	}
	
	if(!is.null(caption)){
		
		# Option 1: in case of long or rotated x-axis labels
		# include caption in title of the x-axis
		# to have automated position of the caption
		# in case the labels of the x-axis are rotated
#		caption <- paste0("<i>", caption, "</i>")
#		xLab <- paste(c(xLab, caption), collapse = "\n\n")

		# Option 2:
		# works for treemap/sunburst (which don't have x-axis)
		# works for facetted boxplot
		args$annotations <- c(args$annotations,
			list(list(
				x = 1, y = 0, text = caption, 
				showarrow = FALSE, 
				xref = 'paper', yref = 'paper', 
				xanchor = 'right', yanchor = 'top', 
				xshift = 0, yshift = -positions$caption,
				font = list(size = 12)
			))
		)
		
	}
	
	if(!is.null(subtitle)){
		
		args$annotations <- c(args$annotations,
			list(list(
				x = 0, y = 1, text = subtitle, 
				align = "left",
				showarrow = FALSE, 
				xref = 'paper', yref = 'paper', 
				xanchor = 'left', yanchor = 'bottom', 
				xshift = 0, yshift = positions$subtitle,
				font = list(size = 12)
			))
		)
			
	}
	
	if(!is.null(xLab)){
		
		# label for the x-axis already set in facetted plot
		if(!facet)
			args$xaxis$title$text <- xLab
		
		# standoff: distance between axis text and title
		# adjusted such as it does not overlap with caption or bottom legend
		# (standoff + automargin on: margins are pushed to fit the axis title at given standoff distance)
		args$xaxis$title$standoff <- 0
		
	}
	
	if(!is.null(yLab)){
		# label for the y-axis already set in facetted plot
		if(!facet)
			args$yaxis$title$text <- yLab
	}
	
	# set margins
	args$margin <- margins
	# margins are expanded if labels of the x-axis are too long
	args$xaxis$automargin  <- TRUE
	
	if(includeLegend){
		
		# fix for legend
		# 'legend.position' not supported in ggplotly
		# Note: in case legend position is left or top, big legend might overlap the plot
		if(legendPosition == "none"){
			
			args$showlegend <- FALSE
			
		}else{
			
			# print legend even if only one y-element
			args$showlegend <- TRUE 
			
			# legend position is in normalized coordinates
			# to the plot region (without margins)
			argsLegend <- switch(legendPosition,
				`top` = {
					legendY <- 1+positions$legend/heightPlot
					list(
						orientation = "h", 
						x = 0.5, xanchor = "center",
						y = 1, yanchor = "bottom"
					)
					
				},
				`bottom` = {
					# should be [-2, 3]
					legendY <- -min(positions$legend/heightPlot, 2)
					# It is important to define the bottom legend
					# with 'top' anchor, otherwise the legend
					# might overlap with the plot if it contains multiple rows.
					list(
						orientation = "h", 
						x = 0.5, xanchor = "center",
						y = legendY, #-0.1/nrow
						yanchor = "top"
					)
				},
				`left` = {
					list(
						orientation = "v", 
						x = -0.1/width, xanchor = "right",
						y = 0.5, yanchor = "top"
					)
				},
				`right` = {
					list(
						orientation = "v", 
						x = 1, xanchor = "left",
						y = 0.5, yanchor = "top"
					)
				}
			)
			args$legend[names(argsLegend)] <- argsLegend

		}
		
	}
	
	pl <- do.call(layout, args)
		
	return(pl)
	
}