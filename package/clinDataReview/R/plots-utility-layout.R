#' Set layout for a clinical data plot.
#' @param pl a \code{\link[plotly]{plotly}} object
#' @param facet Logical (FALSE by default), 
#' does the plot contains facets?
#' @param ... Any parameters for the \link{code[plotly]{layout}}
#' function. This should contain at least the plot object.
#' @inheritParams getSizePlotClinData
#' @return The updated \code{\link[plotly]{plotly}} object
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
	margins <- getMargins(
		title = title, subtitle = subtitle,
		xLab = xLab, caption = caption, 
		facet = facet,
		includeLegend = includeLegend,
		legendPosition = legendPosition
	)
	# height of the plotting region
	# used to set position in normalized coordinates
	heightPlot <- height-margins$b-margins$t 
	
	if(!is.null(title)){
		args$title$text <- title
		args$title$x <- 0.5
		args$title$xanchor <- "center"
	}
	
	if(!is.null(caption)){
		
		# include caption in title of the x-axis
		# to have automated position of the caption
		# in case the labels of the x-axis are rotated
		caption <- paste0("<i>", caption, "</i>")
		xLab <- paste(c(xLab, caption), collapse = "\n\n")

#		pl <- layout(
#			p = pl,
#			annotations = list(
#				list(
#					x = 1, y = 0, text = caption, 
#					showarrow = FALSE, 
#					xref = 'paper', yref = 'paper', 
#					xanchor = 'right', yanchor = 'top', 
#					# position: below the x-axis title and legend
#					xshift = 0, yshift = -bottomMargin,
#					font = list(size = 12)
#				)
#			),
#			margin = list(b = bottomMargin + bottomMarginCaption + 10), # px
#			# fix the distance between axis labels and title text
#			# otherwise title is centered vertically in the margin
#			xaxis = list(title = list(standoff = 10)) # px
#		)
		
	}
	
	if(!is.null(subtitle)){
		
		args$annotations <- c(args$annotations,
			list(
				x = 0, y = 1, text = subtitle, 
				align = "left",
				showarrow = FALSE, 
				xref = 'paper', yref = 'paper', 
				xanchor = 'left', yanchor = 'bottom', 
				xshift = 0, 
				# position for a ggplot2 facet plot is from
				# the bottom (inside) of the facet label
				yshift = ifelse(facet, 25, 0),
				font = list(size = 12)
			)
		)
		
		# fix title at the top of the top margin 
		# (vertical center by default)
		# otherwise might overlap with subtitle
		args$title$yref <- "container"
		args$title$y <- 1
		args$title$yanchor <- "top"
		args$title$pad <- list(t = 10)
			
	}
	
	if(!is.null(xLab)){
		args$xaxis$title$text <- xLab
		# standoff: distance between axis text and title
		# adjusted to not have overlapping legend for bottom legend
		# (standoff + automargin on: margins are pushed to fit the axis title at given standoff distance)
		args$xaxis$title$standoff <- 0
	}
	
	if(!is.null(yLab))
		args$yaxis$title$text <- yLab
	
	# set margins
	args$margin <- margins
	# margins are expanded if labels of the x-axis are too long
	args$xaxis$automargin  <- TRUE
	
	if(includeLegend){
		
		# fix for legend
		# 'legend.position' not supported in ggplotly
		# Note: in case legend position is left or top, big legend might overlap the plot
		if(legendPosition == "none"){
			
			args$legend$showlegend <- FALSE
			
		}else{
			
			# legend position is in normalized coordinates
			# to the plot region (without margins)
			legend <- switch(legendPosition,
				`top` = {
					list(
						orientation = "h", 
						x = 0.5, xanchor = "center",
						y = 1, yanchor = "bottom"
					)
					
				},
				`bottom` = {
					heightPlot <- height-margins$b-margins$t 
					legendY <- -(margins$b-20)/heightPlot
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
			args$legend <- legend		

		}
		
	}
	
	pl <- do.call(layout, args)
		
	return(pl)
	
}