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
	legend = FALSE, 
	legendPosition = "right",
	facet = FALSE,
	nrow = 1L, ncol = 1L,
	width, height,
	...){

	args <- list(...)
	
	if(!is.null(caption)){
		
		# include caption in title of the x-axis
		# to have automated position of the caption
		# in case the labels of the x-axis are rotated
		caption <- paste0("<i>", caption, "</i>")
		xLab <- paste(c(xLab, caption), collapse = "\n\n")
	
		# bottom margin seems to be 50 px by default (?) in plotly
		bottomMargin <- 50 + ifelse(legend & (legendPosition == "bottom"), 10, 0)
		bottomMarginCaption <- getHeightCaption(caption)		
		args$margin$b <- bottomMargin + bottomMarginCaption + 10
		
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
		
		# top margin seems to be 50 px by default (?) in plotly
		topMargin <- 50 + 
			ifelse(legend & (legendPosition == "top"), 10, 0) +
			ifelse(facet, 20, 0)
		topMarginSubtitle <- getHeightSubtitle(subtitle)
		
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
	
		args$margin$t <- topMargin + topMarginSubtitle
		
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
		args$xaxis$yanchor <- "top"
	}
	
	if(!is.null(yLab))
		args$yaxis$title$text <- yLab
	
	if(!is.null(title)){
		args$title$text <- title
		args$title$x <- 0.5
		args$title$xanchor <- "center"
	}
	
	if(legend && legendPosition == "bottom"){
		
		# fix for legend
		# 'legend.position' not supported in ggplotly
		# Note: in case legend position is left or top, big legend might overlap the plot
		if(legendPosition == "none"){
			
			args$legend$showlegend <- FALSE
			
		}else{
			
			# legend position is in normalized coordinates
			legOrient <- ifelse(legendPosition %in% c("top", "bottom"), "h", "v")
			legY <- c(top = 1, bottom = -0.1/nrow, right = 0.5, left = 0.5)[legendPosition]
			legYAnchor <- c(top = "bottom", bottom = "top", right = "top", left = "top")[legendPosition]
			legX <- c(top = 0.5, bottom = 0.5, right = 1, left = -0.1/width)[legendPosition]
			legXAnchor <- c(top = "center", bottom = "center", right = "left", left = "right")[legendPosition]
			args$legend <- list(
				orientation = legOrient, 
				x = legX, xanchor = legXAnchor,
				y = legY, yanchor = legYAnchor
			)
		}
		
	}
	
	pl <- do.call(layout, args)
		
	return(pl)
	
}