#' Add labels, i.e. caption of subtitle to a plotly object
#' @param pl a \code{\link[plotly]{plotly}} object
#' @param caption String with caption.
#' @param subtitle String with subtitle
#' @inheritParams getSizePlotClinData
#' @return The updated \code{\link[plotly]{plotly}} object
#' with a caption included
#' @importFrom plotly layout
#' @author Laure Cougnaud
addLabsToPlotly <- function(
	pl, 
	caption = NULL, 
	subtitle = NULL,
	legend = FALSE, 
	legendPosition = "right"){
	
	if(!is.null(caption)){
	
		# bottom margin seems to be 50 px by default (?) in plotly
		bottomMargin <- 50 + ifelse(legend & (legendPosition == "bottom"), 10, 0)
		bottomMarginCaption <- getHeightLabs(caption)
		
		pl <- layout(
			p = pl,
			annotations = list(
				list(
					x = 1, y = 0, text = caption, 
					showarrow = FALSE, 
					xref = 'paper', yref = 'paper', 
					xanchor = 'right', yanchor = 'top', 
					# position: below the x-axis title and legend
					xshift = 0, yshift = -bottomMargin,
					font = list(size = 12)#,
#					pad = list(t = 10, b = 10) # in px
				)
			),
			margin = list(b = bottomMargin + bottomMarginCaption + 10), # px
			# fix the distance between axis labels and title text
			# otherwise title is centered vertically in the margin
			xaxis = list(title = list(standoff = 10)) # px
		)
		
	}
	
	if(!is.null(subtitle)){
		
		# top margin seems to be 50 px by default (?) in plotly
		topMargin <- 50 + ifelse(legend & (legendPosition == "top"), 10, 0)
		topMarginSubtitle <- getHeightLabs(subtitle)
		
		pl <- layout(
			p = pl,
			annotations = list(
				list(
					x = 0, y = 1, text = subtitle, 
					align = "left",
					showarrow = FALSE, 
					xref = 'paper', yref = 'paper', 
					xanchor = 'left', yanchor = 'bottom', 
					xshift = 0, yshift = 0,
					font = list(size = 12),
					pad = list(b = 10) # in px
				)
			),
			margin = list(t = topMargin + topMarginSubtitle),
			# fix title at the top of the top margin 
			# (vertical center by default)
			# otherwise might overlap with subtitle
			title = list(
				yref = "container", 
				y = 1, yanchor = "top",
				pad = list(t = 10) # in px
			)
		)
			
	}
		
	return(pl)
	
}