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
	
		# bottom margin is 80 px by default in plotly
		bottomMargin <- 80 + ifelse(legend & (legendPosition == "bottom"), 20, 0)
		
		pl <- layout(
			p = pl,
			annotations = list(
				list(
					x = 1, y = 0, text = caption, 
					showarrow = FALSE, 
					xref = 'paper', yref = 'paper', 
					xanchor = 'right', yanchor = 'top', 
					xshift = 0, yshift = -bottomMargin,
					font = list(size = 12)
				)
			),
			margin = list(b = bottomMargin + 20) # 80 px by default
		)
		
	}
	
	if(!is.null(subtitle)){
		
		# top margin is 100 px by default in plotly
		topMargin <- 100 + ifelse(legend & (legendPosition == "top"), 20, 0)
		
		pl <- layout(
			p = pl,
			annotations = list(
				list(
					x = 0.5, y = 1, text = subtitle, 
					showarrow = FALSE, 
					xref = 'paper', yref = 'paper', 
					xanchor = 'center', yanchor = 'bottom', 
					xshift = 0, yshift = 0,
					font = list(size = 12)
				)
			),
			margin = list(t = topMargin + 20),
			# set title at the top of the top margin 
			# (vertical center by default)
			# otherwise might overlap with subtitle
			title = list(
				yref = "container", 
				y = 1, yanchor = "top",
				pad = list(t = 10)
			)
		)
			
	}
		
	return(pl)
	
}