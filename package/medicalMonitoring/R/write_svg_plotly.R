#' Export a \code{\link[plotly]{plotly}} object to a svg file.
#' 
#' This function is not intended to be used directly, but 
#' within the package unit tests,
#' and be passed to the \code{writer} parameter of the
#' \code{\link[vdiffr]{expect_doppelganger}} function.
#' @param plot \code{\link[plotly]{plotly}}
#' @param file String with path to file where the figure should be exported.
#' @param title (optional) String with figure title,
#' this will be added to the current plot title (if present), 
#' e.g. to label a specific test.
#' @references Similar functionality is available in the plotly package 
#' in the 'tests': 'write_plotly_svg', but not exported (github issue #1703).
#' @return No returned value, the \code{plot} is saved to the
#' \code{file}.
#' @author Laure Cougnaud
#' @importFrom plotly plotly_build layout orca
#' @export
write_svg_plotly <- function(plot, file, title = ""){
	
	set.seed(555)
	
	if(title != ""){

		# title seems to be stored in different elements of the 'layout' output
		plotTitle <- plotly_build(plot)$x$layout$title # for scatterplot
		if(is.list(plotTitle))
			plotTitle <- plotTitle$text # for sunburst
		
		title <- paste(c(plotTitle, title), collapse = "<br>")
		plot <- plot %>% layout(title = title)
	}
	
	# orca cannot save plot in a different directory, 
	# so change wd to plot export directrory
	oldwd <- getwd()
	dirTest <- dirname(file)
	if(!dir.exists(dirTest))	dir.create(dirTest, recursive = TRUE)
	setwd(dirTest)
	
	# export plot
	plotly::orca(p = plot, file = basename(file))
	
	# set back to initial wd
	setwd(oldwd)
	
}
