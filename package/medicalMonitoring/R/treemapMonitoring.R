#' Treemap interactive plot.
#' 
#' Note: the table and plot are not (yet) linked.
#' @inheritDotParams plotCountMonitoring -typePlot
#' @inherit plotCountMonitoring return
#' @example inst/examples/treemapMonitoring-example.R
#' @family visualizations of summary statistics for medical monitoring
#' @author Laure Cougnaud
#' @export
treemapMonitoring <- function(...){
	
	plotCountMonitoring(..., typePlot = "treemap")
	
}