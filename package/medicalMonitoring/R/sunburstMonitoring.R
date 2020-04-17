#' Sunburst interactive plot.
#' 
#' Note: the table and plot are not (yet) linked.
#' @inheritDotParams plotCountMonitoring -typePlot
#' @inherit plotCountMonitoring return
#' @example inst/examples/sunburstMonitoring-example.R
#' @family visualizations of summary statistics for medical monitoring
#' @author Laure Cougnaud
#' @export
sunburstMonitoring <- function(...){

	plotCountMonitoring(..., typePlot = "sunburst")
	
}