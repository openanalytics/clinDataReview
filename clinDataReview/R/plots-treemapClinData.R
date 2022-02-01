#' Treemap interactive plot.
#' 
#' Note: the table and plot are not (yet) linked.
#' @inheritDotParams plotCountClinData -typePlot
#' @inherit plotCountClinData return
#' @example inst/examples/treemapClinData-example.R
#' @family visualizations of summary statistics for clinical data
#' @author Laure Cougnaud
#' @export
treemapClinData <- function(...){
	
	plotCountClinData(..., typePlot = "treemap")
	
}