#' Set different variables for the x-axis labels
#' @param var String with variable displayed
#' in the axis.
#' @param labVars Character vector with variable(s) to be displayed 
#' as the labels of the ticks in the axis.
#' @inheritParams clinDataReview-common-args
#' @return Named character vector.
#' The names are the position in the x-axis,
#' the values are the new labels.
#' @author Laure Cougnaud
getAxisLabs <- function(data, var, labVars){
	
	dataAxisLabs <- data[do.call(order, data[, labVars, drop = FALSE]), ]
	axisLabs <- by(
		data = dataAxisLabs,
		# for each element in the x-axis...
		INDICES = dataAxisLabs[, var], 
		FUN = function(dataCol){
			# ... extract unique elements for specified variables
			cols <- lapply(
				dataCol[, labVars, drop = FALSE], 
				function(x) paste(unique(x), collapse = "\n")
			)
			# and combine different variables
			Reduce(function(...) paste(..., sep = "\n"), cols)
		},
		simplify = TRUE
	)
	axisLabs <- c(axisLabs)
	
	return(axisLabs)
	
}
