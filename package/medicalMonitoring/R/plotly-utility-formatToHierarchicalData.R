#' Format data to a hierarchical data, in the format
#' as required by the plotly sunburst and treemap.
#' @param vars Character vector with variable(s) of interest.
#' @inheritParams medicalMonitoring-common-args
#' @return Updated data.frame with \code{vars} in 
#' hierarchical format.
#' @importFrom utils head tail
#' @author Laure Cougnaud
formatToHierarchicalData <- function(data, vars){
		
	if(length(vars) > 1){
	
		# Reformat summary statistics data.frame to hierarchical data required by treemap
		for(varChild in tail(vars, -1)){
			varParent <- vars[match(varChild, vars)-1]
			data[varChild] = ifelse(
				data[, varChild] == "Total", 
				as.character(data[, varParent]), 
				as.character(data[, varChild])
			)  
		}
		varHeadParent <- head(vars, 1)
		data[varHeadParent] = ifelse(
			data[, varHeadParent] == data[, vars[2]], 
			'Overall', 
			as.character(data[, varHeadParent])
		)
		
	}
	
	return(data)
	
}