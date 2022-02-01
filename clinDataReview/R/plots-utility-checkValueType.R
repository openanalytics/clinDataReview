#' Check if the specified \code{valueType} parameter can be passed to the
#' \code{branchvalues} of the \code{\link[plotly]{plot_ly}} treemap/sunburst visualizations.
#' 
#' If this parameter is set to 'total' and the sum of the counts of the
#' the children nodes is not bigger than the parent node,
#' an empty plot is created. In this case, this function
#' set this parameter to: 'relative'.
#' @inheritParams clinDataReview-common-args-summaryStatsVis
#' @inheritParams clinDataReview-common-args
#' @return If the condition is fullfilled:
#' updated \code{valueType} and warning;
#' otherwise input \code{valueType}.
#' @importFrom utils head
#' @author Laure Cougnaud
checkValueType <- function(data, 
	vars, 
	valueVar,
	valueType = "total",
	labelVars = NULL){
	
	if(!is.numeric(data[, valueVar]))
		stop(paste("'valueVar':", valueVar, "should be numeric."))

	if(length(vars) > 1 & valueType == "total"){
	
		parentVars <- head(vars, -1)
		
		for(parentVar in parentVars){
	
			childVar <- vars[match(parentVar, vars)+1]
			groupChildBiggerParent <- sapply(unique(data[, parentVar]), function(group){
						
				idxParent <- which(data[, childVar] == group)
				if(length(idxParent) > 0){
					nParent <- sum(data[idxParent, valueVar])
					idxChild <- which(data[, parentVar] == group)
					nChild <- sum(data[idxChild, valueVar])
					(nChild > nParent)
				}else	FALSE
				
			})
	
			if(any(groupChildBiggerParent)){
				warning("Parent node(s):\n",
					toString(names(which(groupChildBiggerParent))), 
					"\nare smaller than the sum of their children, ",
					"so 'valueType' is set to 'relative' (instead of 'total')."
				)
				valueType <- "relative"
			}
		
		}
		
	}
	
	return(valueType)
	
}