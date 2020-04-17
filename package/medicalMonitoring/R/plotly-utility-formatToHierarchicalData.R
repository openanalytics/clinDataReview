#' Format data to a hierarchical data, in the format
#' as required by the plotly sunburst and treemap.
#' 
#' Note that new variables are created for each variable of interest
#' (the variables are not overwritten) to avoid
#' issues with cases where the value in the child 
#' and parent variables are the same.
#' @param vars Character vector with variable(s) of interest.
#' @inheritParams medicalMonitoring-common-args
#' @return Updated data.frame with \code{vars} in 
#' hierarchical format, with extra attributes (in 'metadat'):
#' \itemize{
#' \item{'varsPlotly': }{Named character vector with
#' new variable names (\code{[var].hier}) names
#' with original variable names
#' }
#' \item{'varLabel': }{String with name
#' of new column containing text used for
#' label in plotly.}
#' }
#' @importFrom utils head tail capture.output
#' @author Laure Cougnaud
formatToHierarchicalData <- function(data, vars){
		
	dataVars <- data[, vars, drop = FALSE]
	
	## Build IDs: combine values of child + parents if any
	varID <- "hierarID"
	data[[varID]] <- apply(dataVars, 1, function(x) 
		paste(x[which(x != "Total")], collapse = "-")
	)
	if(any(duplicated(data[[varID]])))
		stop("Duplicated records when combining: ", 
			toString(sQuote(vars)), ".")

	## Build parent: only store direct parent
	if(length(vars) > 1){
		
		varParent <- "hierarParent"
		
		data[[varParent]] <- apply(dataVars, 1, function(x){
#			print(x)
			colNotTotal <- which(x != "Total")
			colParents <- head(colNotTotal, -1) # remove last var
			colFirstParent <- tail(colParents, 1)
			if(length(colFirstParent) == 0){
				""
			}else{
				# original value in data
				value <- as.character(x[colFirstParent]) 
				# extract reformatted value in data:
				# 1) rows with var == value
				idxFirstParent <- which(dataVars[, colFirstParent] == value) 
				# 2) and with total
				nextVars <- vars[seq(from = colFirstParent + 1, to = length(vars))]
				dataNextVars <- dataVars[, nextVars, drop = FALSE]
				idxNextVarsTotal <- which(rowSums(dataNextVars == "Total") == length(nextVars))
				idxFirstParentTotal <- intersect(idxFirstParent, idxNextVarsTotal)
				if(length(idxFirstParentTotal) != 1)
					stop("Missing parent value")
				data[idxFirstParentTotal, varID]
			}
		})

	}else	varParent <- NULL

	## Build labels: 
	varLabel <- "hierarLabel"
	data[[varLabel]] <- apply(dataVars, 1, function(x){
		idxNotTotal <- which(x != "Total")
		paste(x[tail(idxNotTotal, 1)], collapse = "-")
	})

#	head(data[, c(vars, varID, varParent, varLabel)])

	attr(data, "metadata") <- list(varID = varID, varParent = varParent, varLabel = varLabel)
	
	return(data)
	
}