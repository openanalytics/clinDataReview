#' Format data to a hierarchical data, in the format
#' as required by the plotly sunburst and treemap.
#' 
#' Note that new variables are created for each variable of interest
#' (the variables are not overwritten) to avoid
#' issues with cases where the value in the child 
#' and parent variables are the same.
#' @param vars Character vector with variable(s) of interest.
#' @inheritParams clinDataReview-common-args
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
#' @importFrom plyr colwise
#' @author Laure Cougnaud
formatToHierarchicalData <- function(data, vars){
		
	# filter missing records for 'vars'
	idxAllNa <- which(rowSums(is.na(data[, vars, drop = FALSE])) == length(vars))
	if(length(idxAllNa) > 0){
		warning(length(idxAllNa), " record(s) are filtered from the data",
			" because contain missing for all variable(s): ",
			toString(shQuote(vars)), "."
		)
		data <- data[-idxAllNa, ]
	}
	
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
		
		## extract ID of parent
		
		# create dataset with 'Total' for the parent variables
		
		# extract, for each record (row) the indices
		# of the columns which should be replaced to 'Total'
		getIdxCol <- function(x){
			idxColVars <- if(!any(x == "Total")){
				length(vars)
			}else{
				idxColTotalParent <- min(which(x == "Total")-1)
				seq(from = idxColTotalParent, to = length(vars))
			}
		}
		idParentColToSet <- apply(dataVars, 1, FUN = getIdxCol)
		
		# build matrix with row/colum indices:
		idxParentList <- lapply(seq_along(idParentColToSet), function(iRow)
			cbind(row = iRow, col = idParentColToSet[[iRow]])
		)
		idxParent <- do.call(rbind, idxParentList)
		
		# convert factor -> character otherwise issue when
		#  including 'Total' in the column if not available in factor levels
		dataParent <- colwise(as.character)(dataVars)
		# replace with 'Total'
		dataParent[idxParent] <- "Total" 
		
		# build label for the parent
		parent <- do.call(interaction, dataParent) 
		
		# build label for the record
		idRecord <- interaction(dataVars)

		# extract parent ID of each record
		idParent <- data[match(parent, idRecord), varID]
		
		# the parent should be available for all records
		#  excepted for higher category
		isNoParent <- is.na(idParent)
		if(any(isNoParent)){
			idxColNoParent <- which(data[isNoParent, vars] != "Total", arr.ind = TRUE)[, "col"]
			if(any(idxColNoParent != 1))
				stop("Missing parent value")
			idParent[isNoParent] <- ""
		}
		
		data[[varParent]] <- idParent

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