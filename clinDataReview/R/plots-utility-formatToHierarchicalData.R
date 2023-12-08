#' Format data to a hierarchical data, in the format
#' as required by the plotly sunburst and treemap.
#' 
#' Note that new variables are created for each variable of interest
#' (the variables are not overwritten) to avoid
#' issues with cases where the value in the child 
#' and parent variables are the same.
#' @inheritParams clinDataReview-common-args
#' @inheritParams clinDataReview-common-args-summaryStatsVis
#' @return Updated data.frame with \code{vars} in 
#' hierarchical format, with extra attributes (in 'metadat'):
#' \itemize{
#' \item 'varID':  String with column of output
#' containing ID of specific element.\cr
#' This is a combination from the specified \code{vars},
#' or 'Overall' for the grand total.
#' \item 'varParent': String with column of output
#' containing ID of the parent element
#' \item 'varLabel': String with column of output
#' containing the label to display.\cr
#' This is usually the name of the child element.
#' }
#' @importFrom utils head tail capture.output
#' @importFrom plyr colwise
#' @author Laure Cougnaud
formatToHierarchicalData <- function(data, vars, valueVar){
		
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
	data[[varID]] <- apply(dataVars, 1, function(x){
		if(all(x == "Total")){
			"Overall"
		}else{
			paste(x[which(x != "Total")], collapse = "-")
		}
	})
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
				idxColTotalParent <- max(min(which(x == "Total")-1), 1)
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
		isGrandTotal <- (rowSums(dataVars == "Total") == length(vars))
		hasNoParent <- is.na(idParent) | isGrandTotal
		
		if(any(hasNoParent)){
			idxColNoParent <- which(data[hasNoParent, vars] != "Total", arr.ind = TRUE)[, "col"]
			if(any(idxColNoParent != 1))
				stop("Missing parent value")
			idParent[hasNoParent] <- ""
		}
		
		data[[varParent]] <- idParent

	}else	varParent <- NULL

	## Build labels: 
	varLabel <- "hierarLabel"
	data[[varLabel]] <- apply(data, 1, function(dataRow){
		if(all(dataRow[vars] == "Total")){
			labelFromVars <- "Overall"
		}else{
			# extract label for sector:
			idxNotTotal <- which(dataRow[vars] != "Total")
			dataRowVars <- dataRow[vars][tail(idxNotTotal, 1)]
			labelFromVars <- paste(dataRowVars, collapse = "-")
		}
		# add value to have also values displayed for higher-level sectors
		labelFromValue <- dataRow[valueVar]
		paste(labelFromVars, labelFromValue, sep = ": ")
	})

	attr(data, "metadata") <- list(
		varID = varID, 
		varParent = varParent, 
		varLabel = varLabel
	)
	
	return(data)
	
}