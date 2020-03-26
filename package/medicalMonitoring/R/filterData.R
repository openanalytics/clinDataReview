#' Filter dataset based on specified filters.
#' 
#' **Note that by default, missing values in the filtering variable are retained
#' (which differs from the default behaviour in R)**.
#' To filter missing records, please use the \code{keepNA} parameter.
#' @param data Data.frame with data.
#' @param filters Unique filter or list of filters.
#' Each filter should be a list containing:
#' \itemize{
#' \item{'var': }{String with variable from \code{data} to filter on.}
#' \item{'value': }{Character vector with values from \code{var} to consider.}
#' \item{'op': }{(optional) String with operator used to retain records from \code{value}.
#' If not specified, the inclusion operator: '\%in\%' is considered, a.k.a
#' records with \code{var} in \code{value} are retained.}
#' \item{'rev': }{(optional) Logical, if TRUE (FALSE by default), records NOT fulfilling
#' the specified condition are retained.}
#' \item{'keepNA': }{(optional) Logical, if TRUE (by default), missing values in \code{var}
#' are retained. If not specified, \code{keepNA} general parameter is used.}
#' }
#' If a list of filters is specified, the logical operator (see \code{\link[base]{Logic}})
#' linking the different conditions
#' can be specified between the two conditions, e.g.: 
#' \code{list(list(var = "SEX", value = "F"), "&", list(var = "COUNTRY", value = "DEU"))}.
#' @param verbose Logical, if TRUE (FALSE by default) progress messages are printed
#' in the current console.
#' @inheritParams filterDataSingle
#' @return Filtered \code{data}.
#' @example inst/examples/filterData-example.R
#' @author Laure Cougnaud
#' @export
filterData <- function(
	data, 
	filters, 
	keepNA = TRUE, returnAll = FALSE,
	verbose = FALSE,
	labelVars = NULL){
	
	# if multiple filters are specified:
	# data is successively filtered
	if(any(sapply(filters, is.list))){
		
		iPar <- 1
		while(iPar <= length(filters)){	
			
			filterCur <- filters[[iPar]]
			
			# if a condition is specified (not a logical operator):
			if(is.list(filterCur)){
				
				# extract filtered data
				dataCur <- filterDataSingle(
					data = data, filters = filterCur, 
					keepNA = keepNA,
					returnAll = TRUE, labelVars = labelVars
				)
				
				# extract operator, 'AND' if not specified:
				if(iPar > 1){
					
					if(!is.list(filters[[iPar - 1]])){
						op <- filters[[iPar - 1]]
					}else	op <- "&"
					
					# combine previous and current condition
					dataCur$keep <- match.fun(op)(data$keep, dataCur$keep)
					msg <- paste(msg, op, attr(dataCur, "msg"))
					
				}else{
					
					msg <- attr(dataCur, "msg")
					
				}
				
				# save it to the dataset
				data <- dataCur
				
			}
			iPar <- iPar + 1
		}
		msg <- paste(msg, "are retained.")
		
		# only keep filtered rows if 'returnAll' is FALSE
		if(!returnAll)
			data <- data[which(data$keep), setdiff(colnames(data), "keep")]
		
		res <- data
	
	}else{
		
		res <- filterDataSingle(
			data = data,
			filters = filters, 
			keepNA = keepNA,
			returnAll = returnAll,
			labelVars = labelVars
		)
		msg <- attr(res, "msg")
			
	}
	
	attr(res, "msg") <- msg
	if(verbose)	message(msg)
	
	return(res)
	
}
	
#' Filter data for a single filter
#' @param filters Unique filter or list of filters.
#' @param keepNA Logical, if TRUE (by default) missing values in \code{var} are retained.
#' If set to FALSE, missing values are ignored for all filters.
#' The specification within \code{filters} prevails on this parameter.
#' @param returnAll Logical:
#' \itemize{
#' \item{if FALSE (by default): }{the \code{data} for only the filtered records
#' is returned.}
#' \item{if TRUE: }{the full \code{data} is returned, with the extra column: 'keep',
#' containing 'TRUE' if the record fulfill all conditions, FALSE otherwise}
#' } 
#' @inheritParams medicalMonitoring-common-args
#' @return Updated \code{data}.
#' @author Laure Cougnaud
filterDataSingle <- function(data,
	filters, 
	keepNA = TRUE,
	returnAll = FALSE,
	labelVars = NULL){
		
	# variable used to filter:
	var <- filters$var
	varST <- paste0(getLabelVar(var = var, data = dataAnnot, labelVars = labelVars), " (", sQuote(var), ")")
	if(!var %in% colnames(data)){
		warning(paste("Data is not filtered based on the variable:", varST,
			"because", varST, "is not available in the input data."))
		return(data)
	}
		
	# value used to filter
	value <- filters$value
	valueST <- toString(sQuote(value))
	
	# operand: '%in%' by default
	op <- filters$op
	if(is.null(op))	op <- '%in%'
	
	# inversion:
	rev <- filters$rev
	if(is.null(rev))	rev <- FALSE
	
	# extract records matching condition
	isKept <- match.fun(op)(x = data[, var], table = value)
	if(rev)	isKept <- !isKept
	
	# keep missing values?
	keepNAFilter <- filters$keepNA
	if(is.null(keepNAFilter))	keepNAFilter <- keepNA
	isNA <- is.na(data[, var])
	isKept[isNA] <- (keepNAFilter)
	
	if(!any(isKept))
		warning("No data is retained based on the filtering on the variable")
	
	# store all steps in a message string
	msgNA <- paste(sum(isNA), "records with missing", varST)
	msg <- paste0(
		sum(!isKept), " records with ", varST, 
		if(rev)	" not", " ",
		op, " ", valueST,
		if(sum(isNA) > 0 & all(!is.na(value)))
			ifelse(keepNAFilter, 
				paste("(", msgNA, " are retained)"), 
				paste0(" (including ", msgNA, ")")
			)
	)
	
	res <- if(returnAll){
		data$keep <- isKept
		data
	}else{
		# filter data
		data <- data[which(isKept), ]
	}

	attr(data, "msg") <- msg
	
	return(data)
	
}