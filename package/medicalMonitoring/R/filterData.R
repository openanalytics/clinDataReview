#' Filter dataset based on specified filters.
#' 
#' By default, missing values in filtering variable are retained,
#' see \code{keepNA} parameter.
#' @param data Data.frame with data.
#' @param filters Unique filter or list of filters.
#' Each filter should be a list containing:
#' \itemize{
#' \item{'var': }{String with variable to filter on.}
#' \item{'value': }{Character vector with values from \code{var} to consider.}
#' \item{'op': }{(optional) String with operator used to retain records from \code{value}.
#' If not specified, the inclusion operator: '\%in\%' is considered, a.k.a
#' records with \code{var} in \code{value} are retained.}
#' \item{'rev': }{(optional) Logical, if TRUE (FALSE by default), the condition specified
#' in the filter is reverted.}
#' \item{'keepNA': }{(optional) Logical, if TRUE (by default), missing values in \code{var}
#' are retained. If not specified, \code{keepNA} general parameter is used.}
#' }
#' @param keepNA Logical, if TRUE (by default) missing values in \code{var} are retained.
#' If set to FALSE, missing values are ignored for all filters.
#' The specification within \code{filters} prevails on this parameter.
#' @param verbose Logical, if TRUE (FALSE by default) progress messages are printed
#' in the current console.
#' @return Filtered \code{data}.
#' @example inst/examples/filterData-example.R
#' @author Laure Cougnaud
#' @export
filterData <- function(
	data, 
	filters, 
	verbose = FALSE, 
	keepNA = TRUE){
	
	# if multiple filters are specified:
	# data is successively filtered
	if(any(sapply(filters, is.list))){
		for(par in filters){	
			data <- filterData(data = data, filters = par, verbose = verbose)
		}
		return(data)
	}
		
	# variable used to filter:
	var <- filters$var
	varST <- sQuote(var)
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
	if(keepNAFilter){
		isKept <- isKept | isNA	
	}
	
	idxKept <- which(isKept)
	
	if(length(idxKept) == 0)
		warning("No data is retained based on the filtering on the variable")
	
	if(verbose){
		msgNA <- paste(sum(isNA), "records with missing", varST)
		message(paste0(
			sum(!isKept), " records with ", varST, 
			if(rev)	" not", " ",
			op, " ", valueST, " are filtered",
			ifelse(
				sum(isNA) > 0 & all(!is.na(value)),
				ifelse(keepNAFilter, 
					paste(".", msgNA, " are retained."), 
					paste(" (including", msgNA, ").")
				),
				"."
			)
		))
	}
	
	# filter data
	data <- data[idxKept, ]
	
	return(data)
	
}