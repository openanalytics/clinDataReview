#' Filter dataset based on specified filters.
#' @param data Data.frame with data.
#' @param filters Unique filter or list of filters.
#' Each filter should be a list containing:
#' \itemize{
#' \item{'var': }{String with variable to filter on.}
#' \item{'value': }{Character vector with values from \code{var} to consider.}
#' \item{'op': }{String with operator used to retain records from \code{value}.
#' If not specified, the inclusion operator: '\%in\%' is considered, a.k.a
#' records with \code{var} in \code{value} are retained.}
#' \item{'rev': }{Logical, if TRUE (FALSE by default), the condition specified
#' in the filter is reverted.}
#' }
#' @param verbose Logical, if TRUE (FALSE by default) progress messages are printed
#' in the current console.
#' @return Filtered \code{data}.
#' @example inst/examples/filterData-example.R
#' @author Laure Cougnaud
#' @export
filterData <- function(data, filters, verbose = FALSE){
	
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
	
	idxKept <- which(isKept)
	
	if(length(idxKept) == 0)
		warning("No data is retained based on the filtering on the variable")
	
	if(verbose)
		message(paste(sum(!isKept), "records with", varST, 
			if(rev)	"not",
			op, valueST, "are filtered.")
		)
	
	# filter data
	data <- data[idxKept, ]
	
	return(data)
	
}