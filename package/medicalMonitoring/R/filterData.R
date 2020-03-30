#' Filter dataset based on specified filters.
#' 
#' A dataset can be filtered:
#' \itemize{
#' with records containing only specified values 
#' for a variable (\code{value} parameter)}, or values based on
#' a function of the variable (\code{valueFct} parameter), e.g. maximum of the variable).
#' \item{by groups (\code{varsBy} parameter)
#' }
#' **Note that by default, missing values in the filtering variable are retained
#' (which differs from the default behaviour in R)**.
#' To filter missing records, please use the \code{keepNA} parameter.
#' @param data Data.frame with data.
#' @param filters Unique filter or list of filters.
#' Each filter should be a list containing:
#' \itemize{
#' \item{'var': }{String with variable from \code{data} to filter on.}
#' \item{'value': }{Character vector with values from \code{var} to consider.}
#' \item{'valueFct': }{Function to be applied on \code{var} to extract value to consider}
#' \item{'op': }{(optional) String with operator used to retain records from \code{value}.
#' If not specified, the inclusion operator: '\%in\%' is considered, a.k.a
#' records with \code{var} in \code{value} are retained.}
#' \item{'rev': }{(optional) Logical, if TRUE (FALSE by default), records with \code{var} NOT  are retained.}
#' \item{'keepNA': }{(optional) Logical, if TRUE (by default), missing values in \code{var}
#' are retained. If not specified, \code{keepNA} general parameter is used.}
#' \item{'varsBy': }{(optional) Character vector with variables in \code{data} containing groups to filter by}
#' \item{'varNew': }{(optional) String with new variable created, containing TRUE if
#' condition is fullfilled and FALSE otherwise}
#' \item{'labelNew': }{(optional) String with label for \code{varNew}}
#' }
#' If a list of filters is specified, the logical operator (see \code{\link[base]{Logic}})
#' linking the different conditions
#' can be specified between the two conditions, e.g.: 
#' \code{list(list(var = "SEX", value = "F"), "&", list(var = "COUNTRY", value = "DEU"))}.
#' @param verbose Logical, if TRUE (FALSE by default) progress messages are printed
#' in the current console.
#' @inheritParams filterDataSingle
#' @return Filtered \code{data} if \code{returnAll} is FALSE (by default).
#' Otherwise \code{data} with additional column: \code{keep},
#' containing TRUE for records which fullfill the specified
#' condition(s) or FALSE otherwise.
#' The data contains also any new variable(s) 
#' (\code{varNew}) specified in the \code{filters}.
#' @example inst/examples/filterData-example.R
#' @author Laure Cougnaud
#' @export
filterData <- function(
	data, 
	filters, 
	keepNA = TRUE, 
	returnAll = FALSE,
	verbose = FALSE,
	labelVars = NULL,
	labelData = "data"){

	if(!is.null(filters)){
	
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
						returnAll = TRUE, 
						labelVars = labelVars,
						labelData = labelData
					)
					labelVars <- c(labelVars, attr(dataCur, "labelVars"))
					labelVars <- labelVars[!duplicated(names(labelVars))]
					
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
			
			# only keep filtered rows if 'returnAll' is FALSE
			if(!returnAll)
				data <- data[which(data$keep), setdiff(colnames(data), "keep")]
		
		}else{
			
			data <- filterDataSingle(
				data = data,
				filters = filters, 
				keepNA = keepNA,
				returnAll = returnAll,
				labelData = labelData,
				labelVars = labelVars
			)
			msg <- attr(data, "msg")
			labelVars <- attr(data, "labelVars")
				
		}
	
		msg <- paste0(msg, " are ", ifelse(returnAll, "flagged", "filtered"), " in ", labelData, ".")
		
		attr(data, "msg") <- msg
		
		if(verbose)	message(msg)
		
	}
	
	if(!is.null(labelVars))
		attr(data, "labelVars") <- labelVars
	
	return(data)
	
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
#' @param labelData (optional) String with label for input \code{data},
#' that will be included in progress messages.
#' @inheritParams medicalMonitoring-common-args
#' @importFrom plyr dlply
#' @return Updated \code{data}.
#' @author Laure Cougnaud
filterDataSingle <- function(data,
	filters, 
	keepNA = TRUE,
	returnAll = FALSE,
	labelVars = NULL,
	labelData = "data"){

	# filter by group
	if("varsBy" %in% names(filters)){
		
		inputArgs <- as.list(environment())
		
		varsBy <- filters$varsBy
		
		dataList <- dlply(data, varsBy, function(dataBy){
			inputArgsBy <- inputArgs
			inputArgsBy$filters$varsBy <- NULL # remove varsBy
			inputArgsBy$data <- dataBy # consider data for specific group
			do.call(filterDataSingle, inputArgsBy)
		})

		data <- do.call(rbind, dataList)
		
		# combine message across groups
		msg <- gsub("[[:digit:]]{1,} records with ", "", sapply(dataList, attr, "msg"))
		msg <- toString(Reduce(union, msg))
		varsByST <- toString(paste0(getLabelVar(var = varsBy, data = data, labelVars = labelVars), " (", sQuote(varsBy), ")"))
		msg <- paste(msg, "by", varsByST) 
		attr(data, "msg") <- msg
		
		# combine labelVars
		labelVarsNew <- Reduce(c, lapply(dataList, attr, "labelVars"))
		labelVarsNew <- labelVarsNew[!duplicated(names(labelVarsNew))]
		if(!"labelNew" %in% names(filters)){
			newVars <- setdiff(names(labelVarsNew), names(labelVars))
			labelVarsNew[newVars] <- paste(labelVarsNew[newVars], "by", varsByST) 
		}
		attr(data, "labelVars") <- labelVarsNew
		
		return(data)
		
	}
		
	# variable used to filter:
	var <- filters$var
	if(is.null(var))	stop(paste("'var' used for filtering of", labelData, "should be specified."))
	varST <- paste0(getLabelVar(var = var, data = data, labelVars = labelVars), " (", sQuote(var), ")")
	if(!var %in% colnames(data)){
		warning(paste(simpleCap(labelData), "is not filtered based on the variable:", varST,
			"because", varST, "is not available in the input data."))
		return(data)
	}
		
	# value used to filter
	if("value" %in% names(filters)){
		value <- filters$value
		valueST <- toString(sQuote(value))
	}else	if("valueFct" %in% names(filters)){
		fct <- filters$valueFct
		value <- match.fun(fct)(data[, var])
		valueST <- toString(deparse(substitute(fct)))
	}else	stop(paste0("'value' of interest or 'fct' to obtain it",
		"should be specified for the filtering of ", labelData, "."))
	
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
		warning(paste("No", labelData, "is retained based on the filtering on the variable."))
	
	# store all steps in a message string
	msgNA <- paste(sum(isNA), "records with missing", varST)
	varMsg <- paste0(varST, if(!rev)	" not", " ", op, " ", valueST)
	msg <- paste0(
		sum(!isKept), " records with ", varMsg,
		if(sum(isNA) > 0 & all(!is.na(value)))
			paste0(" (", if(keepNAFilter)	"not ", "including", msgNA, ")")
	)
	
	# add a new variable with filtering
	if("varNew" %in% names(filters)){
		
		varNew <- filters[["varNew"]]
		if(varNew %in% names(data))
			warning(sQuote(varNew), "is overwritten in the data.")
		data[[varNew]] <- isKept
		labelNew <- filters[["labelNew"]]
		if(is.null(labelNew))	labelNew <- paste0(varMsg, if(keepNAFilter)	" (including missing)")
		labelVars[varNew] <- labelNew
		
	# add variable: 'keep' with filtering
	}else	if(returnAll){
		data$keep <- isKept
	# filter data
	}else{		
		data <- data[which(isKept), ]
	}

	attr(data, "msg") <- msg
	attr(data, "labelVars") <- labelVars
	
	return(data)
	
}