#' Filter dataset based on specified filters.
#' 
#' A dataset can be filtered:
#' \itemize{
#' \item{based:
#' \itemize{
#' \item{on a specific \code{value} of interest)}
#' \item{on a function of the variable (\code{valueFct} parameter), 
#' e.g. maximum of the variable)}
#' \item{to retain only non missing values of a variable (\code{keepNA} set to \code{FALSE})}
#' }
#' }
#' \item{by groups (\code{varsBy} parameter)}
#' }
#' **Note that by default, missing values in the filtering variable are retained
#' (which differs from the default behaviour in R)**.
#' To filter missing records, please set the \code{keepNA} parameter to \code{FALSE}.
#' @param data Data.frame with data.
#' @param filters Unique filter or list of filters.
#' Each filter should be a list containing:
#' \itemize{
#' \item{'var': }{String with variable from \code{data} to filter on.}
#' \item{'value': }{(optional) Character vector with values from \code{var} 
#' \strong{to consider/keep}.}
#' \item{'valueFct': }{(optional) Function (or string with this function)
#' to be applied on \code{var} to extract value to consider.
#' For example, \code{valueFct = max} will extract the records
#' with the maximum variable value.}
#' \item{'op': }{(optional) String with operator used to retain records 
#' from \code{value}.
#' If not specified, the inclusion operator: '\%in\%' is considered, a.k.a
#' records with \code{var} in \code{value} are retained.}
#' \item{'rev': }{(optional) Logical, if TRUE (FALSE by default), 
#' filtering condition based on \code{value}/\code{valueFct} is reversed.}
#' \item{'keepNA': }{(optional) Logical, if TRUE (by default), 
#' missing values in \code{var} are retained.\cr
#' If not specified, \code{keepNA} general parameter is used.}
#' \item{'varsBy': }{(optional) Character vector with variables in 
#' \code{data} containing groups to filter by}
#' \item{'varNew': }{(optional) String with name for the new variable created}
#' \item{'labelNew': }{(optional) String with label for \code{varNew}}
#' }
#' If a list of filters is specified, the logical operator 
#' (see \code{\link[base]{Logic}})
#' linking the different conditions
#' can be specified between the two conditions, e.g.: 
#' \code{list(list(var = "SEX", value = "F"), "&", list(var = "COUNTRY", value = "DEU"))}.
#' @inheritParams filterDataSingle
#' @inheritParams clinDataReview-common-args
#' @return Filtered \code{data} if \code{returnAll} is FALSE (by default).
#' Otherwise \code{data} with additional column: \code{keep} or \code{varNew} (if specified),
#' containing TRUE for records which fullfill the specified
#' condition(s) and FALSE otherwise.
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
#' \item{if TRUE: }{the full \code{data} is returned. 
#' Records are flagged based on the \code{filters} condition, in a new column:
#' \code{varNew} (if specified), or 'keep' otherwise; containing TRUE
#'  if the record fulfill all conditions, FALSE otherwise}
#' } 
#' @param labelData (optional) String with label for input \code{data},
#' that will be included in progress messages.
#' @inheritParams clinDataReview-common-args
#' @importFrom plyr dlply
#' @importFrom clinUtils simpleCap
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
	
	# keep missing values?
	keepNAFilter <- filters$keepNA
	if(is.null(keepNAFilter))	keepNAFilter <- keepNA
		
	# value used to filter
	filterOnValue <- any(c("value", "valueFct") %in% names(filters))
	
	if(filterOnValue){
	
		if("value" %in% names(filters)){
			value <- filters$value
			valueST <- toString(sQuote(value))
			filterOnValue <- TRUE
		}else	if("valueFct" %in% names(filters)){
			valueFct <- filters$valueFct
			if(is.character(valueFct)){
				value <- eval(expr = parse(text = valueFct))(data[, var])
				valueST <- valueFct
			}else if(is.function(valueFct)){
				value <- valueFct(data[, var])
				valueST <- paste(as.character(body(valueFct)), collapse = "")
			}else	stop("'valueFct' should be a character or a function.")
		}
	
		# operand: '%in%' by default
		op <- filters$op
		if(is.null(op))	op <- '%in%'
		
		# inversion:
		rev <- filters$rev
		if(is.null(rev))	rev <- FALSE
	
		# extract records matching condition
		isKept <- match.fun(op)(x = data[, var], table = value)
		if(rev)	isKept <- !isKept
		
	}else if(!keepNAFilter){
		
		isKept <- rep(TRUE, length(data[, var]))
		
	}else{
		
		stop(
			paste0("'value' of interest or 'valueFct' to obtain it, ",
				"or filtering of missing values ",
				"should be specified for the filtering of ", 
				labelData, "."
			)
		)
	}

	# filter missing values
	isNA <- is.na(data[, var])
	isKept[isNA] <- (keepNAFilter)
	
	if(!any(isKept))
		warning(paste("No", labelData, "is retained based on the filtering on the variable."))
	
	# store all steps in a message string
	msgNA <- paste(sum(isNA), "records with missing", varST)
	varMsg <- varST
	if(filterOnValue)
		varMsg <- paste0(varST, if(!rev)	" not", " ", op, " ", valueST)
	msg <- paste0(
		sum(!isKept), " records with ", varMsg,
		if(sum(isNA) > 0 & ((filterOnValue && all(!is.na(value))) | !filterOnValue))
			paste0(" (", if(keepNAFilter)	"not ", "including ", msgNA, ")")
	)
	
	# add a new variable with filtering
	isVarNewSpec <- "varNew" %in% names(filters)
	if(isVarNewSpec){
		
		varNew <- filters[["varNew"]]
		if(varNew %in% names(data))
			warning(sQuote(varNew), " is overwritten in the data.")
		data[[varNew]] <- isKept
		labelNew <- filters[["labelNew"]]
		if(is.null(labelNew))	labelNew <- paste0(varMsg, if(keepNAFilter)	" (including missing)")
		labelVars[varNew] <- labelNew
		
	}
	
	if(returnAll){
		# add variable: 'keep' with filtering
		if(!isVarNewSpec)
			data$keep <- isKept
	# filter data
	}else{		
		data <- data[which(isKept), ]
	}

	attr(data, "msg") <- msg
	attr(data, "labelVars") <- labelVars
	
	return(data)
	
}