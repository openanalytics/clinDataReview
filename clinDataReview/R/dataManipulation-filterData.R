#' Filter a dataset based on specified filters.
#' 
#' A dataset can be filtered:
#' \itemize{
#' \item on a specific \code{value} of interest
#' \item on a function of a variable (\code{valueFct} parameter), 
#' e.g. maximum of the variable)
#' \item to retain only non missing values of a variable (\code{keepNA} 
#' set to \code{FALSE})
#' \item by groups (\code{varsBy} parameter)
#' }
#' \strong{Note that by default, missing values in the filtering variable are retained
#' (which differs from the default behaviour in R).}
#' To filter missing records, please set the \code{keepNA} parameter to \code{FALSE}.
#' @param data Data.frame with data.
#' @param filters Unique filter or list of filters.\cr
#' Each filter is a list containing:
#' \itemize{
#' \item 'var': String with variable from \code{data} to filter on.
#' \item 'value': (optional) Character vector with values from \code{var} 
#' \strong{to consider/keep}.
#' \item 'valueFct': (optional) Function (or string with this function)
#' to be applied on \code{var} to extract value to consider.\cr
#' For example, \code{valueFct = max} will extract the records
#' with the maximum value of the variable.
#' \item 'op': (optional) String with operator used to retain records 
#' from \code{value}.
#' If not specified, the inclusion operator: '\%in\%' is considered, so
#' records with \code{var} in \code{value} are retained.
#' \item 'rev': (optional) Logical, if TRUE (FALSE by default), 
#' filtering condition based on \code{value}/\code{valueFct} is reversed.
#' \item 'keepNA': (optional) Logical, if TRUE (by default), 
#' missing values in \code{var} are retained.\cr
#' If not specified, \code{keepNA} general parameter is used.
#' \item 'varsBy': (optional) Character vector with variables in 
#' \code{data} containing groups to filter by
#' \item 'postFct': (optional) Function (or string with this function) with
#' post-processing applied on the results of the filtering criteria 
#' (TRUE/FALSE for each record). This function should return TRUE/FALSE
#' (for each record or for all considered records).\cr
#' For example, '\code{postFct = any, varsBy = "group"}' retains all groups
#' which contain at least one record that fulfills the criteria.
#' \item 'varNew': (optional) String with name of a new variable containing
#' the results of the filtering criteria (as TRUE/FALSE).
#' \item 'labelNew': (optional) String with label for the \code{varNew} variable.
#' }
#' If a list of filters is specified, the different filters are \strong{independently
#' executed on the entire dataset to identify the records to retain for
#' each filtering condition.}\cr
#' The resulting selections are combined 
#' with a \code{\link[base]{Logic}} operator ('&' by default, i.e. 'AND' condition).
#' A custom logic operator can be specified between the lists describing the filter, 
#' for example:\cr
#' \code{list(list(var = "SEX", value = "F"), "&", 
#' list(var = "COUNTRY", value = "DEU"))}.
#' @inheritParams filterDataSingle
#' @inheritParams clinDataReview-common-args
#' @return If \code{returnAll}
#' \itemize{
#' \item is \code{FALSE}: \code{data} filtered with the specified filters
#' \item is \code{TRUE}: \code{data} with the additional column: \code{keep} 
#'  or \code{varNew} (if specified), containing \code{TRUE} for records 
#'  which fulfill the specified condition(s) and \code{FALSE} otherwise.
#' }
#' The output contains the additional attribute: \code{msg} which contains a message
#' describing the filtered records.
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
						warn <- paste(warn, attr(dataCur, "warn"))
						
					}else{
						
						msg <- attr(dataCur, "msg")
						warn <- attr(dataCur, "warn")
						
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
			warn <- attr(data, "warn")
			labelVars <- attr(data, "labelVars")
				
		}
	
		msg <- paste0(msg, " are ", ifelse(returnAll, "flagged", "filtered"), 
	    " in the ", labelData, ".")
		
		attr(data, "msg") <- msg
		
		if(verbose)	message(msg)
		if(length(warn) > 0) warning(warn)
		
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
#' \item if FALSE (by default): the \code{data} for only the filtered records
#' is returned.
#' \item if TRUE: the full \code{data} is returned. 
#' Records are flagged based on the \code{filters} condition, in a new column:
#' \code{varNew} (if specified), or 'keep' otherwise; containing TRUE
#'  if the record fulfill all conditions, FALSE otherwise
#' } 
#' @param labelData (optional) String with label for input \code{data},
#' that will be included in progress messages.
#' @inheritParams clinDataReview-common-args
#' @importFrom plyr dlply
#' @importFrom clinUtils simpleCap
#' @return Updated \code{data} with attributes:
#' \itemize{
#' \item 'labelVars': input \code{labelVars} with any new variables 
#'  if \code{labelNew} is specified.
#' \item 'msg': message describing the filtering process
#' \item 'warn': warning describing the filtering process
#' }
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
		
		# add variable to store row index 
		data$idxRow <- seq_len(nrow(data))
		
		# filter by group
		dataList <- dlply(data, varsBy, function(dataBy){
			inputArgsBy <- inputArgs
			inputArgsBy$filters$varsBy <- NULL # remove varsBy
			inputArgsBy$data <- dataBy # consider data for specific group
			do.call(filterDataSingle, inputArgsBy)
		})

		# no row names, otherwise contain the grouping values
		data <- do.call(rbind.data.frame, c(dataList, list(make.row.names = FALSE)))
		
		# reorder the data based on the row index
		data <- data[order(data$idxRow), ]
		# remove row index
		data$idxRow <- NULL
		
		# combine message/warning across groups
		varsByST <- toString(paste0(getLabelVar(var = varsBy, data = data, labelVars = labelVars), " (", sQuote(varsBy), ")"))
		for(type in c("msg", "warn")){
		  txt <- sapply(dataList, attr, type)
		  if(type == "msg"){
		    txt <- gsub("[[:digit:]]{1,} records with ", "Records with ", txt)
		    txt <- toString(Reduce(union, txt))
		    txt <- paste(txt, "by", varsByST) 
		  }else if(type == "warn"){
		    txt <- Reduce(union, txt)
		    if(length(txt) > 0){
  		    txt <- sub(
  		      "(No data is retained .+)", 
  		      paste0("\\1 for some groups of ", varsByST, "."), 
  		      txt
  		    )
  		    txt <- paste(txt, collapse = " ")
		    }
		  }
  		attr(data, type) <- txt
		}
		
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
  
  warn <- NULL
		
	# variable used to filter:
	var <- filters$var
	if(is.null(var))	stop(paste("'var' used for filtering of the", labelData, 
	   "should be specified."))
	varST <- paste0(getLabelVar(var = var, data = data, labelVars = labelVars), 
	   " (", sQuote(var), ")")
	if(!var %in% colnames(data)){
	  warn <- c(warn, 
	     paste0(simpleCap(labelData), " is not filtered based on the variable: ", varST,
	     " because ", varST, " is not available in the ", labelData, ".")
	  )
	  attr(data, "warn") <- warn
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
				valueST <- getFctCode(valueFct)
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
				"should be specified for the filtering of the ", 
				labelData, "."
			)
		)
	}

	# filter missing values
	isNA <- is.na(data[, var])
	isKept[isNA] <- (keepNAFilter)
	
	# apply extra post-processing function after the filtering
	postFct <- filters$postFct
	if(!is.null(postFct)){
	  
	  if(is.character(postFct)){
	    isKept <- eval(expr = parse(text = postFct))(isKept)
	    postFctST <- postFct
	  }else if(is.function(postFct)){
	    isKept <- postFct(isKept)
	    postFctST <- getFctCode(postFct)
	  }else	stop("'postFct' should be a character or a function.")
	  
	  isKept <- rep(isKept, length.out = nrow(data))
	}
	  
  if(!any(isKept)){
    warn <- c(warn, 
      paste("No", labelData, "is retained based on the filtering on the variable")
    )
	}
	
	# store all steps in a message string
	msgNA <- paste(sum(isNA), "records with missing", varST)
	varMsg <- varST
	if(filterOnValue)
		varMsg <- paste0(varST, if(!rev)	" not", " ", op, " ", valueST)
	if(!is.null(postFct))
	  varMsg <- paste(postFctST, "of", varMsg)
	msg <- paste0(
		sum(!isKept), " records with ", varMsg,
		if(sum(isNA) > 0 & ((filterOnValue && all(!is.na(value))) | !filterOnValue))
			paste0(" (", if(keepNAFilter)	"not ", "including ", msgNA, ")")
	)
	
	# add a new variable with filtering
	isVarNewSpec <- "varNew" %in% names(filters)
	if(isVarNewSpec){
		
		varNew <- filters[["varNew"]]
		if(varNew %in% names(data)){
		  warn <- c(warn, paste0(sQuote(varNew), " is overwritten in the ", labelData, "."))
		}
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
	attr(data, "warn") <- warn
	attr(data, "labelVars") <- labelVars
	
	return(data)
	
}