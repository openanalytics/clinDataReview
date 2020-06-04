#' Create alert data
#' 
#' @param data Data.frame with data to create the subject level alerts.
#' By default 'adsl'.
#' @param dataPath String with path to the data.
#' @param alerts A list of custom alerts as:
#' \itemize{
#' 		\item{(optional) annotation dataset, either:
#' 		\item{'dataset': }{
#' 			String with name of the annotation dataset,
#'  		e.g. 'ex' to import data from the file: '[dataset].sas7bdat'in \code{dataPath}
#' 			}
#' 		\item{'data': }{
#' 			Data.frame with annotation dataset
#' 			}
#' 		}
#' 		\item{'vars': }{
#' 			Character vector with variables of interest from annotation dataset
#' 			}
#' 		\item{'varFct': }{(optional) Function of \code{data} or string containing
#' 			manipulations from column names of \code{data} used to 
#'			create a new variable specified in \code{vars}.}
#' 		\item{'filters': }{(optional) Filters for the annotation dataset, 
#' 			see \code{filters} parameter of \code{\link{filterData}}}
#' 		\item{'varLabel': }{(optional) label for new variable in case \code{varFct} is specified.}
#' 		\item{'varsBy': }{(optional) Character vector with variables used to merge input data and
#' 			the annotation dataset. If not specified, \code{subjectVar} is used if
#' 			an external annotation dataset, or the datasets are merged by rows otherwise.
#' }
#' }
#' @param subjectVar String with subject ID variable, 'USUBJID' by default.
#' @param labelVars Named character vector containing variable labels of \code{data}.
#' This will be updated with the labels of the extra annotation variables
#' (in \code{attr(output, 'labelVars')}).
#' @param labelData (optional) String with label for input \code{data},
#' that will be included in progress messages.
#' @inheritParams medicalMonitoring-common-args
#' @importFrom glpgUtilityFct loadDataADaMSDTM getLabelVar simpleCap
#' @author Michela Pasetto
#' @export 
alertData <- function(
		data,
		dataPath = ".",
		alerts,
		subjectVar = "USUBJID",
		verbose = FALSE,
		labelVars = NULL,
		labelData = "data"
) {
	
	isNest <- ifelse(
			is.list(alerts), 
			is.null(names(alerts)), # if TRUE means that are nested lists
			length(alerts) > 1
	)
	if(isNest) {
		for(par in alerts) {	
			data <- alertData(
					data = data,
					dataPath = dataPath,
					alerts = par,
					subjectVar = subjectVar,
					labelVars = labelVars	
			)
			if(!is.null(labelVars))
				labelVars <- attr(data, "labelVars")
		}
		return(data)
	}
	
	
	alertsArgs <- alerts	
	
	# Alert by
	varsBy <- alertsArgs[["varsBy"]]
	if(is.null(varsBy))	varsBy <- subjectVar
	
	# Get data				
	#alertData <- getDataFromAlertList(listArgs = alertsArgs, dataPath = dataPath)
	alertData <- data
	labelVarsAlert <- attr(alertData, "labelVars")
	
	# Filter if required:
	alertFilter <- alertsArgs$filters
	if(!is.null(alertFilter)) {
		alertData <- filterData(
				data = alertData, 
				filters = alertFilter,
				labelVars = labelVarsAlert
		)
		labelVarsAlert <- attr(alertData, "labelVars")
		msgFilter <- attr(alertData, "msg")
	}
	
	# Get var function
	varFct <- alertsArgs$varFct
	if(!is.null(varFct)) {
		
		if(is.null(alertsArgs$vars) || length(alertsArgs$vars) != 1)
			stop("'vars' should be specified and of length 1 for 'varFct':\n", 
					capture.output(varFct))
		
		varNew <- alertsArgs$vars
		if(is.function(varFct)) {
			
			# Add new variable in the alertData
			alertData[[varNew]] <- varFct(alertData)			
			if(! is.logical(alertData[[varNew]])) stop("'varFct' should provide a logical object.")
			
		} else stop("'varFct' should be a function.")
		
		
		msgVarFct <- paste(as.character(body(varFct)), collapse = "")
		msgVarFct <- paste("based on:", msgVarFct)
		
		# set label:
		labelNew <- alertsArgs$varLabel
		if(is.null(labelNew))	labelNew <- msgVarFct
		labelVarsAlert[varNew] <- labelNew
		
	}
	
	alertVar <- alertsArgs$vars	
	
	# Create the flagged variable
	alertData[[varNew]] <- ifelse(
			alertData[[varNew]],
			"Y", "N"
			)
	
#	if(length(alertVar) > 0) {
#		
#		varsByNotInData <- setdiff(varsBy, colnames(alertData))
#		if(length(varsByNotInData) > 0)
#			stop(simpleCap(labelData), " is not annotated, because doesn't contain variable:", toString(sQuote(varsByNotInData)), ".")
#		
#		# Critical!
#		alertDataSubset <- unique(alertData[, unique(c(varsBy, alertVar)), drop = FALSE])
##		presentDuplicates <- any(duplicated(alertDataSubset[[varsBy]]))
##		if(presentDuplicates) {
##			
##			idxDuplicate <- which(duplicated(alertDataSubset[[varsBy]]))
##			idDuplicate <- alertDataSubset[[varsBy]][idxDuplicate]
##			idxToRemove <- which(
##					alertDataSubset[[varsBy]] %in% idDuplicate &
##							alertDataSubset[[alertVar]] == "N"
##			)
##			alertDataSubset <- alertDataSubset[- idxToRemove, ]					
##		}
#		
#		data <- leftJoinBase(x = data, y = alertDataSubset, by = varsBy)
##	B <- data.frame(
##			data,
##			alertDataSubset[match(data$USUBJID, alertDataSubset$USUBJID), ]	
##	)
		
		msgAnnot <- sprintf("Alert variable %s (%s) created in %s.",
				getLabelVar(var = alertVar, data = alertData, labelVars = labelVarsAlert), 
				sQuote(alertVar), simpleCap(labelData)
		) #,
#								if(!is.null(alertsArgs$varFct))	paste0(" ", msgVarFct),
#								if(!is.null(alertFilter))	paste(" whose", msgFilter),
#								if(annotDataset != "current"){
#									paste0(
#											" based on the variable(s):	", 
#											getLabelVar(var = varsBy, data = alertData, labelVars = labelVarsAlert), 
#											" (", sQuote(varsBy), ")"
#									)
#								},
#								"."
#						)
		if(verbose)	message(msgAnnot)
		
		if(!is.null(labelVars))	labelVars <- c(labelVars, labelVarsAlert[alertVar])						
#	}
	
	if(!is.null(labelVars))	 attr(data, "labelVars") <- labelVars
	
	
	data <- alertData
	
	return(data)	
	
}

#' @importFrom glpgUtilityFct loadDataADaMSDTM
getDataFromAlertList <- function(listArgs, dataPath) {
	
	if("dataset" %in% names(listArgs)) {
		
		datasetName <- listArgs[["dataset"]]
		datasetPath <- file.path(dataPath, paste0(datasetName, ".sas7bdat"))
		dataAll <- loadDataADaMSDTM(datasetPath, verbose = FALSE)
		data <- dataAll[[1]]
		attr(data, "labelVars") <- attr(dataAll, "labelVars")
		
	} else if("data" %in% names(listArgs)) {
		
		data <- listArgs$data
		
	} else stop("Provide either a dataset name or a data frame object.")
	
	return(data)
}


# custom 'left-join' function without ordering of rows and columns in x
leftJoinBase <- function(x, y, by, ...){
	if(any(duplicated(y[, by])))
		warning("Duplicated records in y dataset for: ", 
				toString(sQuote(by)), ", this might create replicated rows in the ",
				"input data."
		)
	res <- merge(
			x = x, y = y, 
			all.x = TRUE, all.y = FALSE, # left join
			by = by,
			sort = FALSE, # doesn't sort rows
			...)
	colsX <- colnames(x)
	cols <- c(colsX, setdiff(colnames(res), colsX))
	res <- res[, cols, drop = FALSE]
	return(res)
}