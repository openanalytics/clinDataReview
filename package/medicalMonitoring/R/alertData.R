#' @param data Data.frame with data to create the subject level alerts.
#' By default 'adsl'.
#' @param dataPath String with path to the data.
#' @param alert A list of custom alerts as:
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
#' @importFrom glpgUtilityFct loadDataADaMSDTM
alertData <- function(
		data = "adsl",
		dataPath = ".",
		alerts,
		subjectVar = "USUBJID",
		labelVars = NULL	
) {
	
#	isNest <- ifelse(
#			is.list(alerts), 
#			is.null(names(alerts)), # if TRUE means that are nested lists
#			length(alerts) > 1
#	)
#	if(isNest){
#		for(par in alerts){	
#			data <- alertData(
#			)
#			if(!is.null(labelVars))
#				labelVars <- attr(data, "labelVars")
#		}
#		return(data)
#	}
	
	sapply(1 : length(alerts), function(idx) {
				
				alertsArgs <- alerts[[idx]]	
				
				# Alert by
				varsBy <- alerts[["varsBy"]]
				if(is.null(varsBy))	varsBy <- subjectVar
				
				# Get data				
				alertData <- getDataFromAlertList(listArgs = alertsArgs, dataPath = dataPath)
				labelVars <- attr(alertData, "labelVars")
								
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
						
						alertsArgs[[varNew]] <- varFct(alertData)
						msgVarFct <- paste(as.character(body(varFct)), collapse = "")
						
					} else	stop("'varFct' should be a character or a function.")
					
					# set label:
					labelNew <- alertsArgs$varLabel
					if(is.null(labelNew))	labelNew <- msgVarFct
					labelVarsAnnot[varNew] <- labelNew
					
					msgVarFct <- paste("based on:", msgVarFct)
					# remove variable in data if already present
					data[[varNew]] <- NULL
					
				}
				
				# alertsArgs[[varNew]] va attaccata al db e poi bisogna fare il match con data
#					if(length(annotVar) > 0){
#						
#						varsByNotInData <- setdiff(varsBy, colnames(annotData))
#						if(length(varsByNotInData) > 0)
#							stop(simpleCap(labelData), " is not annotated with ", sQuote(annotDataset), ", because doesn't contain variable:", toString(sQuote(varsByNotInData)), ".")
#						
#						annotData <- unique(annotData[, unique(c(varsBy, annotVar)), drop = FALSE])
#						
#						data <- leftJoinBase(x = data, y = annotData, by = varsBy)
#						
#						if(annotDataset == "current")	data$idAnnot <- NULL
#						
#						msgAnnot <- paste0(
#								simpleCap(labelData), " annotated with variable(s): ", 
#								toString(paste0(
#												getLabelVar(var = annotVar, data = annotData, labelVars = labelVarsAnnot), 
#												" (", sQuote(annotVar), ")"
#										)),
#								" from the ", sQuote(annotDataset), " dataset",
#								if(!is.null(annotations$varFct))	paste0(" ", msgVarFct),
#								if(!is.null(annotFilter))	paste(" whose", msgFilter),
#								if(annotDataset != "current"){
#									paste0(
#											" based on the variable(s):	", 
#											getLabelVar(var = varsBy, data = annotData, labelVars = labelVarsAnnot), 
#											" (", sQuote(varsBy), ")"
#									)
#								},
#								"."
#						)
#						if(verbose)	message(msgAnnot)
#						
#						if(!is.null(labelVars))	labelVars <- c(labelVars, labelVarsAnnot[annotVar])
#						
#					}
				
			}
	
	)
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
