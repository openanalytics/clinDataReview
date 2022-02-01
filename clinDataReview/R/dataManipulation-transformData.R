#' Transform data.
#' 
#' Transform data from long to wide format.
#' This function converts formats with the \code{stats::reshape} function.
#' 
#' @param data Data.frame with input data to transform.
#' @param transformations Transformations (or list of those) as a list with:
#' \itemize{
#' \item{'type': }{String with type of transformation.
#' Currently, only: 'pivot_wider' is available}
#' \item{extra parameters for the transformation, for:
#' \itemize{
#' \item{'pivot_wider': }{
#' \itemize{
#' \item{'varsID': }{Character vector with variable(s) of \code{data}
#' defining unique records in the wide format.
#' Corresponds to the \code{idvar} parameter of the \code{reshape} function.}
#' \item{'varPivot': }{String with unique variable of \code{data}
#' containing elements to pivot in different columns in the wide format
#' (used for column names).
#' Corresponds to the \code{timevar} parameter of the \code{reshape} function.}
#' \item{'varsValue': }{Character vector with variable(s) of \code{data}
#' used to fill the columns in the wide format.
#' Corresponds to the \code{v.names} parameter of the \code{reshape} function.}
#' }
#' }
#' }
#' }
#' }
#' @return A data.frame in wide format.
#' @inheritParams clinDataReview-common-args
#' @author Laure Cougnaud
#' @importFrom stats reshape
#' @export
transformData <- function(
	data, transformations,
	verbose = FALSE, labelVars = NULL) {
	
	if(!is.null(transformations)) {
	
		isNest <- ifelse(is.list(transformations), is.null(names(transformations)), length(transformations) > 1)
		if(isNest){
			
			for(par in transformations){	
				data <- transformData(
					data = data, 
					transformations = par,
					verbose = verbose,
					labelVars = labelVars
				)
				if(!is.null(attr(data, "labelVars")))
					labelVars <- attr(data, "labelVars")
			}
			return(data)
			
		} else {
			
			transType <- transformations$type
			if(is.null(transType))
				stop("'type' of transformation should be specified.")
			
			if(transType == "pivot_wider") {
				
				## extract input parameters
				varsID <- transformations$varsID
				if(is.null(varsID))
					stop("'varsID' should be specified for a", 
						sQuote(transType), "transformation."
					)
				
				varPivot <- transformations$varPivot
				if(is.null(varPivot))
					stop("'varPivot' should be specified for a", 
						sQuote(transType), "transformation."
					)
				
				varsValue <- transformations$varsValue
				
				## reshape the data
				data <- reshape(
					data = data,
					direction = "wide",
					# column with value to fill the df with
					v.names = varsValue,
					# variables defining unique records
					idvar = varsID,
					# variable converted from long to wide (multiple columns)
					timevar = varPivot
				)
				
				## set labels to variables just created:
				attrTransf <- attributes(data)$reshapeWide
				labelVarsValue <- getLabelVar(
					var = attrTransf$v.names, 
					data = data, 
					labelVars = labelVars
				)
				labelVarsNew <- paste(
					rep(attrTransf$times, each = length(attrTransf$v.names)),
					rep(labelVarsValue, times = length(attrTransf$times))
				)
				names(labelVarsNew) <- attrTransf$varying
				
				# remove label of value and pivot columns
				# as no longer in the data
				labelVars <- labelVars[!names(labelVars) %in% c(varsValue, varPivot)]
				labelVars <- c(labelVars, labelVarsNew)
				
				## print message
				if(verbose)
					message(paste("Data is converted to a wide format with",
						"variables:", 
						toString(sQuote(getLabelVar(varsValue, data = data, labelVars = labelVars))),
						"for different:", toString(sQuote(getLabelVar(varPivot, data = data, labelVars = labelVars))),
						"by", toString(sQuote(getLabelVar(varsID, data = data, labelVars = labelVars))), 
						"pivoted to different columns."
					))
				
			} else {
				stop(paste("Transformation of type", sQuote(transType), "not defined."))
			}
			
		}
		
	}
	
	if(!is.null(labelVars))	
		attr(data, "labelVars") <- labelVars
	
	return(data)
	
}
