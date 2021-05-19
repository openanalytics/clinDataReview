#' Run specified data processing steps.
#' 
#' This function is intended to automate all data processing
#' steps for use in the 'clinDataReview' reports using config files.
#' @param processing List with details of the steps to process the data.
#' Each element in the list should be a named list containing the parameters
#' for the specific processing function. The name specifies which processing
#' step, among: 
#' \itemize{
#' \item{'annotate' for \code{\link{annotateData}} (\code{annotations} parameter)}
#' \item{'filter' for \code{\link{filterData}} (\code{filters} parameter)}
#' \item{'transform' for \code{\link{transformData}} (\code{transformations} parameter)}
#' }
#' @param ... Any parameters passed to all processing functions
#' (if this parameter is available).
#' If specified, these parameters shouldn't be specified also in \code{processing}.
#' @inheritParams clinDataReview-common-args
#' @return Data.frame with processed \code{data},
#' with extra attribute: \code{labelVars}.
#' @author Laure Cougnaud
#' @importFrom methods formalArgs
#' @export
processData <- function(data, processing, labelVars = NULL, ...){
	
	if(!is.list(processing))
		stop("'processing' should be a list.")
	
	# Names of the parameter for each processing function
	processParamNames <- c(filter = "filters", annotate = "annotations", transform = "transformations")
	
	argsExtra <- list(...)
	
	for(iStep in seq_along(processing)){
		
		processI <- processing[[iStep]]
		if(is.null(names(processI)))
			stop("Processing step: '", iStep, "' is not specified (list not named).")
		
		processTypeI <- names(processI)
		
		# get adequate function name
		processFctI <- switch(processTypeI,
			filter = clinDataReview::filterData,
			annotate = clinDataReview::annotateData,
			transform = clinDataReview::transformData,
			stop("Processing step: '", iStep, "' not available in the package.")
		)
		
		argsExtraProcessI <- argsExtra[intersect(names(argsExtra), formalArgs(processFctI))]
		
		# get input parameters
		argsProcessFctI <- c(
			list(data = data, labelVars = labelVars),
			setNames(processI, processParamNames[processTypeI]),
			argsExtraProcessI
		)
		if(any(duplicated(names(argsProcessFctI))))
			stop("Duplicated parameters as input for processing step: ", iStep, ".")
		
		# run processing step
		data <- do.call(processFctI, argsProcessFctI)
		labelVars <- attr(data, "labelVars")
		
	}
	
	attr(data, "labelVars") <- labelVars
	return(data)
	
}