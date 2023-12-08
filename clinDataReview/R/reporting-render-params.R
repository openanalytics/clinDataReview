#' Get parameters from a config file
#' 
#' Please note that the information from this config file
#' and the general config file: \code{config.yml}
#' are considered.\cr
#' In case parameters are defined both in the general
#' and specific config files, the parameter from the
#' general config file is ignored.
#' @inheritParams clinDataReview-common-args-report
#' @return List with parameters from the specified \code{configFile}
#' and the general config file: \code{config.yml}.\cr
#' There are two specific handlers:
#' \itemize{
#' \item parameters tagged with '[param] !r [value]'
#' are evaluated in R, and their evaluated value is returned
#' \item parameters tagged with '[param] !r-lazy [value]'
#' are imported as character, and need to be further process
#' with \code{\link{forceParams}} inside the report.
#' }
#' Parameters with YAML type 'r-lazy' are imported as character,
#' with this additional class.
#' @seealso \link{forceParams}
#' @author Laure Cougnaud
#' @importFrom yaml read_yaml
#' @family clinical data reporting
#' @export
getParamsFromConfig <- function(
  configFile, 
  configDir = file.path(inputDir, "config"),
  inputDir = "."){
  
  if(!dir.exists(configDir))
    stop("Config directory: ", sQuote(configDir), " doesn't exist.")
  
  lazyRHandlers <- list(
    # as the Rmd !r default handler
    `r` = function(x){
      eval(str2expression(text = x))
    },
    # version with lazy-evaluation
    # Note: would be 'cleaner' to store expression rather than character
    # but rapply doesn't work with 'call' type of parameters	
    `r-lazy` = function(x){
      structure(x, class = c("r-lazy", class(x)))
    }
  )
  
  # load general config file
  configGeneralPath <- file.path(configDir, "config.yml")
  if(file.exists(configGeneralPath)) {
    
    configGeneralParams <- yaml::read_yaml(configGeneralPath, handlers = lazyRHandlers)
    
  } else {
    warning("General config file: 'config.yml' not available in:", 
      configDir, ".", call. = FALSE)
    configGeneralParams <- NULL
  }
  
  if(configFile != "config.yml") {
    
    configFilePath <- file.path(configDir, configFile)
    
    if(file.exists(configFilePath)) {
      configParams <- yaml::read_yaml(configFilePath, handlers = lazyRHandlers)
    } else {
      stop("File ", sQuote(configFilePath), " cannot be found. \n",
        "Please check the spelling is correct ",
        "or the file is saved in the directory with the other config files."
      )
    }
    
    paramsDuplicated <- intersect(names(configGeneralParams), names(configParams))
    if(length(paramsDuplicated) > 0){
	      warning(paste(
	        "Parameter(s):", toString(shQuote(paramsDuplicated)), "are",
	        "both defined in the general and chapter-specific config",
	        "file.\nThe parameter(s) from the chapter-specific config file are considered."
	      ))
      configGeneralParams <- configGeneralParams[
        -which(names(configGeneralParams) %in% paramsDuplicated)
      ]
    }
    
    params <- c(configGeneralParams, configParams)
    
  } else {
    
    params <- configGeneralParams
    
  }
  
  return(params)
  
}

#' Force the evaluation of the
#' parameters from config file.
#' 
#' This function is only useful if some
#' parameters should be lazy-evaluated in the report.
#' These parameters should have the class: \code{r-lazy}.
#' A typical use case is a parameter that
#' consists of a R expression
#' depending on objects created in a template
#' report (typically \code{data}).
#' \cr
#' Parameters are searched in the environment
#' in which this function is called from.
#' @param params List of parameters as obtained
#' via the \code{\link{getParamsFromConfig}}
#' function.
#' @return Input parameter list, with
#' object(s) of class \code{r-lazy}
#' evaluated.
#' @examples 
#' data <- mtcars
#' params <- list(label = "Cars dataset", nrow = structure("nrow(data)", class = "r-lazy"))
#' str(params)
#' str(forceParams(params))
#' @author Laure Cougnaud
#' @seealso \link{getParamsFromConfig}
#' @family clinical data reporting
#' @export
forceParams <- function(params){
  
  envirParent <- parent.frame(n = 1)
  
  paramsEval <- rapply(
    object = params, 
    f = function(x)	eval(parse(text = x), envir = envirParent),
    classes = "r-lazy",
    how = "replace"
  )
  
  return(paramsEval)
  
}
