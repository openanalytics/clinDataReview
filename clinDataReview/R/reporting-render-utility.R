#' Get extra directory(ies) required for the clinical data review report
#' 
#' By default, the 'figures', 'tables' and patient profiles folders 
#' (\code{patientProfilePath} parameter in the general
#' config file, if specified) in the input directory are considered.
#' @inheritParams clinDataReview-common-args-report
#' @return Character vector with extra directories required by the report
#' @author Laure Cougnaud
#' @export
getExtraDirs <- function(
  inputDir = ".", 
  configDir = file.path(inputDir, "config")){
  
  extraDirs <- file.path(inputDir, c("figures", "tables"))
  
  # add patient profiles dir to repos to copy
  # extract parameters from general config file
  configGeneralParams <- getParamsFromConfig(
    configFile = "config.yml", 
    configDir = configDir,
    inputDir = inputDir
  )
  patientProfilePath <- file.path(inputDir, configGeneralParams$patientProfilePath)
  extraDirs <- unique(c(extraDirs, patientProfilePath))
  
  return(extraDirs)
  
}

#' Get path of the \code{HTML} file corresponding
#' to a specific config file.
#' 
#' The name of the \code{Markdown} file is based on:
#' \itemize{
#' \item{for the general \code{config.yml} file: }{
#' the basename of the specified \code{indexPath}}
#' \item{for other config file (each sub-report): }{
#' the name of the config file, after removal of the
#' '\code{config-}' part.}
#' }
#' @param configFiles Character vector with name or path of the config file(s).
#' @inheritParams clinDataReview-common-args-report
#' @return String with path to the HTML file
#' @importFrom tools file_path_sans_ext
#' @author Laure Cougnaud
getMdFromConfig <- function(
  configFiles, indexPath = "index.Rmd", 
  intermediateDir = "./interim"){
  
  # For general config file, use specified index path
  # For each chapter, file name are derived from the config file name
  files <- ifelse(
    basename(configFiles) == "config.yml", 
    basename(indexPath), 
    sub("^config-", "", basename(configFiles))
  )
  
  paths <- paste0(tools::file_path_sans_ext(files), ".md")
  paths <- file.path(intermediateDir, paths)
  return(paths)
  
}

#' Get interim res file
#' @inheritParams clinDataReview-common-args-report
#' @return String with path to the file with intermediate results.
#' @importFrom tools file_path_sans_ext
getInterimResFile <- function(
  intermediateDir = "./interim",
  mdFile){

  res <- file.path(
    intermediateDir,
    paste0(tools::file_path_sans_ext(basename(mdFile)), ".rds")
  )
  return(res)
  
}