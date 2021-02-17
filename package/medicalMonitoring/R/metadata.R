
#' Print metadata file in the medical monitoring report
#' 
#' This function receives the metadata information from \code{\link{getMetadata}} and 
#' prints them in a format for an Rmd report.
#' In general, any list could be called as long as it is composed by two elements:
#' \itemize{
#' \item{\code{pathsInfo}}{ an R object.}
#' \item{\code{datasetInfo}}{ a data.frame or a matrix.}
#' }
#' The first (\code{pathsInfo}) is printed as \code{\link{knitr::kable}} object 
#' and the second (\code{datasetInfo}) is printed as hide/show html button with 
#' the function \code{\link{collapseHtmlContent}}.
#' @param metadataInfo List of two elements named \code{pathsInfo} and 
#' \code{datasetInfo}.
#' @return Nothing. The tables are ready to be printed in Rmd.
#' @importFrom knitr kable
#' @importFrom glpgUtilityFct toDTGLPG
#' @export
printMetadataInReport <- function(metadataInfo) {
  
  pathsInfoTable <- metadataInfo$pathsInfo
  datasetInfoTable <- metadataInfo$datasetInfo
  
  pathsInfo <- kable(pathsInfoTable, col.names = c(""))  
  datasetInfoDT <- toDTGLPG(datasetInfoTable)
  
  print(pathsInfo)
  cat("\n\n")
  collapseHtmlContent(
      datasetInfoDT,
      title = "Click to show or hide further metadata information"
  )
 
}

#' Read metadata file
#' 
#' Read the metadata file from a yaml format.
#' Currently, this function checks for existance of the following inputs 
#' provided in yaml:
#' \itemize{
#' \item{\code{pathSDTMs}}{ Path to the SDTM data}
#' \item{\code{pathMeMoADs}}{ Path to the Medical Monitoring Analysis Data sets}
#' \item{\code{dateTimeMeMorun}}{ Date and time of MeMoADs generation}
#' \item{\code{datasetInfo}}{ General information about the data sets.}
#' }
#' 
#' @param filePath String of path to file.
#' @return A list of:
#' \itemize{
#' \item{\code{pathsInfo}}{ Information extracted from the inputs 
#' \code{pathSDTMs}, \code{pathMeMoADs} and \code{dateTimeMeMorun}.}
#' \item{\code{datasetInfo}}{ Information extracted from \code{datasetInfo}.}
#' }
#' @importFrom data.table rbindlist
#' @importFrom yaml read_yaml
#' @export 
getMetadata <- function(filePath) {
  
  if(! file.exists(filePath)) stop("Metadata file does not exist.")
  
  inputFile <- read_yaml(filePath)
  
  pathSDTMs <- checkAvailabilityMetadata(inputFile, "pathSDTMs")
  pathMeMoADs <- checkAvailabilityMetadata(inputFile, "pathMeMoADs")
  dateTimeMeMorun <- checkAvailabilityMetadata(inputFile, "dateTimeMeMorun")
  datasetInfoFromList <- checkAvailabilityMetadata(inputFile, "datasetInfo")
  
  if(is.list(datasetInfoFromList)) {
    datasetInfoTable <- rbindlist(datasetInfoFromList, use.names = TRUE, fill = TRUE)
  } else datasetInfoTable <- data.frame("Not Available" = datasetInfoFromList)
  
  pathsInfoTable <- rbind(
      `path SDTMs` = pathSDTMs,
      `path MeMo ADs` = pathMeMoADs,
      `date Time MeMo run` = dateTimeMeMorun  
  )
   
  resList <- list(
      pathsInfo = pathsInfoTable,
      datasetInfo = datasetInfoTable
  )
  
  return(resList)
}

checkAvailabilityMetadata <- function(inputFile, subListName) {
  
  if(is.null(inputFile[[subListName]])) {
    subListName <- "Not available" 
  } else subListName <- inputFile[[subListName]]
  
  return(subListName)
  
}

#formatDatasetInfo <- function(datasetInfoFromList) {
#    
#  datasetInfoList <- datasetInfoFromList
#  itemsInList <- c(
#      "dataset", "datetime", "signatureStatus", "signatureComment"
#  )
#  allItemsAvailable <- sapply(
#      1 : length(datasetInfoList),
#      function(i) identical(names(datasetInfoList[[i]]), itemsInList)
#  )
#  if(! all(allItemsAvailable)) stop("Not all parameters in 'datasetInfo' are available.")
#  datasetInfoTable <- do.call(rbind, datasetInfoList)
#  
#  return(datasetInfoTable)
#  
#}
#

