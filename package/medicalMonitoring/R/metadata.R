
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
#' @param filePath String of path to file. Currently only one file path is supported. 
#' If more than one paths are provided, a warning will be printed and 
#' the first path will be used.
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
  
  if(! is.character(filePath)) stop("'filePath' argument should be a character.")
  if(length(filePath) > 1) {
    warning("More than one 'filePath' provided. Only the first one will be used.")
    filePath <- filePath[1]
  }
  if(! file.exists(filePath)) stop("Metadata file does not exist.")
  
  paramsList <- read_yaml(filePath)
  
  pathSDTMs <- checkAvailabilityMetadata(paramsList, "pathSDTMs")
  pathMeMoADs <- checkAvailabilityMetadata(paramsList, "pathMeMoADs")
  dateTimeMeMorun <- checkAvailabilityMetadata(paramsList, "dateTimeMeMorun")
  datasetInfoFromList <- checkAvailabilityMetadata(paramsList, "datasetInfo")
  
  if(is.list(datasetInfoFromList)) {
    datasetInfoTable <- rbindlist(datasetInfoFromList, use.names = TRUE, fill = TRUE)
  } else datasetInfoTable <- data.frame("Not Available" = datasetInfoFromList)
  
  pathsInfoTable <- rbind(
      pathSDTMs = pathSDTMs,
      pathMeMoADs = pathMeMoADs,
      dateTimeMeMorun = dateTimeMeMorun  
  )
  
  resList <- list(
      pathsInfo = pathsInfoTable,
      datasetInfo = datasetInfoTable
  )
  
  class(resList) <- c("medicalMonitoringMetadata",  class(resList))
  
  return(resList)
  
}


#' Print metadata file in the medical monitoring report
#' 
#' This function receives the metadata information from \code{\link{getMetadata}} and 
#' prints them in a format for an Rmd report.
#' In general, any list could be called as long as it is composed by two elements:
#' \itemize{
#' \item{\code{pathsInfo}}{ an R object.}
#' \item{\code{datasetInfo}}{ a data.frame or a matrix.}
#' }
#' The first (\code{pathsInfo}) is printed as \code{\link[knitr]{kable}} object 
#' and the second (\code{datasetInfo}) is printed as hide/show html button with 
#' the function \code{\link{collapseHtmlContent}}.
#' @param x List of two elements named \code{pathsInfo} and 
#' \code{datasetInfo}.
#' @param options List of extra options to be passed as chunk options.
#' @param ... Extra arguments to be passed.
#' @return Nothing. The tables are ready to be printed in Rmd.
#' @importFrom knitr knit_print
#' @importFrom clinUtils getClinDT
#' @importFrom htmltools tagList knit_print.shiny.tag.list
#' @export
knit_print.medicalMonitoringMetadata <- function(
    x, options = list(), ...
) {
  
  datasetInfoTable <- x$datasetInfo
  datasetInfoDT <- getClinDT(datasetInfoTable)
  
  if(! is.null(options$dateReportRun)) {
    if (options$dateReportRun) {
      pathsInfoWithDate <- addDateOfReportRun(x$pathsInfo)
      print(formatPathDateInfoMetadata(pathsInfoWithDate))
    } else {
      print(formatPathDateInfoMetadata(x$pathsInfo))
    }
  } else print(formatPathDateInfoMetadata(x$pathsInfo))
  
  
  cat("\n\n")
  table <- collapseHtmlContent(
      datasetInfoDT,
      title = "Click to show or hide further details on the original SDTM data"
  )
  htmltools::knit_print.shiny.tag.list(table)
  
}

#' Format the info on paths from metadata
#' 
#' @param pathsInfo  matrix, see output from \code{\link{getMetadata}}.
#' @return A kable object, to be printed.
#' @importFrom knitr kable
formatPathDateInfoMetadata <- function(pathsInfo) {
  
  pathsInfoRenamed <- renamePathDateInfoMetadata(pathsInfo)
  pathsInfoToPrint <- kable(pathsInfoRenamed, col.names = c(""))
  
  return(pathsInfoToPrint)
}

#' Add date of report running
#' 
#' Add the today's date of when the report runs to the info of the metadata.
#' @param pathsInfo  matrix, see output from \code{\link{getMetadata}}.
#' @return A matrix, same as input \code{pathsInfo} with an extra row with the date 
#' of today.
addDateOfReportRun <- function(pathsInfo) {
  
  dateToday <- as.character(Sys.Date())
  pathsInfoWithDate <- rbind(pathsInfo, dateToday)
  
  return(pathsInfoWithDate)
}

#' Rename variable names of metadata info
#' 
#' Rename variable names referring to the paths and the date.
#' @param pathsInfo A matrix, see output from \code{\link{getMetadata}}.
#' @return A matrix, same as input \code{pathsInfo} with renamed variable names.
renamePathDateInfoMetadata <- function(pathsInfo) {
  
  rownames(pathsInfo) <- gsub("pathSDTMs",
      "Original data (SDTM) path:",
      gsub("pathMeMoADs",
          "Medical Monitoring Analysis Dataset (MeMo-AD) path:",
          gsub("dateTimeMeMorun",
              "MeMo-AD creation date:",
              gsub("dateToday",
                  "Report & patient profiles creation date:",
                  rownames(pathsInfo)
              )
          )
      )
  )
  
  return(pathsInfo)
  
}



#' Check availability of arguments in list
#' 
#' @param paramsList A named list.
#' @param subListName String indicating which of the sublist names to check for existance.
#' @return The content of the sublist. If not available, returns "Not Available".
checkAvailabilityMetadata <- function(paramsList, subListName) {
  
  if(is.null(paramsList[[subListName]])) {
    subListName <- "Not available" 
  } else subListName <- paramsList[[subListName]]
  
  return(subListName)
  
}

