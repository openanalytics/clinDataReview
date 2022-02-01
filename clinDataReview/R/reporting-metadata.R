
#' Read metadata file
#' 
#' Read the metadata file from a yaml format.
#' This function checks for existance of the metadata file and its content.
#' In particular, within the yaml file matches the following strings:
#' \itemize{
#' \item{\code{path}}{ Path to the data. More than one path is allowed.}
#' \item{\code{dateTime}}{ Date and time, usually of the SDTM data creation. 
#' When printing the metadata in Rmd document, there is the possibility to add
#'  the date and time of the report generation. 
#' See \code{\link{knit_print.clinDataReviewMetadata}}. }
#' \item{\code{datasetInfo}}{ General information about the data sets.}
#' }
#' 
#' Note that the input names do not necessarly have to match the exact names. 
#' For instance, the user can also write "dataTimeMySDTMData", 
#' and the function will parse for existance of the string "dataTime".
#' 
#' @param filePath String of path to file. Currently only one file path is supported. 
#' If more than one paths are provided, a warning will be printed and 
#' the first path will be used.
#' @param namesInfo Named vector to rename the final output when printed in Rmd. 
#' The renaming happens only if the metadata info are printed in Rmd and not in the console.
#' @return A list of:
#' \itemize{
#' \item{\code{summaryInfo}}{ Information extracted from the inputs 
#' \code{path}, and \code{dateTime}.}
#' \item{\code{datasetInfo}}{ Information extracted from \code{datasetInfo}.}
#' }
#' @example inst/examples/metadata-example.R
#' @importFrom data.table rbindlist
#' @importFrom yaml read_yaml
#' @export 
getMetadata <- function(filePath, namesInfo) {
  
  if(! is.character(filePath)) stop("'filePath' argument should be a character.")
  if(length(filePath) > 1) {
    warning("More than one 'filePath' provided. Only the first one will be used.")
    filePath <- filePath[1]
  }
  if(! file.exists(filePath)) stop("Metadata file does not exist.")
  
  paramsList <- read_yaml(filePath)
  
  paths <- checkAvailabilityMetadata(paramsList, "path")
  dateTime <- checkAvailabilityMetadata(paramsList, "dateTime")
  datasetInfoFromList <- checkAvailabilityMetadata(paramsList, "datasetInfo")
  
  if(is.list(datasetInfoFromList)) {
    datasetInfoTable <- rbindlist(datasetInfoFromList, use.names = TRUE, fill = TRUE)
  } else datasetInfoTable <- data.frame("Not Available" = datasetInfoFromList)
  
  summaryInfoTable <- rbind(paths, dateTime)
  
  resList <- list(
      summaryInfo = summaryInfoTable,
      datasetInfo = datasetInfoTable
  )
  
  if(missing(namesInfo)) namesInfo <- setNames(
        rownames(summaryInfoTable), rownames(summaryInfoTable)
    )
  
  attr(resList, "namesInfo") <- namesInfo  
  class(resList) <- c("clinDataReviewMetadata",  class(resList))
  
  return(resList)
  
}


#' Print metadata file in the clinical data report
#' 
#' This function receives the metadata information from \code{\link{getMetadata}} and 
#' prints them in a format for an Rmd report.
#' In general, any list could be called as long as it is composed by two elements:
#' \itemize{
#' \item{\code{summaryInfo}}{ an R object.}
#' \item{\code{datasetInfo}}{ a data.frame or a matrix.}
#' }
#' The first (\code{summaryInfo}) is printed as \code{\link[knitr]{kable}} object 
#' and the second (\code{datasetInfo}) is printed as hide/show html button with 
#' the function \code{\link{collapseHtmlContent}}.
#' @param x List of two elements named \code{summaryInfo} and 
#' \code{datasetInfo}.
#' @param options List of extra options to be passed as chunk options. 
#' The option \code{dateReportRun} sets to true prints the date and time of the report creation.
#' @param ... Extra arguments to be passed.
#' @return Nothing. The tables are ready to be printed in Rmd.
#' @importFrom knitr knit_print
#' @importFrom clinUtils getClinDT
#' @importFrom htmltools tagList knit_print.shiny.tag.list
#' @return html code to include metadata in a report
#' @export
knit_print.clinDataReviewMetadata <- function(
    x, options = list(), ...
) {
  
  datasetInfoTable <- x$datasetInfo
  datasetInfoDT <- getClinDT(datasetInfoTable)
  
  namesInfo <- attr(x, "namesInfo")
  
  if(! is.null(options$dateReportRun)) {
    if (options$dateReportRun) {
      summaryInfoWithDate <- addDateOfReportRun(x$summaryInfo)
      print(
          formatPathDateInfoMetadata(
              summaryInfo = summaryInfoWithDate, namesInfo = namesInfo
          )
      )
    } else {
      print(
          formatPathDateInfoMetadata(summaryInfo = x$summaryInfo, namesInfo = namesInfo)
      )
    }
  } else print(
        formatPathDateInfoMetadata(summaryInfo = x$summaryInfo, namesInfo = namesInfo)
  )
  
  
  cat("\n\n")
  table <- collapseHtmlContent(
      datasetInfoDT,
      title = "Click to show or hide further details on the original SDTM data"
  )
  htmltools::knit_print.shiny.tag.list(table)
  
}

#' Format the info on paths from metadata
#' 
#' @param summaryInfo  matrix, see output from \code{\link{getMetadata}}.
#' @param namesInfo Named vector to rename the final output. 
#' @return A kable object, to be printed.
#' @importFrom knitr kable
formatPathDateInfoMetadata <- function(summaryInfo, namesInfo) {
  
  summaryInfoRenamed <- renamePathDateInfoMetadata(summaryInfo, namesInfo)
  summaryInfoToPrint <- kable(summaryInfoRenamed, col.names = c(""))
  
  return(summaryInfoToPrint)
}

#' Add date of report running
#' 
#' Add the today's date of when the report runs to the info of the metadata.
#' @param summaryInfo  matrix, see output from \code{\link{getMetadata}}.
#' @return A matrix, same as input \code{summaryInfo} with an extra row with the date 
#' of today.
addDateOfReportRun <- function(summaryInfo) {
  
  dateToday <- as.character(Sys.Date())
  summaryInfoWithDate <- rbind(summaryInfo, dateToday)
  
  return(summaryInfoWithDate)
}

#' Rename variable names of metadata info
#' 
#' Rename variable names referring to the paths and the date.
#' @param summaryInfo A matrix, see output from \code{\link{getMetadata}}.
#' @param namesInfo Named vector to rename the final output. 
#' @return A matrix, same as input \code{summaryInfo} with renamed variable names.
renamePathDateInfoMetadata <- function(summaryInfo, namesInfo) {
  
  idx <- match(names(namesInfo), rownames(summaryInfo))  
  rownames(summaryInfo)[idx] <- namesInfo
  
  rownames(summaryInfo) <- gsub(
      "dateToday", "Report & patient profiles creation date:", 
      rownames(summaryInfo)
  )
  
  return(summaryInfo)
  
}



#' Check availability of arguments in list
#' 
#' @param paramsList A named list.
#' @param subListName String indicating which of the sublist names to check for existance.
#' @return The content of the sublist. If not available, returns "Not Available".
checkAvailabilityMetadata <- function(paramsList, subListName) {
  
  subListIdx <- grepl(
      subListName, names(paramsList), ignore.case = TRUE
  )
  
  if(! any(as.logical(subListIdx))) {
    
    subListToReturn <- "Not available" 
    
  } else {
    
    subListToReturn <- extractNamesOfMetadata(
        paramsList = paramsList,
        subListIdx = subListIdx
    )
  }
  
  return(subListToReturn)
  
}

extractNamesOfMetadata <- function(paramsList, subListIdx) {
  
  nameToExtract <- names(paramsList)[subListIdx]
  
  if(! any(grepl("datasetInfo", nameToExtract, ignore.case = TRUE))) {
    
    subLists <- unlist(sapply(nameToExtract, function(i) paramsList[[i]]))
    subListToReturn <- matrix(subLists, nrow = length(nameToExtract))
    rownames(subListToReturn) <- nameToExtract
    
  } else {
    
    subListToReturn <- paramsList[[nameToExtract]]
  }
  
  return(subListToReturn)
  
}