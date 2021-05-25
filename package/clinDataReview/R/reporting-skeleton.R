#' Create the skeleton of a report
#' 
#' Creates the skeleton of a report to start running the analyses.
#' 
#' This function is meant to get familiar with the use of the package and the 
#' necessary files to create a report.
#' 
#' It will create a ready-to-use report with example data from the \code{clinUtils} 
#' package. After getting use to the file structure, the user can 
#' substitute the example data with 
#' custom data sets and add specific configuration files.
#' 
#' @param dirName String with the path of the directory where 
#' the skeleton should be created.
#' @return The files to run a report are written in the specified 
#' directory. To run the report, the user can call the 
#' \code{\link{render_clinDataReviewReport}}.
#' @export 
reportSkeleton <- function(dirName) {
  
  if(! dir.exists(dirName)) stop("Directory '", dirName, "' does not exist.")
  preexistingFiles <- list.files(dirName)
  if(length(preexistingFiles) > 0) warning("'", dirName, "' is not empty. Files might be overwritten.")
  
  dirData <- file.path(dirName, "data")
  dirComparisonData <- file.path(dirData, "comparisonData")
  dirConfig <- file.path(dirName, "config")
  if(length(preexistingFiles) == 0) {
    if(! any(grepl("data", preexistingFiles))) dir.create(dirData)
    if(! any(grepl("comparisonData", preexistingFiles))) dir.create(dirComparisonData)
    if(! any(grepl("config", preexistingFiles))) dir.create(dirConfig)
  }
  
  #writeXpt(dirData)  
  moveXpt(dirData)
  createExampleMetadata(dirData)
  createComparisonData(dirComparisonData)
  
  createMainConfigSkeleton(dirName = dirConfig, dirData = dirData)
  moveSkeletonFiles(dirName)
  
  message("The skeleton of the report is ready!")
  
}


# #' @import clinUtils 
# #' @importFrom utils data
# #' @importFrom haven write_xpt
#writeXpt <- function(dirName) {
#  
#  dataSkeleton <- clinUtils::dataSDTMCDISCP01
#  
#  sapply(names(dataSkeleton), function(nameI) {
#        
#        dataI <- dataSkeleton[[nameI]]
#        pathI <- sprintf("%s.xpt", file.path(dirName, nameI))
#        
#        write_xpt(dataI, pathI)
#        
#      })
#  
#}

#' Move data sets from clinUtils
#' 
#' Move SDTM data sets available in \code{clinUtils} into a 
#' specified local directory.
#' @param dirName String, path to the directory.
#' @return Nothing, the data are saved in the dedicated location.
moveXpt <- function(dirName) {
  
  pathToFiles <- system.file(
      "extdata", "cdiscpilot01",
      "SDTM",
      package = "clinUtils"
  )
  fileNames <- list.files(pathToFiles, full.names = TRUE)
  file.copy(
      from = fileNames,
      to = dirName,
      overwrite = TRUE,
      recursive = TRUE
  )
  
  
}

#' Create an example metadata file
#' 
#' Create an example of metadata file for the \code{\link{reportSkeleton}}.
#' @param dirName String, path to the directory.
#' @return Nothing, the example metadata file is created in the specified 
#' directory.
#' @importFrom yaml write_yaml
createExampleMetadata <- function(dirName) {
  
  fileName <- file.path(dirName, "metadata.yml")
  
  write_yaml(
      list(
          pathSDTMs = dirName,
          dateTimeSDTMcreation = "20210101",
          datasetInfo = list(
              list(
                  dataset = "ex.xpt",
                  datetime = "20210101",
                  status = "Checking ongoing"
              ),
              list(
                  dataset = "sl.xpt",
                  datetime = "20210101",
                  status = "Checked",
                  comments = "Nothing to report"
              )
          )
      ),
      fileName
  )
  
}

#' @importFrom haven write_xpt
#' @importFrom clinUtils loadDataADaMSDTM
createComparisonData <- function(dirName) {
  
  pathToFile <- system.file(
      "extdata", "cdiscpilot01",
      "SDTM", "ae.xpt",
      package = "clinUtils"
  )
  data <- suppressMessages(
      loadDataADaMSDTM(files = pathToFile)
  )
  dataAE <- data[[1]]
  idx <- which(dataAE$USUBJID == "01-701-1148")
  dataAE$AESER[idx] <- "Y"
  dataAE$AESEV[idx] <- "SEVERE"
  write_xpt(dataAE, file.path(dirName, "ae.xpt"))
  
}

#' Move skeleton files from the package to a directory
#' 
#' This function moves the files used to create the skeleton from the 
#' package to a specified directory.
#' @param dirName String, path to the directory.
#' @return Nothing, the files are available in the specified 
#' directory.
moveSkeletonFiles <- function(dirName) {
  
  skeletonFiles <- list.files(
      system.file("skeleton", package = "clinDataReview"),
      full.names = TRUE
  )
  file.copy(
      from = skeletonFiles,
      to = dirName,
      overwrite = TRUE, recursive = TRUE
  )
  
}

#' Create the config file for the skeleton
#' 
#' This function creates the main config file for the \code{\link{reportSkeleton}} 
#' with the directory where the data are stored.
#' @param dirName String, path to the directory.
#' @param dirData String, path to the directory of the data.
#' @importFrom yaml write_yaml
createMainConfigSkeleton <- function(dirName, dirData) {
  
  fileName <- file.path(dirName, "config.yml")
  
#  configFilesSkeleton <- list.files(
#      system.file("skeleton", "config", package = "clinDataReview")
#  )
  
  write_yaml(
      list(
          study = "Example study",
          version = "Version 1",
          title = "Clinical data review for the example study",
          contactPerson = "Pippo",
          currentDataTransfer = "2021-01-01",
          previousDataTransfer = "2020-12-01",
          pathDataFolder = dirData,
          pathDataFolderOld = file.path(dirData, "comparisonData"),
          patientProfilePath = "patientProfiles",
          config = c(
              "config-patientProfiles.yml",
              "config-alert-division.yml",
              "config-alert-death.yml",
              "config-adverseEvents-division.yml",
              "config-adverseEvents-summaryTable.yml",
              "config-adverseEvents-all-countsVisualization.yml",
              "config-adverseEvents-timeProfiles.yml",
              "config-adverseEvents-listing-comparison.yml",
              "config-concomitantMedications-division.yml",
              "config-concomitantMedications-listing.yml",
              "config-laboratory-division.yml",
              "config-laboratory-eDISH-ALT.yml",
              "config-laboratory-spaghettiPlot.yml"
          )
      ),
      fileName
  )
  
}




