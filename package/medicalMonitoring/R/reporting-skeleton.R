#'@importFrom utils globalVariables
utils::globalVariables("dataADaMCDISCP01")

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
#' \code{\link{render_medicalMonitoringReport}}.
#' @export 
reportSkeleton <- function(dirName) {
  
  if(! dir.exists(dirName)) stop("Directory '", dirName, "' does not exist.")
  preexistingFiles <- list.files(dirName)
  if(length(preexistingFiles) > 0) warning("'", dirName, "' is not empty. Files might be overwritten.")
  
  dirData <- file.path(dirName, "data")
  dirConfig <- file.path(dirName, "config")
  if(length(preexistingFiles) == 0) {
    if(! any(grepl("data", preexistingFiles))) dir.create(dirData)
    if(! any(grepl("config", preexistingFiles))) dir.create(dirConfig)
  }
  
  writeXpt(dirData)  
  createExampleMetadata(dirData)
  
  createMainConfigSkeleton(dirName = dirConfig, dirData = dirData)
  moveSkeletonFiles(dirName)
  
  message("The skeleton of the report is ready!")
  
}


#' @import clinUtils 
#' @importFrom utils data
#' @importFrom haven write_xpt
writeXpt <- function(dirName) {
  
  dataSkeleton <- clinUtils::dataSDTMCDISCP01
  
  sapply(names(dataSkeleton), function(nameI) {
        
        dataI <- dataSkeleton[[nameI]]
        pathI <- sprintf("%s.xpt", file.path(dirName, nameI))
        
        write_xpt(dataI, pathI)
        
      })
  
}


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


moveSkeletonFiles <- function(dirName) {
  
  skeletonFiles <- list.files(
      system.file("skeleton", package = "medicalMonitoring"),
      full.names = TRUE
  )
  file.copy(
      from = skeletonFiles,
      to = dirName,
      overwrite = TRUE, recursive = TRUE
  )
   
}

#' @importFrom yaml write_yaml
createMainConfigSkeleton <- function(dirName, dirData) {
  
  fileName <- file.path(dirName, "config.yml")
  
#  configFilesSkeleton <- list.files(
#      system.file("skeleton", "config", package = "medicalMonitoring")
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
          patientProfilePath = "patientProfiles",
          config = c(
              "config-patientProfiles.yml",
              "config-adverseEvents-division.yml",
              "config-adverseEvents-all-countsVisualization.yml",
              "config-adverseEvents-timeProfiles.yml"
          )
      ),
      fileName
  )
  
}




