
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
#' \link{\code{render_medicalMonitoringReport}}.
#' @export 
reportSkeleton <- function(dirName) {
  
  if(! dir.exists(dirName)) stop("Directory '", dirName, "' does not exist.")
  preexistingFiles <- list.files(dirName)
  if(length(preexistingFiles) > 0) warning("'", dirName, "' is not empty. Files might be overwritten.")
  
  # create data folder
  dirData <- file.path(dirName, "data")
  if(length(preexistingFiles) == 0 || ! any(grepl("data", preexistingFiles))) dir.create(dirData)
  
  writeXpt(dirData)
  
  createExampleMetadata(dirData)
  
  moveSkeletonFiles(dirName)
  
  message("The skeleton of the report is ready!")
  
}


#' @import clinUtils
writeXpt <- function(dirName) {
  
  data(dataADaMCDISCP01)
  
  sapply(names(dataADaMCDISCP01), function(nameI) {
        
    dataI <- dataADaMCDISCP01[[nameI]]
    pathI <- file.path(dirName, nameI)
    
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
