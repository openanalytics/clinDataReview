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
#' @param dir String with the path of the directory where 
#' the skeleton should be created. The current working
#' directory is used as default.
#' @return The files to run a report are written in the specified 
#' directory. To run the report, the user can call the 
#' \code{\link{render_clinDataReviewReport}}.
#' @export 
createClinDataReviewReportSkeleton <- function(dir = ".") {
  
  if(!dir.exists(dir))	dir.create(dir, recursive = TRUE)
  preexistingFiles <- list.files(dir)
  if(length(preexistingFiles) > 0)
	  warning("'", dir, "' is not empty. Files might be overwritten.")
  
  dirData <- file.path(dir, "data")
  if(!dir.exists(dirData))	dir.create(dirData)
  
  moveXpt(dirData)
  createExampleMetadata(dirData)
  
  dirComparisonData <- file.path(dirData, "comparisonData")
  createComparisonData(dirComparisonData)
  
  dirConfig <- file.path(dir, "config")
  if(!dir.exists(dirConfig))	dir.create(dirConfig)
  createMainConfigSkeleton(dir = dirConfig, dirData = dirData)
  moveSkeletonFiles(dir)
  
  message("The skeleton of the report is ready!")
  
}

#' Move data sets from clinUtils
#' 
#' Move SDTM data sets available in \code{clinUtils} into a 
#' specified local directory.
#' @param dir String, path to the directory.
#' @return Nothing, the data are saved in the dedicated location.
moveXpt <- function(dir) {
  
  pathToFiles <- system.file(
      "extdata", "cdiscpilot01",
      "SDTM",
      package = "clinUtils"
  )
  fileNames <- list.files(pathToFiles, full.names = TRUE)
  if(!dir.exists(dir))	dir.create(dir, recursive = TRUE)
  file.copy(
      from = fileNames,
      to = dir,
      overwrite = TRUE,
      recursive = TRUE
  )
  
  
}

#' Create an example metadata file
#' 
#' Create an example of metadata file for the \code{\link{createClinDataReviewReportSkeleton}}.
#' @param dir String, path to the directory.
#' @return Nothing, the example metadata file is created in the specified 
#' directory.
#' @importFrom yaml write_yaml
createExampleMetadata <- function(dir) {
  
  if(!dir.exists(dir))	dir.create(dir, recursive = TRUE)
  fileName <- file.path(dir, "metadata.yml")
  
  write_yaml(
      list(
          pathSDTMs = dir,
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
createComparisonData <- function(dir) {
	
	if(!dir.exists(dir))	dir.create(dir, recursive = TRUE)
  
	# Adverse events
	pathToFile <- system.file(
		"extdata", "cdiscpilot01",
		"SDTM", "ae.xpt",
		package = "clinUtils"
	)
	data <- suppressMessages(
		loadDataADaMSDTM(files = pathToFile)
	)
	dataAE <- data[[1]]
	dataAE <- dataAE[which(dataAE$USUBJID != "01-718-1427"), ]
	idx <- which(dataAE$USUBJID == "01-701-1148")
	dataAE$AESER[idx] <- "Y"
	dataAE$AESEV[idx] <- "SEVERE"
	write_xpt(dataAE, file.path(dir, "ae.xpt"))
  
	# dm
	pathToFile <- system.file(
		"extdata", "cdiscpilot01",
		"SDTM", "dm.xpt",
		package = "clinUtils"
	)
	data <- suppressMessages(
		loadDataADaMSDTM(files = pathToFile)
	)
	dataDM <- data[[1]]
	dataDM <- dataDM[which(dataDM$USUBJID != "01-718-1427"), ]
	write_xpt(dataDM, file.path(dir, "dm.xpt"))
  
}

#' Move skeleton files from the package to a directory
#' 
#' This function moves the files used to create the skeleton from the 
#' package to a specified directory.
#' @param dir String, path to the directory.
#' @return Nothing, the files are available in the specified 
#' directory.
moveSkeletonFiles <- function(dir) {
	
  if(!dir.exists(dir))	dir.create(dir, recursive = TRUE)
  
  skeletonFiles <- list.files(
      system.file("skeleton", package = "clinDataReview"),
      full.names = TRUE
  )
  file.copy(
      from = skeletonFiles,
      to = dir,
      overwrite = TRUE, recursive = TRUE
  )
  
}

#' Create the config file for the skeleton
#' 
#' This function creates the main config file for the \code{\link{createClinDataReviewReportSkeleton}} 
#' with the directory where the data are stored.
#' @param dir String, path to the directory.
#' @param dirData String, path to the directory of the data.
#' @return No return value, a file _config.yml_
#' is created in the specified directory.
#' @importFrom yaml write_yaml
createMainConfigSkeleton <- function(dir, dirData) {
  
  if(!dir.exists(dir))	dir.create(dir, recursive = TRUE)
	
  fileName <- file.path(dir, "config.yml")
  
  write_yaml(
    x = list(
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
        "config-subjectDisposition-division.yml",
        "config-enrollment-countsVisualization.yml",
        "config-cumulativeEnrollment.yml",
        "config-subjectVisits-summaryBarplot.yml",
        "config-summaryDisposition.yml",
        "config-discontinuation-listing.yml",
        "config-demographics-summaryTable.yml",
        "config-adverseEvents-division.yml",
        "config-adverseEvents-summaryTable.yml",
			  "config-adverseEvents-summaryTable-comparison.yml",
			  "config-adverseEvents-countsVisualization.yml",
			  "config-adverseEvents-listing-comparison.yml",
			  "config-adverseEvents-timeProfiles.yml",
			  "config-concomitantMedications-division.yml",
			  "config-concomitantMedications-listing.yml",
			  "config-laboratory-division.yml",
			  "config-laboratory-summaryBarplot.yml",
			  "config-laboratory-eDISH-ALT.yml",
			  "config-laboratory-spaghettiPlot.yml",
			  "config-laboratory-spaghettiPlot-byVisit.yml",
			  "config-laboratory-shiftPlot.yml",
			  "config-laboratory-errorBar.yml",
			  "config-efficacy-division.yml",
			  "config-efficacy-errorBar.yml"
      )
    ),
    file = fileName
  )
  
}




