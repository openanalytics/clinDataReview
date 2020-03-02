#' Create patient profile report, based on template.
#' 
#' Currently template is only available for SDTM data format. 
#' @param dataPath String with path to data.
#' @param outputFile (optional) String with name of output file,
#' 'subjectProfile.pdf' by default.
#' @param study (optional) String with study name.
#' @param batch (optional) String with batch name.
#' @param author (optional) String with author name.
#' @inheritParams filterData
#' @return Character vector with output path.
#' @author Laure Cougnaud
#' @importFrom rmarkdown render
#' @export
runPatientProfileTemplateReport <- function(
	dataPath, 
	outputFile = "subjectProfile.pdf",
	study = "custom",
	batch = "X",
	author = NULL,
	subsetDataset = NULL,
	subsetVar = NULL,
	subsetValue = NULL,
	subsetSample = NULL
	){
	
	# use absolute path for data (Rmd change wd)
	dataPath <- normalizePath(dataPath)
	
	# create output directory (if not already created)
	outputDir <- dirname(normalizePath(outputFile, mustWork = FALSE))
	if(!dir.exists(outputDir))	dir.create(outputDir, recursive = TRUE)
	
	pathTemplate <- system.file(
		"template", "patientProfiles-SDTM.Rmd", 
		package = "medicalMonitoring"
	)
	
	# copy template file in output dir (for reproducibility)
	pathTemplateNew <- file.path(outputDir, basename(pathTemplate))
	file.copy(from = pathTemplate, to = outputDir)
	
	tmp <- rmarkdown::render(
		input = pathTemplateNew,
		output_dir = outputDir, # output dir for html file
		intermediates_dir = outputDir, # output dir for intermediate file (e.g. '.md')
		envir = new.env(),
		params = list(
			dataPath = dataPath,
			outputFile = basename(outputFile),
			study = study,
			batch = batch,
			author = author,
			subsetDataset = subsetDataset,
			subsetVar = subsetVar,
			subsetValue = subsetValue,
			subsetSample = subsetSample
		)
	)
	
}