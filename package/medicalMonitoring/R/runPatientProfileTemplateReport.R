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
	
	pathTemplate <- system.file("template", "patientProfiles-SDTM.Rmd", 
		package = "medicalMonitoring")

	dataPath <- normalizePath(dataPath)

	tmp <- rmarkdown::render(
		input = pathTemplate,
		params = list(
			dataPath = dataPath,
			outputFile = outputFile,
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