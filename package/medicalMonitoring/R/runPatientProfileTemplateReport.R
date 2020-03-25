#' Create patient profile report, based on template.
#' 
#' Currently template is only available for SDTM data format. 
#' @param dataPath String with path to data.
#' @param outputFile (optional) String with name of output file,
#' 'subjectProfile.pdf' by default.
#' @param study (optional) String with study name.
#' @param batch (optional) String with batch name.
#' @param author (optional) String with author name.
#' @param overwrite Logical, if TRUE (FALSE by default)
#' the patient profile template is overwritten in the 
#' output directory.
#' @param exportBatchSize Integer, number of subjects
#' for which patient profiles should be created at the same time,
#' 10 by default (to optimize creation of patient profiles).
#' @param subjectSortDataset,subjectSortVar,subjectSortDecreasing (optional) 
#' String with dataset name, variable name by which the subjects should
#' be sorted for the export, based on increasing value if \code{subjectSortDecreasing}
#' is FALSE (TRUE by default).
#' @param subsetDataset,subsetVar,subsetValue,subsetSample 
#' Dataset, variable, corresponding value and random number of subjects
#' to consider to subset subjects
#' for which patient profiles should be created.
#' @return Character vector with output path.
#' The patient profile(s) and the Rmd template
#' are created at \code{outputFile}.
#' @author Laure Cougnaud
#' @importFrom rmarkdown render
#' @export
runPatientProfileTemplateReport <- function(
	dataPath, 
	overwrite = FALSE,
	outputFile = "subjectProfile.pdf",
	study = "custom",
	batch = "X",
	author = NULL,
	subsetDataset = NULL,
	subsetVar = NULL,
	subsetValue = NULL,
	subsetSample = NULL,
	subjectSortDataset = NULL,
	subjectSortVar = NULL,
	subjectSortDecreasing = FALSE,
	exportBatchSize = 10
	){
	
	# use absolute path for data (Rmd change wd)
	dataPath <- normalizePath(dataPath)
	
	# create output directory (if not already created)
	outputDir <- dirname(normalizePath(outputFile, mustWork = FALSE))
	if(!dir.exists(outputDir))	dir.create(outputDir, recursive = TRUE)
	
	pathTemplate <- system.file(
		"template", "patientProfiles-medicalMonitoring.Rmd", 
		package = "medicalMonitoring"
	)
	
	# copy template file in output dir (for reproducibility)
	pathTemplateNew <- file.path(outputDir, basename(pathTemplate))
	if(file.exists(pathTemplateNew) & !overwrite){
		stop(
			"Patient profiles template:", sQuote(basename(pathTemplate)), 
			" already exists in output directory:",
			sQuote(outputDir), ".\n",
			" Please delete this file or use: 'overwrite = TRUE' is you want to re-run the patient profiles."
		)
	}else	file.copy(from = pathTemplate, to = outputDir, overwrite = TRUE)
	
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
			subsetSample = subsetSample,
			subjectSortDataset = subjectSortDataset,
			subjectSortVar = subjectSortVar,
			subjectSortDecreasing = subjectSortDecreasing,
			exportBatchSize = exportBatchSize
		)
	)
	
}