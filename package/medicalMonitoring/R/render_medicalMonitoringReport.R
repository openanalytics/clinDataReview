#' Render a medical monitoring report.
#' 
#' This function is based on the \link[bookdown]{render_book}
#' function, enabling specification of chapter-specific input parameters,
#' specified in YAML configuration files.
#' @param indexPath String with path to the index file,
#' ('index.Rmd' by default).
#' @param outputDir String with output directory,
#' ('MOMP', for: 'Medical Oversight and monitoring Plan' by default).
#' @param configDir String with directory with config files,
#' ('config' by default).
#' It should contain a general 'config.yml' file and dedicated
#' 'config-[X].yml' for each chapter.
#' The order of each chapter is specified in the 'config' slot in the general 
#' general 'config.yml'.
#' @param extraDirs Character vector with extra directories required by
#' the report, directory with external images  .
#' By default, the directories: 'figures', 'tables' and 
#' directory included in the 'patientProfilePath' parameter of the
#' general config file are considered.
#' @param ... Extra parameters for the link[rmarkdown]{render} function.
#' @return String with path to the medical monitoring report.
#' @author Laure Cougnaud
#' @importFrom tools file_path_sans_ext
#' @importFrom rmarkdown render
#' @importFrom yaml read_yaml
#' @export
render_medicalMonitoringReport <- function(
	indexPath = "index.Rmd", 
	outputDir = "./MOMP",
	configDir = "./config", 
	extraDirs = c("figures", "tables"), ...){
	
	if(!dir.exists(configDir))	stop("Config directory doesn't exist.")
	
	if(!dir.exists(outputDir))	dir.create(outputDir, recursive = TRUE)
	#unlink(outputDir, recursive = TRUE)
		
	# get path of Markdown doc from Rmd doc + output dir
	getMdFile <- function(RmdFile){
		path <- paste0(tools::file_path_sans_ext(basename(RmdFile)), ".md")
		path <- file.path(outputDir, path)
		return(path)
	}
	
	# load general config file
	configGeneralPath <- file.path(configDir, "config.yml")
	if(file.exists(configGeneralPath)){
		
		configGeneralParams <- yaml::read_yaml(configGeneralPath)
		# add patient profiles dir to repos to copy
		extraDirs <- c(extraDirs, configGeneralParams$patientProfilePath)
		
	}else	stop("General config file not available in:", configDir, ".")
	
	# render Rmd report to Rmd
	renderToMd <- function(input, output_file, params, ...){
		
		# specify report-specific input parameters
		# in a new environment
		envReport <- new.env()
		assign("params", params, envir = envReport)
		
		# render report
		outputRmd <- rmarkdown::render(
			input = input, 
			output_file = output_file,
			run_pandoc = FALSE,
			output_options = list(keep_md = TRUE),
#			params = configGeneralParams,
			env = envReport,
      		encoding = "UTF-8",
			...
		)
		return(attr(outputRmd, "knit_meta"))
	}
	
	# run index document
	outputMdFile <- getMdFile(indexPath)
	knitMetaIndex <- renderToMd(
		input = indexPath, 
		output_file = outputMdFile,
		params = configGeneralParams
	)
	mdFiles <- outputMdFile
	knit_meta_reports <- knitMetaIndex

	# run each chapter
	for(configFile in configGeneralParams[["config"]]){
		
		configFilePath <- file.path(configDir, configFile)
		
		# extract chapter-specific parameters from config file
		configParams <- c(
			configGeneralParams,
			yaml::read_yaml(configFilePath)
		)
		inputRmdFile <- configParams$template
		if(is.null(inputRmdFile))
			stop("Template missing for config file: ", sQuote(configFile), ".")
		# extract path of the template from the R package:
#		if(!configParams$templateCustom)
		
		outputMdFile <- getMdFile(sub("^config-", "", configFile))
		
		# run report
		message("Run report for config file", sQuote(configFile), ".")
		knitMetaReport <- renderToMd(
			input = inputRmdFile, 
			output_file = outputMdFile,
			params = configParams
		)
		mdFiles <- c(mdFiles, outputMdFile)
		knit_meta_reports <- c(knit_meta_reports, knitMetaReport)
		
	}
		
	# combine all Md documents into one single Md document
	mdContentList <- lapply(mdFiles, readLines, encoding = 'UTF-8', warn = FALSE)
	mdContent <- do.call(c, mdContentList)
	mdMainFile <- file.path(outputDir, "main.md")
	writeLines(text = mdContent, con = mdMainFile,useBytes = TRUE)
	
	# copy created directory into output directory
	file.copy(from = extraDirs, to = outputDir, overwrite = TRUE, recursive = TRUE)
	
	# convert Markdown -> html 
	outputFile <- rmarkdown::render(
		input = mdMainFile, 
		run_pandoc = TRUE, 
		knit_meta = knit_meta_reports,
		encoding = "UTF-8"
	)
	
	return(outputFile)
	
}
