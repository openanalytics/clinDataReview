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
	
	# add patient profiles dir to repos to copy
	configGeneralParams <- getParamsFromConfig(configFile = "config.yml", configDir = configDir)
	extraDirs <- c(extraDirs, configGeneralParams$patientProfilePath)

	configFiles <- c("config.yml", configGeneralParams[["config"]])
	
	## run index file + each chapter
	
	mdFiles <- c()
	knit_meta_reports <- c()
	for(configFile in configFiles){
		
		if(configFile == "config.yml"){
			
			params <- configGeneralParams
			inputRmdFile <- indexPath
			outputMdFile <- getMdFile(indexPath)
			
		}else{
			
			# extract chapter-specific parameters from config file
			params <- getParamsFromConfig(configFile = configFile, configDir = configDir)
			
			inputRmdFile <- params$template
			outputMdFile <- getMdFile(sub("^config-", "", configFile))
			
			# extract path of the template from the R package:
			# if(!params$templateCustom)
			
		}
		
		if(is.null(inputRmdFile))
			stop("Template missing for config file: ", sQuote(configFile), ".")

		# specify report-specific input parameters
		# in a new environment
		envReport <- new.env()
		assign("params", params, envir = envReport)
		
		# run report
		message("Run report for config file: ", sQuote(configFile), ".")
		
		# render report
		outputRmd <- rmarkdown::render(
			input = inputRmdFile, 
			output_file = outputMdFile,
			run_pandoc = FALSE,
			output_options = list(keep_md = TRUE),
			env = envReport,
			encoding = "UTF-8"
		)
		
		# and knit_meat attributes
		knit_meta_reports <- c(knit_meta_reports, attr(outputRmd, "knit_meta"))
		
		# store Markdown file path
		mdFiles <- c(mdFiles, outputMdFile)
		
		# remove objects created in Rmd file
#		envReportEnd <- environment()
#		objReportEnd <- ls(envir = envReportEnd)
#		rm(list = setdiff(objReportEnd, c("envFct", "knit_meta_reports", "mdFiles")))
#		attach(envFct)
		
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

#' Get parameters from a config file
#' @param configFile String with filename of the config
#' file of interest.
#' @param configDir String with directory with config files,
#' ('config' by default).
#' @return List with parameters from the specified \code{configFile}
#' and the general config file: \code{config.yml}.
#' @author Laure Cougnaud
#' @importFrom yaml read_yaml
#' @export
getParamsFromConfig <- function(
	configFile, configDir = "./config"){
	
	# load general config file
	configGeneralPath <- file.path(configDir, "config.yml")
	if(file.exists(configGeneralPath)){
		
		configGeneralParams <- yaml::read_yaml(configGeneralPath)
		
	}else	warning("General config file: 'config.yml' not available in:", configDir, ".")

	if(configFile != "config.yml"){

		configFilePath <- file.path(configDir, configFile)
		configParams <- yaml::read_yaml(configFilePath)
		params <- c(configGeneralParams, configParams)
	
	}else{
		
		params <- configGeneralParams
		
	}
	
	return(params)
	
}
