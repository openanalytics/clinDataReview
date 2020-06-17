#' Render a medical monitoring report.
#' 
#' This function is based on the \link[bookdown]{render_book}
#' function, enabling specification of chapter-specific input parameters,
#' specified in YAML configuration files.
#' This consists of:
#' \enumerate{
#' \item{importing the general config file ('config'.yml) to identify
#' each report of interest ('config' tag)}
#' \item{for each report of interest:
#' \itemize{
#' \item{loading the report specific parameter from the associated 'config' file
#' (see the \code{\link{getParamsFromConfig}} function)}
#' \item{executing the report ('template' tag) with the associated
#' parameters in a \strong{new R session for reproducibility}, 
#' to obtain the associated Markdown file}
#' }}
#' \item{combining all Markdown files to a html document
#' (see the \code{\link{convertMdToHtml}} function)
#' }
#' }
#' @param extraDirs Character vector with extra directories required by
#' the report, directory with external images  .
#' By default, the directories: 'figures', 'tables' and 
#' directory included in the 'patientProfilePath' parameter of the
#' general config file are considered.
#' @param configFiles (optional) Character vector with specific config files
#' to be converted from Rmarkdown to Markdown. If
#' \itemize{
#' \item{not specified (by default): }{all config files
#' specified in the general 'config.yml' will be run (Rmd -> md)}
#' \item{specified (\strong{expert use only}): }{only the specified files will be run (Rmd -> md).
#' Other config files mentioned in the general 
#' 'config.yml' file won't be rerun, so the associated 'md' file
#' should be already available in the \code{intermediateDir} folder.
#' }
#' }
#' @inheritParams medicalMonitoring-common-args-report
#' @inherit convertMdToHtml return
#' @author Laure Cougnaud
#' @importFrom rmarkdown render
#' @importFrom xfun Rscript_call
#' @family medical monitoring reporting
#' @export
render_medicalMonitoringReport <- function(
	indexPath = "index.Rmd", 
	outputDir = "./MOMP", intermediateDir = "./interim",
	configDir = "./config", 
	configFiles = NULL, 
	extraDirs = c("figures", "tables")){
	
	if(!dir.exists(configDir))	stop("Config directory doesn't exist.")
	
	if(!dir.exists(outputDir))	dir.create(outputDir, recursive = TRUE)
	outputDir <- normalizePath(outputDir)
	
	if(!dir.exists(intermediateDir))	dir.create(intermediateDir, recursive = TRUE)
	intermediateDir <- normalizePath(intermediateDir)
	#unlink(outputDir, recursive = TRUE)
	
	# extract parameters from general config file
	configGeneralParams <- getParamsFromConfig(configFile = "config.yml", configDir = configDir)
	
	## copy created directory into output directory
	
	# add patient profiles dir to repos to copy
	extraDirs <- c(extraDirs, configGeneralParams$patientProfilePath)
	extraDirs <- extraDirs[dir.exists(extraDirs)]
	
	if(length(extraDirs) > 0)
		file.copy(from = extraDirs, to = outputDir, overwrite = TRUE, recursive = TRUE)

	
	## run index file + each chapter
	
	# consider all config files
	if(is.null(configFiles))
		configFiles <- c("config.yml", configGeneralParams[["config"]])
	
	mdFiles <- c()
	knit_meta_reports <- c()
	for(configFile in configFiles){
		
		if(configFile == "config.yml"){
			
			params <- configGeneralParams
			inputRmdFile <- indexPath
			
		}else{
			
			# extract chapter-specific parameters from config file
			params <- getParamsFromConfig(configFile = configFile, configDir = configDir)
			
			inputRmdFile <- params$template
			
			# extract path of the template from the R package:
			# if(!params$templateCustom)
			
		}
		
		if(is.null(inputRmdFile))
			stop("Template missing for config file: ", sQuote(configFile), ".")
		
		# path to output Md file
		outputMdFile <- getMdFromConfig(
			configFiles = configFile, 
			indexPath = indexPath, 
			intermediateDir = intermediateDir
		)

		# specify report-specific input parameters
		# in a new environment ('params' doesn't work within 'Rscript_call'?)
		# Note: new.env will inherit of parent environments
		# so objects created in global environment also available there
		envReport <- new.env() # parent = baseenv()
		assign("params", params, envir = envReport)
		
		# run report
		message("Run report for config file: ", sQuote(configFile), ".")
		
		# render report
		# call each Rmd doc within a new R session
		# to ensure that current R session doesn't pollute Rmd doc
		outputRmd <- Rscript_call(
			fun = rmarkdown::render, 
			args = list(
				input = inputRmdFile, 
				output_file = basename(outputMdFile),
				output_dir = dirname(outputMdFile),
				run_pandoc = FALSE,
				output_options = list(keep_md = TRUE), # default in rmarkdown >= 2.2
				env = envReport,
				encoding = "UTF-8"
			)
		)
		
		# save knit_meta parameters (contain required Js lib for each report)
		knitMetaReport <- attr(outputRmd, "knit_meta", exact = TRUE)
		knitMetaReportFile <- file.path(
			intermediateDir,
			paste0(file_path_sans_ext(basename(outputMdFile)), ".rds")
		)
		saveRDS(knitMetaReport, file = knitMetaReportFile)
		
	}
	
	## convert all Md files to the HTML report
	message("Convert all Md files to HTML.")
	outputFile <- convertMdToHtml(
		outputDir = outputDir, 
		configDir = configDir, 
		indexPath = indexPath,
		intermediateDir = intermediateDir
	)
	
	return(outputFile)
	
}

#' Get path of the \code{Markdown} file corresponding
#' to a specific config file.
#' 
#' The name of the \code{Markdown} file is based on:
#' \itemize{
#' \item{for the general \code{config.yml} file: }{
#' the basename of the specified \code{indexPath}}
#' \item{for other config file (each sub-report): }{
#' the name of the config file, after removal of the
#' '\code{config-}' part.}
#' }
#' @param configFiles Character vector with name or path of the config file(s).
#' @inheritParams medicalMonitoring-common-args-report
#' @return String with path to the 
#' @importFrom tools file_path_sans_ext
#' @family medical monitoring reporting
#' @author Laure Cougnaud
getMdFromConfig <- function(
	configFiles, indexPath = "index.Rmd", 
	intermediateDir = "./inter"){

	# For general config file, use specified index path
	# For each chapter, file name are derived from the config file name
	files <- ifelse(configFiles == "config.yml", indexPath, sub("^config-", "", configFiles))
	
	paths <- paste0(tools::file_path_sans_ext(basename(files)), ".md")
	paths <- file.path(intermediateDir, paths)
	return(paths)
	
}

#' Get parameters from a config file
#' @param configFile String with filename of the config
#' file of interest.
#' @inheritParams medicalMonitoring-common-args-report
#' @return List with parameters from the specified \code{configFile}
#' and the general config file: \code{config.yml}.
#' @author Laure Cougnaud
#' @importFrom yaml read_yaml
#' @family medical monitoring reporting
#' @export
getParamsFromConfig <- function(
	configFile, configDir = "./config"){

	if(!dir.exists(configDir))
		stop("Config directory:", sQuote(configDir), "doesn't exist.")
	
	# load general config file
	configGeneralPath <- file.path(configDir, "config.yml")
	if(file.exists(configGeneralPath)){
		
		configGeneralParams <- yaml::read_yaml(configGeneralPath)
		
	}else	warning("General config file: 'config.yml' not available in:", 
				configDir, ".", call. = FALSE)

	if(configFile != "config.yml"){

		configFilePath <- file.path(configDir, configFile)
		configParams <- yaml::read_yaml(configFilePath)
		params <- c(configGeneralParams, configParams)
	
	}else{
		
		params <- configGeneralParams
		
	}
	
	return(params)
	
}

#' Convert medical monitoring Markdown files to HTML
#' 
#' This consists of:
#' \enumerate{
#' \item{importing the general config file ('config'.yml) to identify
#' each report of interest ('config' tag)}
#' \item{for each report of interest:
#' checking if the associated _Markdown_ and _rds_ file
#' (list of Js dependencies) are available in \code{intermediateDir}}
#' \item{combining all _Rmarkdown_ report to a single document: _main.md_}
#' \item{converting _main.md_ to the HTML document}
#' }
#' @param mdFiles (optional) Path to the \code{Markdown} files that
#' should be converted. If specified, the specified config files 
#' in \code{configDir} are ignored.
#' @inheritParams medicalMonitoring-common-args-report
#' @return String with path to the front page of the 
#' medical monitoring report.
#' @author Laure Cougnaud
#' @importFrom rmarkdown render
#' @family medical monitoring reporting
#' @export
convertMdToHtml <- function(
	outputDir = "./MOMP", intermediateDir = "./interim",
	configDir = "./config", 
	mdFiles = NULL,
	indexPath = "index.Rmd"){
	
	outputDir <- normalizePath(outputDir)
	if(!dir.exists(outputDir))	dir.create(outputDir)
	
	intermediateDir <- normalizePath(intermediateDir)
	if(!dir.exists(intermediateDir))	dir.create(intermediateDir)
	
	# Extract md files from general config file
	if(is.null(mdFiles)){
		
		configGeneralParams <- getParamsFromConfig(configFile = "config.yml", configDir = configDir)
		configFiles <- c("config.yml", configGeneralParams$config)
		
		mdFiles <- getMdFromConfig(
			configFiles = configFiles, 
			indexPath = indexPath, 
			intermediateDir = intermediateDir
		)
		
	}
	
	# Check if some Md files are missing
	mdFilesMissing <- !file.exists(mdFiles)
	if(any(mdFilesMissing)){
		warning(paste(
			"Markdown file(s):", toString(sQuote(basename(mdFiles[mdFilesMissing]))),
			"are missing, these files are ignored."
		), call. = FALSE)
		mdFiles <- mdFiles[!mdFilesMissing]
	}
	
	# Extract knit_meta params
	interFiles <- file.path(
		intermediateDir,
		paste0(file_path_sans_ext(basename(mdFiles)), ".rds")
	)
	interFilesMissing <- !file.exists(interFiles)
	if(any(interFilesMissing)){
		warning(paste0(
			"No intermediate file available for the reports: ", 
			toString(sQuote(basename(interFiles[interFilesMissing]))), "."
		), call. = FALSE)
		interFiles <- interFiles[interFilesMissing]
	}
	knit_meta_reports <- c()
	for(interFile in interFiles){
		knitMetaReport <- readRDS(interFile)
		knit_meta_reports <- c(knit_meta_reports, knitMetaReport)
	}
	
	
	# combine all Md documents into one single Md document
	mdContentList <- lapply(mdFiles, readLines, encoding = 'UTF-8', warn = FALSE)
	mdContent <- do.call(c, mdContentList)
	# Save Md file in output directory to avoid the:
	# rmarkdown::render error: 
	# The path [intermediateDir]/libs/X does not appear to be a descendant of [outputDir]
	mdMainFile <- file.path(outputDir, "main.md")
	writeLines(text = mdContent, con = mdMainFile, useBytes = TRUE)
	
	# convert Markdown -> html 
	outputFile <- rmarkdown::render(
		input = mdMainFile, 
		output_file = "index.html",
		output_dir = outputDir,
		run_pandoc = TRUE, 
		knit_meta = knit_meta_reports,
		encoding = "UTF-8"
	)
	res <- file.remove(mdMainFile)
	
	return(outputFile)
	
}

#' Common parameters for the medical monitoring reporting function
#' @param indexPath String with path to the index file,
#' ('index.Rmd' by default).
#' @param configDir String with directory with config files,
#' ('config' by default).
#' It should contain a general 'config.yml' file and dedicated
#' 'config-[X].yml' for each chapter.
#' The order of each chapter is specified in the 'config' slot in the general 
#' general 'config.yml'.
#' @param outputDir String with output directory,
#' ('MOMP', for: 'Medical Oversight and monitoring Plan' by default).
#' @param intermediateDir String with intermediate directory ('inter'
#' by default), where
#' markdown files and rds file specifying Js libraries (with \code{knit_meta}) for
#' each sub report are stored.
#' @name medicalMonitoring-common-args-report
NULL
