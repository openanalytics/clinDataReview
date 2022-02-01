#' Render a clinical data report.
#' 
#' @section Framework: 
#' This function is based on the \link[bookdown]{render_book}
#' function, enabling specification of chapter-specific input parameters,
#' specified in YAML configuration files.
#' This consists of:
#' \enumerate{
#' \item{importing the general config file ('config'.yml) to identify
#' each report of interest ('config' tag)}
#' \item{for each report of interest:
#' \itemize{
#' \item{loading the report specific parameters from the associated 'config' file
#' (see the \code{\link{getParamsFromConfig}} function)}
#' \item{if the template should be extracted from a specified package
#' (\code{templatePackage} tag), this template is copied to the
#' current directory.
#' Please note that if a file with same name is available in
#' the working directory, this file will be overwritten.
#' }
#' \item{executing the report ('template' tag) with the associated
#' parameters in a \strong{new R session for reproducibility}, 
#' to obtain the associated Markdown file}
#' }}
#' \item{combining all Markdown files to a html document
#' (see the \code{\link{convertMdToHtml}} function)
#' }
#' }
#' If the execution of a specific report fails with error, 
#' a warning message is triggered. A report containing
#' only the specified title is created, to ensure
#' output consistency (especially html file numbering)
#' in case the report succeeds.
#' @section Available template report:
#' see \strong{\code{? `clinDataReview-templates`}} for list of 
#' clinical data template report available in the package.
#' @param extraDirs Character vector with extra directories required by
#' the report, directory with external images  .
#' By default, the directories: 'figures', 'tables' and mentioned in the 
#' 'patientProfilePath' parameter of the
#' general config file are included.
#' All these folders should be available in \code{inputDir}.
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
#' @param quiet Logical, if TRUE (FALSE by default)
#' messages during the execution of each report are not displayed
#' in the console (see \code{\link[rmarkdown]{render}}).
#' @param logFile (optional) String with path to a log file,
#' where output (also error/messages/warnings) should be stored.
#' If specified, the entire output is re-directed to this file.
#' @inheritParams clinDataReview-common-args-report
#' @inherit convertMdToHtml return
#' @author Laure Cougnaud
#' @importFrom rmarkdown render
#' @family clinical data reporting
#' @export
render_clinDataReviewReport <- function(
	configFiles = NULL, 
	configDir = file.path(inputDir, "config"), 
	logFile = NULL,
    indexPath = file.path(inputDir, "index.Rmd"), 
	inputDir = ".",
    outputDir = "./report", 
	intermediateDir = "./interim",
    extraDirs = file.path(inputDir, c("figures", "tables")),
    quiet = FALSE){
  
  # log output
  if(!is.null(logFile)){
    logFileCon <- file(logFile, "w")
    sink(file = logFileCon, split = FALSE, type = "message")
    sink(file = logFileCon, split = FALSE, type = "output", append = TRUE)
    on.exit({sink(type = "output");sink(type="message")})
	# quiet option is set to FALSE to ensure output is saved in the log file
	if(quiet)	quiet <- FALSE
  }
  
  if(!dir.exists(configDir))	stop("Config directory doesn't exist.")
  
  if(!dir.exists(outputDir))	dir.create(outputDir, recursive = TRUE)
  outputDir <- normalizePath(outputDir)
  
  if(!dir.exists(intermediateDir))	dir.create(intermediateDir, recursive = TRUE)
  intermediateDir <- normalizePath(intermediateDir)
  
  # extract parameters from general config file
  configGeneralParams <- getParamsFromConfig(
	configFile = "config.yml", 
	configDir = configDir,
	inputDir = inputDir
 )
  
  ## copy created directory into output directory
  
  # add patient profiles dir to repos to copy
	patientProfilePath <- file.path(inputDir, configGeneralParams$patientProfilePath)
	extraDirs <- unique(c(extraDirs, patientProfilePath))
  
  ## run index file + each chapter
  
  # consider all config files
  if(is.null(configFiles))
    configFiles <- c("config.yml", configGeneralParams[["config"]])
  
  configFiles <- checkTemplatesName(configFiles = configFiles, configDir = configDir)
  
  # check uniqueness of report titles
  reportTitles <- checkReportTitles(configFiles, configDir = configDir)
  
  mdFiles <- c()
  knit_meta_reports <- c()
  for(configFile in configFiles){
    
    runDocument <- TRUE
    if(configFile == "config.yml"){
      
      params <- configGeneralParams
      inputRmdFile <- indexPath
      
    }else{
      
      # extract chapter-specific parameters from config file
      resParams <- try(
          params <- getParamsFromConfig(
			configFile = configFile, 
			configDir = configDir,
			inputDir = inputDir
		  )
          , silent = TRUE)
      
      if(inherits(resParams, "try-error")){
        
        warning(
            paste0("Extraction of the parameters from config file: ", 
                sQuote(configFile), " failed (see error below), ",
                "this report is ignored\n",
				attr(resParams, "condition")$message
            ), immediate. = TRUE, call. = FALSE
        )
        runDocument <- FALSE
        
      }else{
        
        inputRmdFile <- params$template
        
      }
      
    }
    
    if(runDocument && is.null(inputRmdFile)){
      warning("Template missing for config file: ", sQuote(configFile), ".")
      runDocument <- FALSE
    }
    
    # Extract template from package
    if(runDocument){
		
		if(configFile != "config.yml")
			inputRmdFile <- file.path(inputDir, inputRmdFile)
      
      # Extract the template from the package if: 'templateFromPackage' is 'true'
      if(!is.null(params$templatePackage)){
        
        pathTemplate <- clinDataReview::getPathTemplate(
            file = basename(inputRmdFile), 
            package = params$templatePackage
        )
        if(!file.exists(pathTemplate)){
          
          runDocument <- FALSE
          
        }else{
			
			if(file.exists(inputRmdFile))
				warning(paste("Document with similar name than",
					"specified template from", sQuote(params$templatePackage),
					"for config file: ", sQuote(configFile),
					"is already available in the working directory,",
					"this document will be overwritten."
				), immediate. = TRUE, call. = FALSE)
          
          # copy file to working directory
          tmp <- file.copy(from = pathTemplate, to = inputDir, overwrite = TRUE)
		  
          # Extract the config file
          configSpecFile <- file_path_sans_ext(basename(inputRmdFile))
          configSpecFile <- paste0(configSpecFile, ".json")
          pathConfigSpecFile <- clinDataReview::getPathTemplate(
              file = configSpecFile, 
              package = params$templatePackage
          )
          
          # Check config parameters
          if(file.exists(pathConfigSpecFile)){
            resCheck <- try(
                checkConfigFile(
                    configFile = file.path(configDir, configFile), 
                    configSpecFile = pathConfigSpecFile
                ), silent = TRUE
            )
            if(inherits(resCheck, "try-error")){
              warning(
                  paste0("The report for the config file: ", 
                      sQuote(basename(configFile)), 
                      " is not created because the check of the parameters",
                      " failed with the error: ",
                      attr(resCheck, "condition")$message
                  ), immediate. = TRUE, call. = FALSE
              )
              runDocument <- FALSE
            }
            
          }else{
            warning(
                "No config parameter available, ",
                "input parameters for the report are not checked."
                , immediate. = TRUE, call. = FALSE)
          }
        }
      }
      
    }
    
    # execute the report
    if(runDocument){
      
      # path to output Md file
      outputMdFile <- getMdFromConfig(
          configFiles = configFile, 
          indexPath = indexPath, 
          intermediateDir = intermediateDir
      )
      
      # run report
      message("Run report for config file: ", sQuote(configFile), ".")
      
      # render report
      # call each Rmd doc within a new R session
      # to ensure that current R session doesn't pollute Rmd doc		
      resRender <- try(
          outputRmd <- renderInNewSession(
              input = inputRmdFile, 
              output_file = basename(outputMdFile),
              output_dir = dirname(outputMdFile),
              quiet = quiet,
			  params = params
          ),
          silent = TRUE
      )
      
      # save knit_meta parameters (contain required Js lib for each report)
      interimResFile <- file.path(
          intermediateDir,
          paste0(file_path_sans_ext(basename(outputMdFile)), ".rds")
      )
      
      if(inherits(resRender, "try-error")){
        
        warning(
            paste0("Rendering of the ", sQuote(basename(inputRmdFile)),
                " report for config file: ", sQuote(configFile), " failed, a report",
                " with only the section title is created."
            ), immediate. = TRUE, call. = FALSE
        )
        
        # create a report only with a section title
        pathTemplate <- clinDataReview::getPathTemplate("divisionTemplate.Rmd")
        tmp <- file.copy(from = pathTemplate, to = inputDir, overwrite = TRUE)
        params$content <- "**This part of the report could not be created.**"
        outputRmd <- renderInNewSession(
            input = basename(pathTemplate), 
            output_file = basename(outputMdFile),
            output_dir = dirname(outputMdFile),
            quiet = quiet,
			params = params
        )
        
      }
      
      knitMetaReport <- attr(outputRmd, "knit_meta", exact = TRUE)
      sessionInfoReport <- attr(outputRmd, "sessionInfo", exact = TRUE)
      interimRes <- list(knitMeta = knitMetaReport, sessionInfo = sessionInfoReport)
      saveRDS(interimRes, file = interimResFile)
      
    }
    
  }
  
  # copy output directories before rendering the full report
  # e.g. if output dir contain logo required for full report
  extraDirs <- extraDirs[file.exists(extraDirs)]
  if (length(extraDirs) > 0){
	  tmp <- file.copy(
		from = extraDirs, to = outputDir, 
		overwrite = TRUE, recursive = TRUE
	  )
  }
  
  ## convert all Md files to the HTML report
  message("Convert all Md files to HTML.")
  # pandoc print text in console
  outputFile <- convertMdToHtml(
      outputDir = outputDir, 
      configDir = configDir, 
      indexPath = indexPath,
      intermediateDir = intermediateDir,
	  quiet = quiet
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
#' @inheritParams clinDataReview-common-args-report
#' @return String with path to the 
#' @importFrom tools file_path_sans_ext
#' @author Laure Cougnaud
getMdFromConfig <- function(
    configFiles, indexPath = "index.Rmd", 
    intermediateDir = "./interim"){
  
  # For general config file, use specified index path
  # For each chapter, file name are derived from the config file name
  files <- ifelse(configFiles == "config.yml", indexPath, sub("^config-", "", configFiles))
  
  paths <- paste0(tools::file_path_sans_ext(basename(files)), ".md")
  paths <- file.path(intermediateDir, paths)
  return(paths)
  
}

#' Get parameters from a config file
#' 
#' Please note that the information from this config file
#' and the general config file: \code{config.yml}
#' are considered.\cr
#' In case parameters are defined both in the general
#' and specific config files, the parameter from the
#' general config file is ignored.
#' @param configFile String with filename of the config
#' file of interest in YAML format.
#' @inheritParams clinDataReview-common-args-report
#' @return List with parameters from the specified \code{configFile}
#' and the general config file: \code{config.yml}.\cr
#' There are two specific handlers:
#' \itemize{
#' \item{parameters tagged with '[param] !r [value]'
#' are evaluated in R, and their evaluated value is returned}
#' \item{parameters tagged with '[param] !r-lazy [value]'
#' are imported as character, and need to be further process
#' with \code{\link{forceParams}} inside the report.}
#' }
#' Parameters with YAML type 'r-lazy' are imported as character,
#' with this additional class.
#' @seealso \link{forceParams}
#' @author Laure Cougnaud
#' @importFrom yaml read_yaml
#' @family clinical data reporting
#' @export
getParamsFromConfig <- function(
	configFile, 
	configDir = file.path(inputDir, "config"),
	inputDir = "."){
  
  if(!dir.exists(configDir))
    stop("Config directory: ", sQuote(configDir), " doesn't exist.")
  
	lazyRHandlers <- list(
		# as the Rmd !r default handler
		`r` = function(x){
			eval(str2expression(text = x))
		},
		# version with lazy-evaluation
		# Note: would be 'cleaner' to store expression rather than character
		# but rapply doesn't work with 'call' type of parameters	
		`r-lazy` = function(x){
			structure(x, class = c("r-lazy", class(x)))
		}
	)

  # load general config file
  configGeneralPath <- file.path(configDir, "config.yml")
  if(file.exists(configGeneralPath)) {
    
    configGeneralParams <- yaml::read_yaml(configGeneralPath, handlers = lazyRHandlers)
    
  } else {
    warning("General config file: 'config.yml' not available in:", 
        configDir, ".", call. = FALSE)
    configGeneralParams <- NULL
  }
  
  if(configFile != "config.yml") {
    
    configFilePath <- file.path(configDir, configFile)
    
    if(file.exists(configFilePath)) {
      configParams <- yaml::read_yaml(configFilePath, handlers = lazyRHandlers)
    } else {
      stop("File ", sQuote(configFilePath), " cannot be found. \n",
          "Please check the spelling is correct ",
          "or the file is saved in the directory with the other config files."
      )
    }
	
	paramsDuplicated <- intersect(names(configGeneralParams), names(configParams))
	if(length(paramsDuplicated) > 0){
		message(paste(
			"Parameter(s):", toString(shQuote(paramsDuplicated)), "are",
			"both defined in the general and chapter-specific config",
			"file.\nThe parameter(s) from the chapter-specific config file are considered."
		))
		configGeneralParams <- configGeneralParams[
			-which(names(configGeneralParams) %in% paramsDuplicated)
		]
	}
	
    params <- c(configGeneralParams, configParams)
    
  } else {
    
    params <- configGeneralParams
    
  }
  
  return(params)
  
}

#' Force the evaluation of the
#' parameters from config file.
#' 
#' This function is only useful if some
#' parameters should be lazy-evaluated in the report.
#' These parameters should have the class: \code{r-lazy}.
#' A typical use case is a parameter that
#' consists of a R expression
#' depending on objects created in a template
#' report (typically \code{data}).
#' \cr
#' Parameters are searched in the environment
#' in which this function is called from.
#' @param params List of parameters as obtained
#' via the \code{\link{getParamsFromConfig}}
#' function.
#' @return Input parameter list, with
#' object(s) of class \code{r-lazy}
#' evaluated.
#' @examples 
#' data <- mtcars
#' params <- list(label = "Cars dataset", nrow = structure("nrow(data)", class = "r-lazy"))
#' str(params)
#' str(forceParams(params))
#' @author Laure Cougnaud
#' @seealso \link{getParamsFromConfig}
#' @family clinical data reporting
#' @export
forceParams <- function(params){
	
	envirParent <- parent.frame(n = 1)

	paramsEval <- rapply(
		object = params, 
		f = function(x)	eval(parse(text = x), envir = envirParent),
		classes = "r-lazy",
		how = "replace"
	)

	return(paramsEval)
	
}

#' Convert clinical data Markdown files to HTML
#' 
#' This consists of:
#' \enumerate{
#' \item{importing the general config file ('config'.yml) to identify
#' each report of interest ('config' tag)}
#' \item{for each report of interest:
#' checking if the associated \code{Markdown} and \code{rds} file
#' (list of Js dependencies) are available in \code{intermediateDir}}
#' \item{combining all \code{Rmarkdown} reports to a single document: \code{main.md}}
#' \item{converting \code{main.md} to an HTML document}
#' }
#' @param mdFiles (optional) Path to the \code{Markdown} files that
#' should be converted. If specified, the specified config files 
#' in \code{configDir} are ignored.
#' @param ... Any parameters passed to \code{\link[rmarkdown]{render}},
#' for expert use only.
#' @inheritParams clinDataReview-common-args-report
#' @return String with path to the front page of the 
#' clinical data report.
#' @author Laure Cougnaud
#' @importFrom rmarkdown render
#' @family clinical data reporting
#' @export
convertMdToHtml <- function(
    outputDir = "./report", 
	inputDir = ".",
    configDir = file.path(inputDir, "config"), 
	indexPath = file.path(inputDir, "index.Rmd"),
	intermediateDir = "./interim",
    mdFiles = NULL,
	...
    ){
  
  outputDir <- normalizePath(outputDir)
  if(!dir.exists(outputDir))	dir.create(outputDir)
  
  intermediateDir <- normalizePath(intermediateDir)
  if(!dir.exists(intermediateDir))	dir.create(intermediateDir)
  
  # Extract md files from general config file
  if(is.null(mdFiles)){
    
    configGeneralParams <- getParamsFromConfig(
		configFile = "config.yml", 
		configDir = configDir,
		inputDir = inputDir
    )
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
        ), call. = FALSE, immediate. = TRUE)
    mdFiles <- mdFiles[!mdFilesMissing]
  }
  
  interimResFiles <- file.path(
      intermediateDir,
      paste0(file_path_sans_ext(basename(mdFiles)), ".rds")
  )
  interimResFileMissing <- !file.exists(interimResFiles)
  if(any(interimResFileMissing)){
    warning(paste0(
            "No intermediate file available for the reports: ", 
            toString(sQuote(basename(interimResFiles[interimResFileMissing]))), "."
        ), call. = FALSE)
    interimResFiles <- interimResFiles[interimResFileMissing]
  }
  knit_meta_reports <- c();sessionInfoReports <- list()
  for(file in interimResFiles){
    interimFile <- readRDS(file)
    interimKnitMeta <- interimFile$knitMeta
    if(!is.null(interimKnitMeta))
      knit_meta_reports <- c(knit_meta_reports, interimKnitMeta)
    interimSessionInfo <- interimFile$sessionInfo
    if(!is.null(interimSessionInfo))
      sessionInfoReports <- c(sessionInfoReports, list(interimSessionInfo))
  }
  
  # include session information in the report
  sessionInfoMd <- exportSessionInfoToMd(
      sessionInfos = sessionInfoReports, 
      mdFiles = mdFiles, 
      intermediateDir = intermediateDir,
	  ...
  )
  mdFiles <- c(mdFiles, sessionInfoMd)
  
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
      encoding = "UTF-8",
	  ...
  )
  res <- file.remove(mdMainFile)
  
  return(outputFile)
  
}

#' Checks of config files template.
#' 
#' Check if the templates specified in the input config files
#' don't originate from multiple sources (e.g. custom and R package
#' via the parameter \code{templatePackage}).
#' If so, the corresponding config files are not considered.
#' @param configFiles Character vector with name or path of the config file(s).
#' @inheritParams getParamsFromConfig
#' @return Updated \code{configFiles}
#' @author Laure Cougnaud
checkTemplatesName <- function(configFiles, 
	configDir = file.path(inputDir, "config"),
	inputDir = "."){
  
  configFilesWithTemplate <- setdiff(configFiles, "config.yml")
  
  # check report name
  configTemplateInfoList <- sapply(configFilesWithTemplate, function(configFile){
        
        res <- try(
            params <- getParamsFromConfig(
				configFile = configFile, 
				configDir = configDir,
				inputDir = inputDir
			),
            silent = TRUE
        )
        
        if(!inherits(res, "try-error") && "template" %in% names(params)) {
          
          templatePackage <- params[["templatePackage"]]
          if(is.null(templatePackage))	templatePackage <- ""
          data.frame(
              configFile = configFile, 
              template = params$template, 
              templatePackage = templatePackage,
              stringsAsFactors = FALSE
          )
          
        } else {
			warning("Import of parameters from config file ", sQuote(configFile), 
				" failed with error:\n",
				attr(res, "condition")$message
			)
			data.frame()
		}
      }, simplify = FALSE)
  
  configTemplateInfo <- do.call(rbind.data.frame, configTemplateInfoList)
  
  if(nrow(configTemplateInfo) > 0){
	  
	  nPkgByTemplate <- with(configTemplateInfo, 
	      tapply(templatePackage, template, function(x) length(unique(x)))
	  )
	  templateWithMultPkg <- names(which(nPkgByTemplate > 1))
	  
	  if(length(templateWithMultPkg) > 0){
	    
	    configFilesRemoved <- configTemplateInfo[which(configTemplateInfo$template %in% templateWithMultPkg), "configFile"]
	    warning(paste0(
	            "The following config file(s) are ignored, because the ",
	            "same template name is used for a custom template or a template ",
	            "within the package(s): ", toString(sQuote(configFilesRemoved)), "."
	        ))
	    configFiles <- setdiff(configFiles, configFilesRemoved)
	    
	  }
	  
  }
  
  return(configFiles)
  
  
}

#' Check report titles
#'
#' Check uniqueness of report titles across the config files.
#' If not unique titles are provided, an error is returned.
#' @param configFiles Character vector with config file names
#' @param configDir String with directory with config files,
#' ('config' by default)
#' @inheritParams clinDataReview-common-args-report
#' @return A named vector with the report titles and the corresponding config file
#' @author Michela Pasetto
#' @family clinical data reporting
#' @export 
checkReportTitles <- function(
	configFiles, 
	configDir = file.path(inputDir, "config"),
	inputDir = ".") {
  
  configFileNames <- configFiles[configFiles != "config.yml"]
  
  reportTitles <- sapply(configFileNames, function(configFileI) {
        
        res <- try(            
            configParams <- getParamsFromConfig(
				configFile = configFileI, 
				configDir = configDir,
				inputDir = inputDir
			),
            silent = TRUE)
        
        if(!inherits(res, "try-error")) {
          configParams$reportTitle
        } else {
          warning("Import of parameters from config file ", sQuote(configFileI), 
		 	" failed with error:\n",
			attr(res, "condition")$message
          )
		  NULL
        }
        
      })
  
  isTitleDuplicated <- any(duplicated(reportTitles))
  
  if(isTitleDuplicated) {
    
    idxDuplicatedTitle <- which(duplicated(reportTitles))
    nameDuplicatedTitle <- reportTitles[idxDuplicatedTitle]
    
    stop("The title in ", toString(nameDuplicatedTitle), " is duplicated.")
    
  }
  return(reportTitles)
}

#' Render a rmarkdown doc in a new session,
#' with the possibility to save output in a log file,
#' and saving also session information.
#' 
#' Note: this function is inspired from \code{xfun::Rscript_call}
#' @param input Input file to be rendered.
#' @param run_pandoc Logical, if TRUE (FALSE by default)
#' convert Md to specified output with pandoc.
#' @param output_options List of output options,
#' by default 'keep_md = TRUE' (keep Markdown file)
#' @param encoding String with encoding, 'UTF-8' by default.
#' @param params List with input parameters for this document.\cr
#' These parameters should be accessed in the Rmd document via
#' \code{params$...}.\cr
#' These parameters will be saved to a RDS file and imported 
#' during the report rendering.
#' @param ... Any extra parameters passed to \code{\link[rmarkdown]{render}},
#' for expert use only.
#' @return Output of the function executed in the new R session 
#' with additional attribute: 'sessionInfo' containing
#' the details of the session information in the separated R session.
#' If the report fails, an error message is returned.
#' @importFrom tools file_path_sans_ext
#' @importFrom utils hasName
#' @author Laure Cougnaud
renderInNewSession <- function(
    input, 
    run_pandoc = FALSE,
    output_options = list(keep_md = TRUE),
    encoding = "UTF-8",
	params = NULL,
    ...){
  
	argsRenderExtra <- list(...)
	argsRender <- c(
		list(
			input = input, 
			run_pandoc = run_pandoc,
			output_options = output_options,
			encoding = encoding
		),
		argsRenderExtra
	)
  
  ## Run job in a separated session with Rscript
  
  # which requires a R script:
  RFile <- tempfile(fileext = '.R')
  cat(
      'local({
          inputArgs <- commandArgs(trailingOnly = TRUE)
          
          inputFile <- inputArgs[[1]]
          input <- readRDS(inputFile)	
          fct <- input$fct	
          params <- input$params

          res <- do.call(fct, input$args)

          attr(res, "sessionInfo") <- sessionInfo()
          
          outputFile <- inputArgs[[2]]
          saveRDS(object = res, file = outputFile)
          
          })'
      , file = RFile)
  
  # and input parameters:
  inputFile <- tempfile(fileext = '.rds')
  outputFile <- tempfile(fileext = '.rds')
  on.exit(unlink(c(outputFile, inputFile, RFile)))
  IOFiles <- list(input = inputFile, output = outputFile)
  
  # store fct and input parameters in a temporary file
  saveRDS(
	object = list(
		fct = rmarkdown::render, 
		args = argsRender,
		params = params
	), 
	file = inputFile
	)
  
  # run in separated session
  # capture output/messages/errors in output
  # otherwise these are not included in the logFile (with sink)
  RscriptPath <- file.path(R.home('bin'), 'Rscript')
  stderrout <- system2(
      command = RscriptPath, 
      args = shQuote(c(RFile, IOFiles, "--verbose")),
      # to store output (including error) in an object
      stderr = TRUE, stdout = TRUE
  )
  if(!(hasName(argsRenderExtra, "quiet") && isTRUE(argsRenderExtra[["quiet"]])))
  	cat(stderrout, sep = "\n")
  
  resStatus <- attr(stderrout, "status")
  if(!is.null(resStatus) && resStatus != 0)
    stop("Creation of the report failed.")
  
  # load output
  res <- if(file.exists(outputFile))	readRDS(outputFile)
  
  # clean intermediary file
  resFile <- as.character(res)
  if(grepl(".knit.md", resFile) && file.exists(resFile))
    file.remove(resFile)
  
  return(res)
  
}

#' Merge multiple session information
#' @param ... objects of type \code{\link{sessionInfo}}
#' @return \code{\link{sessionInfo}} with combined information
#' @author Laure Cougnaud
merge.sessionInfo <- function(...){
  
  sessionInfoAll <- do.call(mapply, c(
          list(FUN = function(...) unique(c(...)), SIMPLIFY = FALSE), 
          list(...)
      ))
  class(sessionInfoAll) <- c("sessionInfo", class(sessionInfoAll))
  
  return(sessionInfoAll)
  
}

#' Combine all session informations across all clinical data reports
#' and export them into a dedicated Markdown document
#' @param sessionInfos List with \code{\link{sessionInfo}} objects
#' @param mdFiles Character vector with Markdown files
#' @param ... Any parameters passed to \code{\link{renderInNewSession}},
#' for expert use only.
#' @inheritParams clinDataReview-common-args-report
#' @return String with path to Markdown file containing the session information,
#' NULL if no session information(s) are provided.
#' @author Laure Cougnaud
exportSessionInfoToMd <- function(sessionInfos, mdFiles,
	intermediateDir = "interim", ...){
  
  if(length(sessionInfos) > 0){
    
    # combine session informations
    sessionInfoAll <- do.call(merge, sessionInfos)
    sessionInfoFile <- tempfile("sessionInfo", fileext = ".Rmd")
    cat(
        '\n\n# Appendix  \n\n## Session information  \n\n',
        '```{r, echo = FALSE, results = "asis"}\nparams$sessionInfoAll\n```', 
        file = sessionInfoFile
    )
    sessionInfoMd <- "sessionInfo.md"
    outputRmd <- renderInNewSession(
        input = sessionInfoFile, 
        output_file = sessionInfoMd, 
        output_dir = intermediateDir, 
		params = list(sessionInfoAll = sessionInfoAll),
		...
    )
    
    sessionInfoMdPath <- file.path(intermediateDir, sessionInfoMd)
    
  }else	sessionInfoMdPath <- NULL
  return(sessionInfoMdPath)
#	reportFirst <- readLines(mdFiles[1], encoding = 'UTF-8', warn = FALSE)
#	sessionInfoMd <- readLines(file.path(intermediateDir, "sessionInfo.md"), encoding = 'UTF-8', warn = FALSE)
#	writeLines(text = c(reportFirst, sessionInfoMd), con = mdFiles[1], useBytes = TRUE)
  
}

#' Common parameters for the clinical data reporting function
#' @param indexPath String with path to the index file,
#' by default 'index.Rmd' in \code{inputDir}.
#' @param configDir String with directory with config files,
#' by default a 'config' folder in \code{inputDir}.\cr
#' It should contain a general 'config.yml' file and dedicated
#' 'config-[X].yml' for each chapter.
#' The order of each chapter is specified in the 'config' slot in the general 
#' general 'config.yml'.
#' @param inputDir String with input directory,
#' working directory by default.
#' @param outputDir String with output directory,
#' ('report' by default).
#' @param intermediateDir String with intermediate directory ('interim'
#' by default), where
#' markdown files and rds file specifying Js libraries (with \code{knit_meta}) for
#' each sub report are stored.
#' @name clinDataReview-common-args-report
#' @return No return value, used for the documentation of 
#' the clinical data reporting functions of the package.
NULL
