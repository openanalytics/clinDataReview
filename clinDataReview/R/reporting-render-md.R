#' Render one chapter of a clinical report, based on a configuration
#' file
#' @param configGeneralParams List with parameters from the general config file
#' @param ... options passed to \code{\link{renderFile}}
#' @inheritParams clinDataReview-common-args-report
#' @return No output file, the Markdown report for the chapter
#' and the \code{knit_meta} object is available in the
#' \code{intermediateDir} directory.\cr
#' If the input parameters are not correctly extracted, NULL is returned.
#' @importFrom utils hasName
#' @importFrom tools file_path_sans_ext
renderChapter <- function(
  configFile,
  configGeneralParams = getParamsFromConfig(
    configFile = "config.yml", 
    configDir = configDir,
    inputDir = inputDir
  ),
  configDir = file.path(inputDir, "config"), 
  indexPath = file.path(inputDir, "index.Rmd"), 
  inputDir = ".",
  intermediateDir = "./interim",
  logFile = NULL,
  verbose = TRUE,
  ...){
  
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
              configFile = configFile, 
              configDir = configDir,
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
            "input parameters for the report are not checked.", 
            immediate. = TRUE, call. = FALSE)
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
	if(verbose)
    	message("Run report for config file: ", sQuote(configFile), ".")
    
    resRender <- try(
      outputRmd <- renderFile(
        input = inputRmdFile, 
        output_file = basename(outputMdFile),
        output_dir = dirname(outputMdFile),
        params = params,
        run_pandoc = FALSE,
        output_options = list(keep_md = TRUE),
        logFile = logFile,
        ...
      ),
      silent = TRUE
    )
    
    # save knit_meta parameters (contain required Js lib for each report)
    interimResFile <- getInterimResFile(
      intermediateDir = intermediateDir,
      mdFile = outputMdFile
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
      
      # create params
      if(!utils::hasName(x = params, name = "reportTitle")){
        warning(paste("No title is specified for the report, you",
          "may want to specify 'reportTitle' in the config file:",
          shQuote(configFile), " (by default set to: 'Title')."))
        params$reportTitle <- "Title"
      }
      params$content <- "**This part of the report could not be created.**"
      
      # run report
      outputRmd <- renderFile(
        input = file.path(inputDir, basename(pathTemplate)), 
        output_file = basename(outputMdFile),
        output_dir = dirname(outputMdFile),
        params = params,
        run_pandoc = FALSE,
        output_options = list(keep_md = TRUE),
        logFile = logFile,
        ...
      )
      
    }
    
    knitMetaReport <- attr(outputRmd, "knit_meta", exact = TRUE)
    sessionInfoReport <- attr(outputRmd, "sessionInfo", exact = TRUE)
    interimRes <- list(knitMeta = knitMetaReport, sessionInfo = sessionInfoReport)
    saveRDS(interimRes, file = interimResFile)
    
  }else	outputRmd <- NULL
  
  return(outputRmd)
  
}

#' Render a rmarkdown file, possibly in a new R session
#' 
#' 
#' This has the possibility to save output in a log file,
#' and saving also session information.
#' 
#' Note: this function is inspired from \code{xfun::Rscript_call}
#' @param input Input file to be rendered.
#' @param encoding String with encoding, 'UTF-8' by default.
#' @param params List with input parameters for this document.\cr
#' These parameters should be accessed in the Rmd document via
#' \code{params$...}.\cr
#' These parameters will be saved to a RDS file and imported 
#' during the report rendering.
#' @param ... Any extra parameters passed to \code{\link[rmarkdown]{render}},
#' for expert use only.
#' @inheritParams clinDataReview-common-args-report
#' @return Output of the function with additional attribute: 'sessionInfo' 
#' containing the details of the session information.
#' If the report fails, an error message is returned.
#' @importFrom utils hasName
#' @importFrom rmarkdown render
#' @importFrom utils sessionInfo
#' @author Laure Cougnaud
renderFile <- function(
  input, 
  encoding = "UTF-8",
  params = NULL,
  logFile = NULL,
  ...){
  
  argsRenderExtra <- list(...)
  argsRender <- c(
    list(
      input = input, 
      encoding = encoding
    ),
    argsRenderExtra
  )
    
  # run job in a separated session with Rscript
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
	          
			})', 
    file = RFile
  )
  on.exit(unlink(RFile))
  
  # and input parameters:
  inputFile <- tempfile(fileext = '.rds')
  outputFile <- tempfile(fileext = '.rds')
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
  on.exit(unlink(inputFile))
  
  # run in separated session
  RscriptPath <- file.path(R.home('bin'), 'Rscript')
  
  if(!is.null(logFile)){
    # to avoid to overwrite logFile, store log in a temporary file
    logFileTmp <- tempfile("log")
    stdOpt <- logFileTmp
  }else{
    # print standard output/error if quiet is FALSE
    quiet <- hasName(argsRenderExtra, "quiet") && isTRUE(argsRenderExtra[["quiet"]])
    stdOpt <- ifelse(quiet, FALSE, "") # '' = to the R console
  }
  
  # run the command:
  resStatus <- system2(
    command = RscriptPath, 
    args = shQuote(c(RFile, IOFiles, "--verbose")),
    stderr = stdOpt, stdout = stdOpt,
    wait = TRUE
  )
  
  if((!is.null(resStatus) && resStatus != 0) | !file.exists(outputFile))
    stop("Creation of the report failed.")
  
  if(!is.null(logFile) && file.exists(stdOpt)){
    # append run log to logFile
    file.append(file1 = logFile, file2 = stdOpt)
    unlink(stdOpt)
  }
  
  # load output
  res <- if(file.exists(outputFile)){
    on.exit(unlink(outputFile))
    readRDS(outputFile)
  }
  
  # clean intermediary file
  resFile <- as.character(res)
  if(!is.null(res) && grepl(".knit.md", resFile) && file.exists(resFile))
    file.remove(resFile)
  
  return(res)
  
}