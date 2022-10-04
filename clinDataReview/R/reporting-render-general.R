#' Render a clinical data review report.
#' 
#' @section Process:
#' This function is based on the \link[bookdown]{render_book}
#' function, with the extra functionalities:
#' \itemize{
#' \item{specification of chapter-specific input parameters, 
#' specified in YAML configuration files}
#' \item{(optional) creation of each chapter in parallel if \code{nCores} 
#' > 1. In that case, all chapters are run in parallel, excepted the 
#' chapter(s) run internally in parallel (config file with \code{parallel} set 
#' to 'TRUE').}
#' \item{(optional) split of each chapter into html file specific 
#' for each chapter, by specifying the \code{split_by} parameter in the 
#' chapter-specific config file}
#' }
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
#' \item{running the report ('template' tag) with the associated
#' parameters in a \strong{new R session for reproducibility}, 
#' to obtain the associated Markdown file.\cr
#' This step is parallelized across the different config files, if the 
#' \code{nCores} parameter is specified.}
#' }}
#' \item{checking if the associated \code{Markdown} and \code{rds} file
#' (list of Js dependencies) are available in \code{intermediateDir}}
#' \item{split each chapter into separated Markdown documents, 
#' based on the \code{split_by} parameter 
#' (specified at the report or config level)}
#' \item{conversion of each Markdown document to an HTML document. \cr
#' This step is parallelized across the different Markdown documents, if the 
#' \code{nCores} parameter is specified.}
#' \item{build the book: }{
#' \enumerate{
#' \item{creation of a common TOC for the book}
#' \item{inclusion of the TOC in each Markdown file}
#' \item{update of the section number in each chapter}
#' \item{inclusion of the section number in each HTML file name}
#' }}
#' }
#' If the execution of a specific report fails with error, 
#' a warning message is triggered. A report containing
#' only the specified title is created, to ensure
#' output consistency (especially html file numbering)
#' in case the report succeeds.
#' @section Available template report:
#' see \strong{\code{? `clinDataReview-templates`}} for a list of 
#' clinical data template report available in the package.
#' @inheritSection splitChapter Extension to chapter-specific split
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
#' @inheritParams clinDataReview-common-args-report
#' @inherit postProcessReport return
#' @author Laure Cougnaud
#' @importFrom parallel parLapply stopCluster makeCluster
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
	extraDirs = getExtraDirs(inputDir = inputDir, configDir = configDir),
	quiet = FALSE, verbose = TRUE,
	nCores = 1){
  
  # log output
  if(!is.null(logFile)){
    logFileCon <- file(logFile, "w")
    sink(file = logFileCon, split = FALSE, type = "message", append = TRUE)
    sink(file = logFileCon, split = FALSE, type = "output", append = TRUE)
    on.exit({sink(type = "output");sink(type="message")})
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
  
  ## run index file + each chapter
  
  # consider all config files
  if(is.null(configFiles))
    configFiles <- c("config.yml", configGeneralParams[["config"]])
  
  configFiles <- checkTemplatesName(configFiles = configFiles, configDir = configDir)
  
  # check uniqueness of report titles
  reportTitles <- checkReportTitles(configFiles, configDir = configDir)
  
  # identify chapters that should be run in parallel
  isParallel <- (nCores > 1)
  if(isParallel){
    # chapters which are run internally in parallel
    isParallelChapter <- sapply(configFiles, 
      checkChapterParallel,
      configDir = configDir, inputDir = inputDir
    )
    # chapters which are run internally in parallel should run sequentially
    configFilesRunNotParallel <- names(which(isParallelChapter))
    configFilesRunParallel <- setdiff(configFiles, configFilesRunNotParallel)
  }else{
    configFilesRunNotParallel <- configFiles
    configFilesRunParallel <- character()
  }
  
  ## render each chapter (Rmd -> Md)
  
  # chapter(s) run in parallel
  if(length(configFilesRunParallel) > 0){
    
    cl <- makeCluster(nCores, outfile = "")
	
    mdFiles <- parLapply(
      cl = cl,
      X = configFilesRunParallel,
      fun = function(configFile, ...)
        renderChapter(
          configFile = configFile,
          # create knit.md in a temporary folder to avoid conflicts
          # between parallel sessions
          intermediates_dir = tempfile("interim"),
          ...
        ),
      configGeneralParams = configGeneralParams,
      configDir = configDir, 
      indexPath = indexPath, 
      inputDir = inputDir,
      intermediateDir = intermediateDir,
      quiet = quiet, verbose = verbose,
	  logFile = logFile
    )
    
    stopCluster(cl = cl)
    
  }

  # chapter(s) run in sequential
  if(length(configFilesRunNotParallel) > 0){
    for(configFile in configFilesRunNotParallel){
      mdFileChapter <- renderChapter(
        configFile = configFile,
        configGeneralParams = configGeneralParams,
        configDir = configDir,
        indexPath = indexPath,
        inputDir = inputDir,
        intermediateDir = intermediateDir,
        quiet = quiet, verbose = verbose,
		logFile = logFile
      )
    }
  }
  
  # post-process the report
  outputFile <- postProcessReport(
    configDir = configDir, 
    indexPath = indexPath,
    intermediateDir = intermediateDir,
    outputDir = outputDir,
    quiet = quiet,
    nCores = nCores,
    extraDirs = extraDirs,
    logFile = logFile,
	verbose = verbose
  )
  
  return(outputFile)
  
}

#' Check if a chapter is run internally in parallel or not.
#' 
#' This is identified via the 'parallel' parameter from the config file.
#' If this parameter is not available in the config file 
#' (or the parameters are imported with an error), the chapter is 
#' considered to not be run in parallel.
#' @inheritParams clinDataReview-common-args-report
#' @importFrom utils hasName
#' @return Logical, if TRUE, the chapter is run in parallel
#' (FALSE if not available).
checkChapterParallel <- function(configFile, 
  configDir = file.path(inputDir, "config"), 
  inputDir = "."){
  
  params <- try(
    getParamsFromConfig(
      configFile = configFile,
      configDir = configDir,
      inputDir = inputDir
    ), 
    silent = TRUE
  )
  
  isChapterParallel <- 
    !inherits(params, "try-error") && 
    utils::hasName(params, "parallel") && 
    isTRUE(params[["parallel"]])
  
  return(isChapterParallel)
  
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

#' Common parameters for the clinical data reporting function
#' @param indexPath String with path to the index file,
#' by default 'index.Rmd' in \code{inputDir}.
#' @param configDir String with directory with config files,
#' by default a 'config' folder in \code{inputDir}.\cr
#' It should contain a general 'config.yml' file and dedicated
#' 'config-[X].yml' for each chapter.
#' The order of each chapter is specified in the 'config' slot in the general 
#' general 'config.yml'.
#' @param configFile String with filename of the config
#' file of interest in YAML format.
#' @param inputDir String with input directory,
#' working directory by default.
#' @param outputDir String with output directory,
#' ('report' by default).
#' @param intermediateDir String with intermediate directory ('interim'
#' by default), where
#' markdown files and rds file specifying Js libraries (with \code{knit_meta}) for
#' each sub report are stored.
#' @param extraDirs Character vector with extra directories required by
#' the report, directory with external images.
#' By default, the directories: 'figures', 'tables' and mentioned in the 
#' 'patientProfilePath' parameter of the
#' general config file are included.
#' All these folders should be available in \code{inputDir}.
#' @param mdFile String with path of the Markdown file
#' @param logFile (optional) String with path to a log file,
#' where output (also error/messages/warnings) should be stored.
#' If specified, the entire output is re-directed to this file.
#' @param nCores Integer containing the number of cores used to render the report
#' (1 by default). If more than 1, two steps of the report creation are 
#' run in parallel across chapters: 
#' \itemize{
#' \item{the rendering of the Rmarkdown file to Markdown}
#' \item{the conversion from Markdown to HTML}
#' }
#' @param verbose Logical, if TRUE (FALSE by default) progress messages are printed
#' during the report execution.
#' @name clinDataReview-common-args-report
#' @return No return value, used for the documentation of 
#' the clinical data reporting functions of the package.
NULL
