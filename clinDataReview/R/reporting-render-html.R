#' Convert clinical data Markdown files to HTML
#' @param mdFiles (optional) Path to the \code{Markdown} files that
#' should be converted. If specified, the specified config files 
#' in \code{configDir} are ignored.
#' @param ... Any parameters passed to \code{\link[rmarkdown]{render}},
#' for expert use only.
#' @inheritParams clinDataReview-common-args-report
#' @inherit buildBook return
#' @author Laure Cougnaud
#' @importFrom rmarkdown render
#' @family clinical data reporting
#' @importFrom tools file_ext
#' @export
postProcessReport <- function(
  inputDir = ".",
  configDir = file.path(inputDir, "config"), 
  indexPath = file.path(inputDir, "index.Rmd"),
  extraDirs = getExtraDirs(inputDir = inputDir, configDir = configDir),
  outputDir = "./report", 
  intermediateDir = "./interim",
  mdFiles = NULL,
  nCores = 1,
  logFile = NULL,
  verbose = TRUE,
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
  
  # Check if some files are missing
  filesMissing <- !file.exists(mdFiles)
  if(any(filesMissing)){
    warning(paste(
      "Markdown file(s):", toString(sQuote(basename(mdFiles[filesMissing]))),
      "are missing, these files are ignored."
    ), call. = FALSE, immediate. = TRUE)
    mdFiles <- mdFiles[!filesMissing]
    configFiles <- configFiles[!filesMissing]
  }	
  
  # extract session info
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
  sessionInfoReports <- list()
  for(file in interimResFiles){
    interimFile <- readRDS(file)
    interimSessionInfo <- interimFile$sessionInfo
    if(!is.null(interimSessionInfo))
      sessionInfoReports <- c(sessionInfoReports, list(interimSessionInfo))
  }
  
  # include session information in the report
  sessionInfoMd <- exportSessionInfoToMd(
    sessionInfos = sessionInfoReports, 
    intermediateDir = intermediateDir,
    logFile = logFile,
    ...
  )
  
  # split each chapter based on the specified: 'split_by' parameter
  configMd <- c()
  for(file in configFiles){
    mdFileSplit <- splitChapter(
      configFile = file, 
      indexPath = indexPath,
      configDir = configDir,
      intermediateDir = intermediateDir,
      outputDir = outputDir,
	  verbose = verbose
    )
    configMd[mdFileSplit] <- file
  }
  # + session info
  mdSessionInfoFileSplit <- splitChapter(
    mdFile = sessionInfoMd,
    indexPath = indexPath, 
    intermediateDir = "interim", 
    outputDir = outputDir,
	verbose = verbose
  )
  configMd[mdSessionInfoFileSplit] <- ""
  
  # copy extra directories before rendering the full report
  # (e.g. logo required to run the full report)
  extraDirs <- extraDirs[file.exists(extraDirs)]
  if (length(extraDirs) > 0){
    tmp <- file.copy(
      from = extraDirs, to = outputDir, 
      overwrite = TRUE, recursive = TRUE
    )
  }
  
  # convert Md -> html (in parallel if requested)
  isParallel <- (nCores > 1)
  if(isParallel){
    
    # create cluster
    cl <- makeCluster(nCores, outfile = "")
    on.exit(stopCluster(cl = cl))
    
    # run in parallel
    htmlFiles <- parLapply(
      cl = cl,
      X = seq_along(configMd),
      fun = function(iFile, ...){	
        
        # get input files
        configFile <- unname(configMd[iFile])
        mdFile <- names(configMd)[iFile]
        
        # run report in a separated folder, otherwise libs files can overwrite
        # files of parallel execution
        outputDirTmp <- tempfile("report")
        dir.create(outputDirTmp)
        on.exit(unlink(outputDirTmp, recursive = TRUE))
        
        mdFileTmp <- file.path(outputDirTmp, basename(mdFile))
        file.copy(from = mdFile, to = mdFileTmp)
        
        # convert Md -> html
        file <- convertMdToHtml(
          mdFile = mdFileTmp, 
          configFile = if(configFile != ""){configFile},
          setTitle = (iFile > 1),
          outputDir = outputDirTmp,
		  verbose = verbose,
          ...
        )
        
        # copy files from temporary directory to output directory
        resFilesTmp <- list.files(outputDirTmp, full.names = TRUE)
        tmp <- sapply(resFilesTmp, function(pathTmp){
          isDir <- (tools::file_ext(pathTmp) == "")
          if(isDir){
            pathNew <- file.path(outputDir, basename(pathTmp))
            if(!dir.exists(pathNew)){dir.create(pathNew)}
          }
          file.copy(from = pathTmp, to = outputDir, 
            recursive = isDir, overwrite = TRUE)
        })
        
        file.path(outputDir, basename(file))
        
      },
      indexPath = indexPath, 
      intermediateDir = intermediateDir, 
      ...
    )
    htmlFiles <- unlist(htmlFiles)
  }else{
    htmlFiles <- c()
    for(iFile in seq_along(configMd)){
      mdFile <- names(configMd)[iFile]
      configFile <- unname(configMd[iFile])
      htmlFileChapter <- convertMdToHtml(
        mdFile = mdFile,
        configFile = if(configFile != ""){configFile}, 
        indexPath = indexPath, 
        intermediateDir = intermediateDir, 
        outputDir = outputDir,
        setTitle = (iFile > 1),
		verbose = verbose,
        ...
      )
      htmlFiles <- c(htmlFiles, htmlFileChapter)
    }
  }
  
  # resolve TOC
  indexHTMLFile <- buildBook(htmlFiles = htmlFiles, verbose = verbose)
  
  # delete split md files
  tmp <- file.remove(names(configMd))
  
  # delete original html files
  tmp <- file.remove(htmlFiles)
  
  indexHTMLFile <- file.path(outputDir, basename(indexHTMLFile))
  return(indexHTMLFile)
  
}

#' Split a chapter based on the 'split_by' parameter.
#' 
#' @section Extension to chapter-specific split:
#' The bookdown 'split_by' parameter is extended, to support:
#' \itemize{
#' \item{chapter-specific split, specified in the configuration file
#' of the specific chapter, via the \code{split_by} parameter}
#' \item{specification as a number (if specified within a config file), 
#' e.g. '0' for no split, 1' for chapter, '2' for
#' section, '3' for subsection, ...}
#' \item{split at section level higher than 2 (until 7)  
#' (if specified within a config file)}
#' }
#' @param mdFile (optional) Path to the Markdown file containing the chapter.
#' If not specified, the Markdown file corresponding to the specified
#' \code{configFile} parameter is used.
#' @inheritParams clinDataReview-common-args-report
#' @return No return value, the Markdown files are split as specified.
#' @importFrom utils tail
#' @importFrom rmarkdown yaml_front_matter
#' @importFrom xfun read_utf8 write_utf8
#' @importFrom tools file_path_sans_ext file_ext
#' @author Laure Cougnaud
splitChapter <- function(
  configFile = NULL, configDir = "./config", 
  mdFile = NULL,
  indexPath = "index.Rmd", 
  intermediateDir = "./interim", 
  outputDir = "./report",
  verbose = TRUE){
  
  if(is.null(configFile) & is.null(mdFile)){
    stop("The configuration or the Markdown file should be specified",
      "in order that the chapter be split.")
  }
  
  # 'split_by' parameter: set at the report level ...
  split_by <- NULL
  outputFormat <- rmarkdown::yaml_front_matter(input = indexPath)$output[[1]]
  if(hasName(outputFormat, "split_by"))
    split_by <- outputFormat[["split_by"]]
  # ... or at the chapter level
  if(!is.null(configFile)){	
    configPath <- file.path(configDir, configFile)
    params <- getParamsFromConfig(
      configDir = configDir, 
      configFile = configFile
    )
    if(!is.null(params[["split_by"]]))
      split_by <- params[["split_by"]]
  }
  
  # set/check default
  if(is.null(split_by))	split_by <- "chapter"
  if(is.integer(split_by))  split_by <- as.character(split_by)
  split_by <- match.arg(split_by, 
    choices = c("none", "rmd", 
      outer(c("chapter", "section", 0:7), c("", "+number"), paste0)
    )
  )
  # extract split level
  split_level <- sub("[+]number$", "", split_by)
  split_level <- switch(split_level,
    none = 0, 
    chapter = 1, 
    section = 2, 
    rmd = 1,
    if (!(split_level %in% as.character(0:7))){
      stop("split_level must be: 'none', 'chapter', 'section', 'rmd' or among 0:8")
    }else	as.numeric(split_level)
  )
  
  if(split_level == 0)
    return()
  
  if(is.null(mdFile)){
    mdFile <- getMdFromConfig(
      configFiles = configFile, 
      indexPath = indexPath, 
      intermediateDir = intermediateDir
    )
    if(verbose)	message("Split chapter: ", sQuote(configFile), ".")
  }else{
	if(verbose)	message("Split chapter: ", sQuote(basename(mdFile)), ".")
  }
  
  # import Markdown content
  x <- xfun::read_utf8(mdFile)
  
  # extract lines with Markdown headers
  headMd <- sapply(seq_len(split_level), function(n){
    paste(rep("#", n), collapse = "")
  })
  idxHead <- grep(paste0("^(", paste(headMd, collapse = "|"),") .+"), x)
  idxHead <- utils::tail(idxHead, -1) # first section is included in the first page

  # split the chapter
  if(length(idxHead) > 0){
    idx <- unique(c(1, idxHead))
    nFiles <- length(idx)
    mdFiles <- file.path(outputDir, 
      paste0(
         tools::file_path_sans_ext(basename(mdFile)), "-", 
         seq_len(nFiles), ".", 
         tools::file_ext(mdFile)
      )
    )
    for(i in seq_len(nFiles)){
      start <- idx[i]
      if(i == nFiles){
        end <- length(x)
      }else{
        end <- idx[i+1]-1
      }
      if(end < start)	end <- start # in case the section is empty
      xChapter <- x[seq(from = start, to = end)]
      xfun::write_utf8(text = xChapter, con = mdFiles[i])
    }
  }else{
    mdFiles <- file.path(outputDir, basename(mdFile))
    file.copy(from = mdFile, to = mdFiles)
  }
  
  return(mdFiles)
  
}

#' Convert the Md file for a specific chapter to html
#' @param setTitle Logical (TRUE by default), should the title be set 
#' to the document? If so, the pandoc metadata option: 'pagetitle' is set to:
#' base file name of \code{mdFile}.
#' @param ... Arguments passed to \code{\link{renderFile}}
#' @inheritParams clinDataReview-common-args-report
#' @inheritParams splitChapter
#' @return No returned value, the files in the \code{intermediateDir}
#' are converted to HTML
#' @importFrom tools file_path_sans_ext
#' @importFrom rmarkdown pandoc_metadata_arg resolve_output_format
#' @author Laure Cougnaud
convertMdToHtml <- function(
  mdFile, 
  configFile = NULL, 
  indexPath = "index.Rmd", 
  intermediateDir = "./interim", 
  outputDir = "./report",
  setTitle = TRUE,
  verbose = TRUE,
  ...){
  
	if(verbose)
		message("Convert the Markdown file(s) to html for the chapter: ",
			sQuote(basename(mdFile)), ".")
  
  # interim file with e.g. library dependencies
  interimRes <- if(!is.null(configFile)){
    mdFileConfig <- getMdFromConfig(
      configFiles = configFile, 
      indexPath = indexPath, 
      intermediateDir = intermediateDir
    )
    interimResFile <- getInterimResFile(
      intermediateDir = intermediateDir,
      mdFile = mdFileConfig
    )
    if(file.exists(interimResFile))
      readRDS(interimResFile)	
  }
  
  # convert Markdown -> html
  htmlFiles <- c()
  
  # create YAML file for output format
  outputYAMLPath <- createOutputYaml(
    indexPath = indexPath,
    outputDir = outputDir
  )
  # delete output YAML file
  on.exit(unlink(outputYAMLPath))
    
  outputOpts <- list(split_by = "rmd")
  
  # for each chapter, define a title to avoid pandoc:
  # [WARNING] This document format requires a nonempty <title> element.
  # excepted for the front page of the book
  if(setTitle){
    title <- tools::file_path_sans_ext(basename(mdFile))
    outputOpts$pandoc_args <- rmarkdown::pandoc_metadata_arg(
      name = "pagetitle", 
      value = title
    )
  }
  
  # fix filename for the output file
  outputFile <- tools::file_path_sans_ext(basename(mdFile))
  
  # set fixed filename for the output file
  htmlFile <- rmarkdown::render(
    input = mdFile, 
    output_dir = outputDir,
    knit_meta = interimRes$knitMeta,
    output_file = outputFile,
    output_options = outputOpts,
    output_yaml = outputYAMLPath,
    encoding = "UTF-8",
    ...
  )
  
  return(htmlFile)
  
}

#' Create a output YAML file
#' 
#' This file containing the contents of the
#' \code{output} field of the YAML header of a Markdown file.
#' It can be passed to the \code{output_yaml} parameter
#' of the \code{\link[rmarkdown]{render}} function.
#' @inheritParams clinDataReview-common-args-report 
#' @return String with file to the \code{_output.yml} file
#' in a temporary folder.
#' @importFrom yaml yaml.load write_yaml
#' @importFrom xfun read_utf8
createOutputYaml <- function(indexPath, outputDir){
  
  indexCnt <- xfun::read_utf8(indexPath)
  idxYAMLHeader <- which(trimws(indexCnt) == "---")
  if(length(idxYAMLHeader) < 2)
    stop(paste("The file:", shQuote(indexPath), "doesn't contain",
      "a proper YAML header."))
  yamlHeader <- indexCnt[seq(from = idxYAMLHeader[1]+1, to = idxYAMLHeader[2]-1)]
  yamlOutput <- yaml::yaml.load(string = yamlHeader)$output
  outputYAMLPath <- tempfile("_output", fileext = ".yml")
  yaml::write_yaml(x = yamlOutput, file = outputYAMLPath)
  
  return(outputYAMLPath)
  
}

#' Build the book
#' @param htmlFiles character vector with path to HTML files
#' @inheritParams clinDataReview-common-args-report
#' @return String with path to the front page of the 
#' report.
#' @importFrom xml2 xml_text read_xml
#' @importFrom xfun read_utf8
#' @author Laure Cougnaud
buildBook <- function(htmlFiles, verbose = TRUE){
  
	if(verbose)
		message("Build the book.")
  
  # import the html files
  htmlContentList <- sapply(htmlFiles, xfun::read_utf8, simplify = FALSE)
  
  # get toc
  regexHeader <- "^<h(\\d{1,})>(.+)</h\\d{1,}>$"
  tocList <- lapply(htmlFiles, function(file){
    cnt <- htmlContentList[[file]]
    idx <- grep(pattern = regexHeader, x = cnt)
    if(length(idx) > 0){
      levels <- sub(regexHeader, "\\1", cnt[idx])
      titles <- unname(sapply(cnt[idx], function(x) xml2::xml_text(xml2::read_xml(x))))
      ids <- sub('<div id="(.+)" class.+', "\\1", cnt[idx-1])
      data.frame(
        file = basename(file), 
        idx = idx, 
        level = levels, 
        title = titles, 
        id = ids
      )
    }
  })
  toc <- do.call(rbind, tocList)
  
  # get section numbers
  toc$number <- getTocNumbering(levels = toc$level)
  
  # get new file name
  toc$fileNew <- paste(toc$number, toc$id, sep = "-") 
  toc$fileNew <- paste0(toc$fileNew, ".html")
  # only consider the new file name when it is not split (first header)
  tocFiles <- toc[!duplicated(toc$file), c("file", "fileNew")]
  toc$fileNew <- tocFiles[match(toc$file, tocFiles$file), "fileNew"]
  
  # get new title
  toc$titleNew <- trimws(sub("^(\\d{1,}|\\.)*", "", toc$title))
  
  # get html toc
  tocText <- getHTMLToc(toc = toc)
  
  # add TOC & update title in each chapter
  idxTitleBook <- getIndexHTMLTitle(x = htmlContentList[[1]])
  titleBook <- if(length(idxTitleBook) == 1){
    sub("^<title>(.+)</title>$", "\\1", trimws(htmlContentList[[1]][idxTitleBook]))
  }
  for(path in names(htmlContentList)){
    
    cnt <- htmlContentList[[path]]
    
    tocFile <- toc[which(toc$file == basename(path)), ]
    
    # update title of the file
    idxTitle <- getIndexHTMLTitle(x = cnt)
    if(length(idxTitle) > 0){
      tocFileFirst <- tocFile[1, ]
      cnt[idxTitle] <- paste0(
        "<title>", tocFileFirst[, "number"], " ",  
        tocFileFirst[1, "titleNew"], 
        " | ", titleBook, "</title>"
      )
    }
    
    # update section number in TOC title
    for(i in seq_len(nrow(tocFile))){
      idx <- tocFile[i, "idx"]
      cnt[idx] <- sub(
        "(.+<span.*>)(.+)(</span>.+)", 
        paste0("\\1", tocFile[i, "number"], "\\3"), 
        cnt[idx]
      )
    }
    # insert TOC
    start <- grep('<nav role="navigation">', cnt)
    idxUL <- grep('</nav', cnt)
    idxUL <- idxUL[which(idxUL-start > 0)]
    end <- idxUL[which.min(idxUL-start)]
    cnt <- c(
      cnt[seq_len(start)], tocText,
      cnt[seq(from = end, to = length(cnt))]
    )
    
    # export to the file
    fileNew <- tocFile[1, "fileNew"]
    xfun::write_utf8(text = cnt, con = file.path(dirname(path), fileNew))
    
  }
  
  indexHTMLFile <- file.path(dirname(htmlFiles)[1], toc[1, "fileNew"])
  return(indexHTMLFile)
  
}

#' Get TOC numbering
#' @param levels vector with levels of the section,
#' in the order as available in the book.
#' @return Character vector with section numbers
#' @author Laure Cougnaud
getTocNumbering <- function(levels){
  
  numbers <- list()
  for(level in as.integer(sort(unique(levels)))){
    if(level == 1){
      secNum <- cumsum(levels == level)
    }else{
      # vector with groups of headers in the same header previous level (X-1)
      secGroups <- do.call(paste, c(numbers[seq_len(level-1)], list(sep = ".")))
      rleGroups <- rle(secGroups)
      groups <- rep(x = seq_along(rleGroups[["values"]]), times = rleGroups[["lengths"]])
      # section number for header of level (X)
      secNum <- unlist(tapply(levels == level, groups, cumsum))			
    }
    numbers[[level]] <- secNum
  }
  numbers <- do.call(paste, c(numbers, list(sep = ".")))
  
  # remove trailing zeros
  numbers <- gsub("\\.0", "", numbers)
  
  return(numbers)
  
}

#' Get HTML toc
#' @param toc data.frame with TOC info
#' @return Character vector with HTML toc
getHTMLToc <- function(toc){
  
  # create HTML toc format
  tocText <- paste0(
    '<li class="chapter', 
    ifelse(toc$file == toc$file[1], " active", ""), 
    '" ',
    'data-level="', toc$number, '" ',
    'data-path="', toc$fileNew, '"',
    # href should contain [file]#[id]
    '><a href="', toc$fileNew, "#", toc$id, '"><i class="fa fa-check"></i><b>',
    toc$number, '</b> ', toc$titleNew, '</a></li>'
  )
  
  # add sublist
  levelDiff <- diff(as.integer(toc$level))
  
  getUlEl <- function(x, times){
    x <- rep(x, times = times)
    return(paste(x, collapse = ""))
  }
  
  # start a ul when a new subsection is created
  ulStart <- which(levelDiff > 0) + 1
  nStart <- levelDiff[ulStart-1]
  ulStartTxt <- sapply(nStart, getUlEl, x = "<ul>")
  tocText[ulStart] <- paste0(ulStartTxt, tocText[ulStart])
  tocText[ulStart-1] <- gsub("</li>$", "", tocText[ulStart-1])
  # end a ul when a higher level subsection is created
  ulEnd <- which(levelDiff < 0)
  last <- nrow(toc)
  nEnd <- abs(levelDiff[ulEnd]) # number of ul ends to be included
  # end ul at the end
  if(toc$level[last] > 1){
    ulEnd <- c(ulEnd, last)
    nEnd <- c(nEnd, length(ulStart)-length(nEnd))
  }
  ulEndTxt <- sapply(nEnd, getUlEl, x = "</ul></li>")
  tocText[ulEnd] <- paste0(tocText[ulEnd], ulEndTxt)
  
  # add summary class
  tocText <- c(
    '<ul class = "summary">',
    tocText,
    "</ul>"
  )
  
  return(tocText)
  
}

#' Get index of the line containing the HTML title in a vector of HTML strings
#' @param x Character vector with HTML
#' @return Integer vector with index of the title in the vector \code{x}
getIndexHTMLTitle <- function(x){
  
  idxElStart <- which(x == "<head>")
  idxElEnd <- which(x == "</head>")
  idxTitle <- if(length(idxElStart) == 1 && length(idxElEnd) == 1){
    idxElTitle <- grep("<title>", x[seq(from = idxElStart, to = idxElEnd)])
    idxTitle <- idxElStart+idxElTitle-1
  }else integer()
  
  return(idxTitle)
  
}

