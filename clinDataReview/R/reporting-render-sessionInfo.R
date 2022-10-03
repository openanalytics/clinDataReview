#' Combine all session informations across all clinical data reports
#' and export them into a dedicated Markdown document
#' @param sessionInfos List with \code{\link{sessionInfo}} objects
#' @param ... Any parameters passed to \code{\link{renderFile}},
#' for expert use only.
#' @inheritParams clinDataReview-common-args-report
#' @return String with path to Markdown file containing the session information,
#' NULL if no session information(s) are provided.
#' @author Laure Cougnaud
exportSessionInfoToMd <- function(
  sessionInfos, 
  intermediateDir = "interim", 
  logFile = NULL, ...){
  
  if(length(sessionInfos) > 0){
    
    # combine session informations
    sessionInfoAll <- do.call(merge, sessionInfos)
    sessionInfoFile <- tempfile("sessionInfo", fileext = ".Rmd")
    cat(
      '# Appendix  \n\n## Session information  \n\n',
      '```{r, echo = FALSE, results = "asis"}\nparams$sessionInfoAll\n```', 
      file = sessionInfoFile, sep = ""
    )
    on.exit(unlink(sessionInfoFile))
    
    sessionInfoMd <- "sessionInfo.md"
    outputRmd <- renderFile(
      input = sessionInfoFile, 
      output_file = sessionInfoMd, 
      output_dir = intermediateDir, 
      params = list(sessionInfoAll = sessionInfoAll),
      run_pandoc = FALSE,
      output_options = list(keep_md = TRUE),
      logFile = logFile,
      ...
    )
    mdFile <- file.path(intermediateDir, sessionInfoMd)
    
  }else	mdFile <- NULL
  
  return(mdFile)
  
}

#' Merge multiple session information
#' @param ... objects of type \code{\link{sessionInfo}}
#' @return \code{\link{sessionInfo}} with combined information
#' @author Laure Cougnaud
merge.sessionInfo <- function(...){
  
  sessionInfos <- list(...)
  sessionInfoNames <- unique(unlist(lapply(sessionInfos, names)))
  
  sessionInfos <- lapply(sessionInfos, "[", sessionInfoNames)
  
  sessionInfoAll <- do.call(mapply, c(
    list(FUN = function(...) unique(c(...)), SIMPLIFY = FALSE), 
    sessionInfos
  ))
  class(sessionInfoAll) <- c("sessionInfo", class(sessionInfoAll))
  
  return(sessionInfoAll)
  
}