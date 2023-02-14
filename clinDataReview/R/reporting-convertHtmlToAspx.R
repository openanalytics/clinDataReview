#' Convert report format from .html to .aspx
#' 
#' Report files generated as output of \code{\link{render_clinDataReviewReport}}  are converted from .html to 
#' to .aspx format by changing extensions and cross-links of all files within the directory \code{reportDir}.
#' This allows for deployment on SharePoint. 
#' 
#' 
#' @param reportDir String for the path to the directory where
#' the clinical data reports are stored, defaults to current directory
#' @importFrom xfun read_utf8 write_utf8
#' @return no return value, files in directory are modified
#' @export 
convertReportToAspx <- function( reportDir = '.' ){
  
  # check inputs
  if(!dir.exists(reportDir)){
    stop("'reportDir' does not exist")
  }
  
  htmlSubFiles <- list.files(path=reportDir, recursive=TRUE, pattern = "*html$")
  
  if( ! length(htmlSubFiles) > 0 ){
    warning('No html files present in directory')
  }
  
  htmlSubFiles <- file.path(reportDir, htmlSubFiles)
  noOut <- lapply(htmlSubFiles, changeExtension)
  aspxSubFiles <- gsub('.html','.aspx', htmlSubFiles)
  no_out  <- lapply(aspxSubFiles, updateLinks)
}

# Helper functions

changeExtension <- function(file, from='.html', to = '.aspx'){
  file.rename(from=file,to=sub(pattern = from,replacement = to ,file))
}

updateLinks <- function(path, from='.html', to = '.aspx'){
  text <-  read_utf8(path)
  text <- as.character(lapply(X= text,FUN= function(line){gsub(from, to , line, fixed=TRUE)}  ))
  write_utf8(text = text, con = path)
}
