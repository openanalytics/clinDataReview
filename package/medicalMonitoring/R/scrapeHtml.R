
#' Scrape html
#' 
#' Scrape html file(s) and retrieve the data
#' @param htmlPath String pointing to the path
#' where the html is stored
#' @return The data
#' @example inst/examples/scrapeHtml-example.R
#' @import magrittr
#' @importFrom rvest read_html html_nodes html_text
#' @importFrom jsonlite fromJSON
#' @author Michela Pasetto
#' @export 
scrapeHtml <- function(htmlPath) {
  
  doc <- read_html(htmlPath)
  text <- doc %>% html_nodes("script[data-for]") %>% html_text
  jsonObj <- fromJSON(text)
  
  # Extract data
  dataText <- jsonObj$x$data
  table <- t(matrix(dataText, ncol = ncol(dataText)))
  
  # Extract colnames
  namesText <- jsonObj$x$container
  namesText <- gsub("<.*?>", "", namesText)
  # unlist(strsplit(namesText, split = "<.*?>|\n|\\s+"))
  namesText <- unlist(strsplit(namesText, split = "\n"))
  namesText <- gsub("^\\s+", "", namesText)
  
  colnamesTable <- namesText[namesText != ""]
  
  colnames(table) <- colnamesTable
  
  return(table)
  
}

