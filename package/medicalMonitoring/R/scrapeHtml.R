library(rvest)
library(jsonlite)

#' @importFrom rvest read_html html_nodes html_text
#' @import magrittr
#' @importFrom jsonlite fromJSON
scrapeHtml <- function(htmlPath) {
  
  doc <- read_html(htmlPath)
  
  text <- doc %>% html_nodes("script[data-for]") %>% html_text
  jsonObj <- fromJSON(text)
  # Extract data
  dataText <- jsonObj$x$data
  table <- t(matrix(dataText, ncol = ncol(dataText)))
  # Extract colnames
  namesText <- jsonObj$x$container
  namesText <- gsub(
      "<table class=\"display\">\n  <thead>\n    <tr>\n      <th> </th>\n      ",
      "",
      namesText)
  
  return(table)
  
}

