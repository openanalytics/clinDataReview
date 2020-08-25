
#' Scrape html
#' 
#' Scrape html file(s) and retrieve the data
#' @param pathHtml String pointing to the path
#' where the html is stored
#' @return The data
#' @example inst/examples/scrapeHtml-example.R
#' @import magrittr
#' @importFrom rvest read_html html_nodes html_text
#' @importFrom jsonlite fromJSON
#' @author Michela Pasetto
#' @export 
scrapeHtml <- function(pathHtml) {
  
  doc <- read_html(pathHtml)
  text <- doc %>% html_nodes("script[data-for]") %>% html_text
  jsonObj <- fromJSON(text)
  
  # Extract data
  dataText <- jsonObj$x$data
  table <- t(matrix(dataText, ncol = ncol(dataText)))
  
  # Check for column with + symbol
  #### Is there a better way?
  symbolExist <- sapply(1 : ncol(table), function(x) any(grepl("&oplus", table[, x])))
  if(any(symbolExist)) {
    
    colIdx <- which(symbolExist)
    table <- table[, -colIdx]
    
  }
  
  # Extract colnames
  namesText <- jsonObj$x$container
  namesText <- gsub("<.*?>", "", namesText)
  # unlist(strsplit(namesText, split = "<.*?>|\n|\\s+"))
  namesText <- unlist(strsplit(namesText, split = "\n"))
  namesText <- gsub("^\\s+", "", namesText)
  
  colnamesTable <- namesText[namesText != ""]
  
  colnames(table) <- colnamesTable
  
  saveScrapeTable(table, pathHtml)
  
  return(table)
  
}

#' @importFrom utils write.table
saveScrapeTable <- function(table, pathHtml) {
  
  fileName <- gsub(
      ".+[/]MOMP[/](.+)[.]html",
      "\\1",
      pathHtml
  )
  
  dirScrape <- gsub(
      "(.+)[/]MOMP[/](.+)[.]html",
      "\\1/MOMP/scrapedTables",
      pathHtml
  )
  if(! dir.exists(dirScrape)) dir.create(dirScrape)
  
  pathScrapeTableTxt <- sprintf("%s/%s.txt", dirScrape, fileName)
  pathScrapeTableCsv <- sprintf("%s/%s.csv", dirScrape, fileName)
  
  write.table(
      x = table,
      file = pathScrapeTableTxt,
      quote = FALSE,
      sep = '\t',
      row.names = FALSE
  )
  write.table(
      x = table,
      file = pathScrapeTableCsv,
      quote = FALSE,
      sep = '\t',
      row.names = FALSE
  )
  
}
