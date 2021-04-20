
#' Scrape html
#' 
#' Scrape html file(s), retrieve the data and saves the output.
#' 
#' @param pathHtml String pointing to the path
#' where the html is stored
#' @param pathToExportDir String for the path to save the data from the html.
#' By default the same directory where the html is stored.
#' @param fileNameExport String with name of the file to be saved.
#' By default the name is the same of the html file.
#' @return The data. The output is also saved in a directory.
#' @author Michela Pasetto
#' @importFrom tools file_path_sans_ext
#' @export 
scrapeHtml <- function(
    pathHtml,
    pathToExportDir = dirname(pathHtml),
    fileNameExport = file_path_sans_ext(basename(pathHtml))
    ) {
  
  if(missing(pathHtml)) stop("No path to html specified.")
  if(! file.exists(pathHtml)) stop(sprintf("File at %s does not exist.", pathHtml))
  
  jsonObj <- readHtml(pathHtml)
  
  parsedTable <- parseScrapeTable(jsonObj)  
  saveScrapeTable(parsedTable, pathToDir = pathToExportDir, fileName = fileNameExport)
  
  return(parsedTable)
  
}

#' Read html
#' 
#' Read the html file specified.
#' @param pathHtml String pointing to the path
#' where the html is stored
#' @import magrittr
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @importFrom jsonlite fromJSON
#' @return A JSON object
#' @author Michela Pasetto
readHtml <- function(pathHtml) {
  
  doc <- read_html(pathHtml)
  text <- doc %>% html_nodes("script[data-for]") %>% html_text
  jsonObj <- fromJSON(text)
  
  return(jsonObj)
  
}

#' Parse JSON objects
#' 
#' Parsing JSON objects from \code{jsonlite::fromJSON} function.
#' See functions \code{readHtml}.
#' @param jsonObj An object from \code{fromJSON}.
#' The object must contain
#' \itemize{
#'    \item{x}{
#'        \item{data}{The data}
#'        \item{container}{The container to extract the column names
#' to assign to the data}
#'   }
#' }
#' @return A matrix object, as a table of the data
#' @author Michela Pasetto
parseScrapeTable <- function(jsonObj) {
  
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
  
  return(table)
  
}

#' Save scraped table
#' 
#' Save a table to a path.
#' @param table A matrix object to be saved
#' @param pathToDir String with the path to directory
#' @param fileName String with name of the file to be saved
#' @return The table is saved as both txt and csv file at the
#' directory indicated with \code{pathToDir} in a folder called 
#' "scrapedTables".
#' @importFrom utils write.table
#' @author Michela Pasetto
saveScrapeTable <- function(table, pathToDir, fileName) {
   
  dirScrape <- file.path(pathToDir, "scrapedTables")
  
  if(! dir.exists(dirScrape)) dir.create(dirScrape)
  
  pathScrapeTableTxt <- file.path(dirScrape, sprintf("%s.txt", fileName))
  pathScrapeTableCsv <- file.path(dirScrape, sprintf("%s.csv", fileName))
  
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
