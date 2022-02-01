
#' Zip the clinical data report
#' 
#' Create a zip folder of clinical data reports with a redirect page.
#' The clinical data report out of the 
#' \code{\link{render_clinDataReviewReport}} is copied into a new folder.
#' A redirect html page is created to enable the user to navigate the report
#' without needing to look into the new directory.
#' 
#' @param reportDir String for the path to the directory where
#' the clinical data reports are stored
#' @param newDir String for the path where the files from
#' \code{reportDir} should be copied to.
#' @param redirectPage String with the path of the html file that redirects
#' to the "1-introduction.html" page of the report.
#' @param zipFolder String with the path to the zipped folder.
#' @return The zip folder is created in the specified location.
#' @importFrom utils zip
#' @export 
zipClinDataReview <- function(
    reportDir = "report",
    newDir = "report_dependencies",
    redirectPage = "report.html", zipFolder = "report.zip"
) {
  
  areChrs <- all(c(
          is.character(reportDir), is.character(newDir),
          is.character(redirectPage), is.character(zipFolder)
      )
  )  
  if(! areChrs) stop("Input arguments should be characters.")  
  if(! dir.exists(reportDir)) stop("Directory specified in 'reportDir' does not exist.")
  
  folderName <- basename(newDir)  
  if(! dir.exists(newDir)) dir.create(newDir)
  
  reportFiles <- list.files(reportDir, full.names = TRUE)
  if(length(reportFiles) == 0) stop("No files available in the 'reportDir'.")
  file.copy(reportFiles, newDir, recursive = TRUE)
  
  createRedirectPage(redirectPage, dir = folderName)
  
  origWd <- getwd()
  on.exit(setwd(origWd))
  
  setwd(dirname(zipFolder))
  zip(
      zipFolder,
      files = c(basename(redirectPage), folderName)
  )
  
}


#' Create a redirect page
#' 
#' Create an html page that redirects to the 
#' "1-introduction.html" page of the clinical data report available in 
#' a directory.
#' See output from \code{\link{render_clinDataReviewReport}}.
#' @param redirectPage String with the path of the html file that redirects
#' to the "1-introduction.html" page of the report.
#' @param dir String for the path where the "1-introduction.html" is stored.
#' @return The html file is created.
#' @export 
createRedirectPage <- function(
    redirectPage = "report.html",
    dir = "report_dependencies"
) {
  
  linkToPage <- file.path(dir, "1-introduction.html")
  linkToFigure <- file.path(dir, "figures/summaryTables.png")
  
  htmlPage <- c(
      sprintf(
          '
              <!DOCTYPE html>
              <html>
              
              <p id="error"></p>
              
              <script>
              
              function imageExists(url, callback) {
              var img = new Image();
              img.onload = function() { callback(true, url); };
              img.onerror = function() { callback(false, url); };
              img.src = url;
              }
              
              function loadDoc() {
              var fileURL = "%s";
              imageExists(fileURL, function(exists, url) {
              if(exists) {
              console.log("The image exists! unzipping has occurred");
              window.location.replace("%s");
              } else {
              document.getElementById("error").innerHTML = \'<p class=\"styleP\">The report could not be found. If you received this report in a ZIP format, please make sure to first unzip the folder before opening the report.</p>\';
              }
              });
              };
              
              </script>
              
              <body onLoad = \"loadDoc()\">
              
              <style>
              p.styleP {
              font-size: 40px;
              text-align: center;
              }
              </style>
              
              </body>
              </html>
              ',
          linkToFigure, linkToPage
      )
  )
  
  write(x = htmlPage, file = redirectPage)
  
}
