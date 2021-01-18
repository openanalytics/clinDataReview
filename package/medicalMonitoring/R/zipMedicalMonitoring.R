
#' Zip the medical monitoring report
#' 
#' Create a zip folder of medical monitoring reports with a redirect page.
#' The medical monitoring report out of the 
#' \code{\link{render_medicalMonitoringReport}} is copied into a new folder.
#' A redirect html page is created to enable the user to navigate the report
#' without needing to look into the new directory.
#' 
#' @param reportDir String for the path to the directory where
#' the medical monitoring reports are stored
#' @param newDir String for the path where the files from
#' \code{reportDir} should be copied to.
#' @param redirectPage String with the path of the html file that redirects
#' to the "1-introduction.html" page of the report.
#' @param zipFolder String with the path to the zipped folder.
#' @return The zip folder is created in the specified location.
#' @importFrom utils zip
#' @export 
zipMedicalMonitoring <- function(
    reportDir = "./report",
    newDir = "./report_dependencies",
    redirectPage = "report.html", zipFolder = "report.zip"
) {
  
  if(! all(sapply(match.call()[-1], function(x) is.character(eval(x))))) stop("Input arguments should be characters.")  
  if(! dir.exists(reportDir)) stop("Directory specified in 'reportDir' does not exist.")
  
  folderName <- basename(newDir)  
  if(! dir.exists(newDir)) dir.create(newDir)
  
  reportFiles <- list.files(reportDir, full.names = TRUE)
  if(length(reportFiles) == 0) stop("No files available in the 'reportDir'.")
  file.copy(reportFiles, newDir, recursive = TRUE)
  
  createRedirectPage(redirectPage, dir = folderName)
  
  origWd <- getwd()
  setwd(dirname(zipFolder))
  zip(
      zipFolder,
      files = c(basename(redirectPage), folderName)
  )
  on.exit(setwd(origWd))
  
}


#' Create a redirect page
#' 
#' Create an html page that redirects to the 
#' "1-introduction.html" page of the medical monitoring report available in 
#' a directory.
#' See output from \code{\link{render_medicalMonitoringReport}}.
#' @param redirectPage String with the path of the html file that redirects
#' to the "1-introduction.html" page of the report.
#' @param dir String for the path where the "1-introduction.html" is stored.
#' @return The html file is created.
createRedirectPage <- function(
    redirectPage = "report.html",
    dir = "./report_dependencies"
) {
  
  linkToPage <- file.path(dir, "1-introduction.html")
  
#  htmlPage <- c(
#      sprintf(
#          '<!DOCTYPE html>
#              <html lang="" xml:lang="">
#              <head>
#              <meta http-equiv="refresh" content="0; URL=\'%s\'" />
#              </head>
#              
#			  <body>
#              </body>
#              </html>',
#          linkToPage
#      )
#  )
  htmlPage <- c('
          <!DOCTYPE html>
          <html>
          
          <script>
          function loadDoc() {
          
          var xhttp = new XMLHttpRequest();
          xhttp.onreadystatechange = function() {
          if (this.readyState == 4 && this.status == 200) {
          window.location.replace("file:///home/mpasetto/Desktop/report/medMon/1-introduction.html")
          }
          };
          
          xhttp.open("HEAD", "file:///home/mpasetto/Desktop/report/medMon/1-introduction.html", true);
          xhttp.send();
          }
          </script>
          
          <body onLoad = \"loadDoc()\">
          
          \'The report could not be found. Please be sure to unzip the folder 
            and open again the report page from the unzipped directory.\'
          
          </body>
          </html>
          '
  )
  write(x = htmlPage, file = redirectPage)
  
}




'
    <!DOCTYPE html>
    <html>
    
    <script>
    function loadDoc() {
    
    var xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
    window.location.replace("file:///home/mpasetto/Desktop/report/medMon/1-introduction.html")
    }
    };
    
    xhttp.open("HEAD", "1-introduction.html", true);
    xhttp.send();
    }
    </script>
    
    <body onLoad = \"loadDoc()\">
    
    \'Ciao\'
    
    </body>
    </html>
    '

