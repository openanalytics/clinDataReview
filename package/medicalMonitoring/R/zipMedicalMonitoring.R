
#' @importFrom utils zip
zipMedicalMonitoring <- function(
    reportDir = "./report",
    dependencyDir = "./report_dependencies",
    redirectPage = "report.html", zipFolderName = "report.zip"
) {
  
  folderName <- basename(dependencyDir)  
  dir.create(folderName)
  
  reportFiles <- list.files(reportDir, full.names = TRUE)  
  file.copy(reportFiles, dependencyDir, recursive = TRUE)
  
  createRedirectPage(redirectPage) 
  zip(zipFolderName, c(redirectPageName, folderName))
  
}



createRedirectPage <- function(
    redirectPage = "report.html",
    dependencyDir = "./report_dependencies"
) {
  
  linkToPage <- file.path(dependencyDir, "1-introduction.html")
  
  htmlPage <- c(
      sprintf(
          '<!DOCTYPE html>
              <html lang="" xml:lang="">
              <head>
              <meta http-equiv="refresh" content="0; URL=\'%s\'" />
              </head>
              <body>
              </body>
              </html>',
          linkToPage
      )
  )
  write(x = htmlPage, file = redirectPage)
  
}