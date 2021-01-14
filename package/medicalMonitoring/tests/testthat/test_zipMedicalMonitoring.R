context("Test zip of reports")

tmpdir <- tempdir()

test_that("Creation of redirect page", {
      
      tmpRedirect <- file.path(tmpdir, "tmp")
      dir.create(tmpRedirect)
      createRedirectPage(
          dir = tmpRedirect,
          redirectPage = file.path(tmpRedirect, "report.html")
      )
      expect_true("report.html" %in% list.files(tmpRedirect))
      
      textInHtml <- readLines(file.path(tmpRedirect, "report.html"))
      expect_is(textInHtml, "character")
      expect_true(any(grepl("1-introduction.html", textInHtml)))
      
    })

test_that("Error in input arguments", {
      
      expect_error(
          zipMedicalMonitoring(reportDir = 1),
          "Input arguments should be characters."
      )
      expect_error(
          zipMedicalMonitoring(newDir = 1),
          "Input arguments should be characters."
      )
      
    })

test_that("Error in not existing directory", {
 
      expect_error(
          zipMedicalMonitoring(reportDir = "folderReport"),
          "Directory specified in 'reportDir' does not exist."
      )
      
    })

test_that("Error for files not available", {
      
      emptyDir <- file.path(tmpdir, "emptyDir")
      dir.create(file.path(tmpdir, "emptyDir"))
      expect_error(
          zipMedicalMonitoring(reportDir = emptyDir),
          "No files available in the 'reportDir'."
      )
      
    })

test_that("Zip reports", {
      
      tmpZip <- file.path(tmpdir, "tmpZip")
      dir.create(tmpZip)
      reportDir <- file.path(tmpZip, "reportFolder")
      dir.create(reportDir)
      medMonFile <- file.path(reportDir, "medMonFile.html")
      write(x = c('<!DOCTYPE html> </html>'), medMonFile)
      
      newDir <- file.path(tmpZip, "report_dependencies")
      zipFolder <- file.path(tmpZip, "report.zip") 
      
      expect_silent(
          zipMedicalMonitoring(
              reportDir = reportDir,
              newDir = newDir,
              redirectPage = file.path(tmpZip, "report.html"),
              zipFolder = zipFolder       
          )
      )
      
      expect_identical(
          list.files(tmpZip),
          c(
              "report_dependencies",
              "report.html", "report.zip",
              "reportFolder"             
          )
      )
      expect_identical(
          list.files(newDir),
          "medMonFile.html"
      )
      
    })


#zipMedicalMonitoring(
#    reportDir = "~/Downloads/SMC_demo_20201130/report_dependencies",
#    newDir = "~/Desktop/medMon",
#    redirectPage = file.path("~/Desktop", "report.html"),
#    zipFolder = file.path("~/Desktop", "report.zip")       
#)
#
#createRedirectPage(
#    redirectPage = file.path("~/Desktop", "report.html"),
#    dir = file.path("medMon")
#)

