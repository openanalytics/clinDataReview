context("Test zip of reports")

tmpdir <- tempdir()

test_that("Creation of redirect page", {
      
      tmpRedirect <- tempfile()
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
      expect_error(
          zipMedicalMonitoring(redirectPage = 1),
          "Input arguments should be characters."
      )
      expect_error(
          zipMedicalMonitoring(zipFolder = 1),
          "Input arguments should be characters."
      )
      
    })

test_that("Error in not existing directory", {
      
      expect_error(
          zipMedicalMonitoring(reportDir = "folderReport"),
          "Directory specified in 'reportDir' does not exist."
      )
      
    })

test_that("Error in empty directory", {
      
      emptyDir <- tempfile()
      dir.create(emptyDir)
#      expect_true(dir.exists(emptyDir))
      expect_error(
          zipMedicalMonitoring(reportDir = emptyDir),
          "No files available in the 'reportDir'."
      )
      
    })

test_that("Zip reports", {
      
      tmpZip <- tempfile()
      dir.create(tmpZip)
      reportDir <- tempfile("reportFolder", tmpdir = tmpZip)
      dir.create(reportDir)
      medMonFile <- tempfile(pattern = "file", tmpdir = reportDir, fileext = ".html")
      write(x = c('<!DOCTYPE html> </html>'), medMonFile)
      
      newDir <- tempfile("report_dependencies", tmpdir = tmpZip)
      zipFolder <- tempfile("reports", tmpdir = tmpZip, fileext = ".zip")
      
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
              basename(newDir),
              "report.html",
              basename(reportDir),
              basename(zipFolder)
          )
      )
      expect_identical(
          list.files(newDir),
          basename(medMonFile)
      )
      
    })
