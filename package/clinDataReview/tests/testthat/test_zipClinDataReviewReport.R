context("Test zip of reports")

tmpdir <- tempdir()

test_that("Redirect page is created successfully", {
      
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

test_that("Input arguments for zip function are correctly checked", {
      
      expect_error(
          zipClinDataReview(reportDir = 1),
          "Input arguments should be characters."
      )
      expect_error(
          zipClinDataReview(newDir = 1),
          "Input arguments should be characters."
      )
      expect_error(
          zipClinDataReview(redirectPage = 1),
          "Input arguments should be characters."
      )
      expect_error(
          zipClinDataReview(zipFolder = 1),
          "Input arguments should be characters."
      )
      
    })

test_that("An error is generated during creation of the zip folder creation if the report directory doesn't exist", {
      
      expect_error(
          zipClinDataReview(reportDir = "folderReport"),
          "Directory specified in 'reportDir' does not exist."
      )
      
    })

test_that("An error is generated during creation of the zip folder creation if the report directory is empty", {
      
      emptyDir <- tempfile()
      dir.create(emptyDir)
	  newDir <- tempfile("report_dependencies")
#      expect_true(dir.exists(emptyDir))
      expect_error(
          zipClinDataReview(
			reportDir = emptyDir, 
			newDir = newDir
		),
          "No files available in the 'reportDir'."
      )
      
    })

test_that("The zip folder is successfully created", {
      
      tmpZip <- tempfile()
      dir.create(tmpZip)
      reportDir <- tempfile("reportFolder", tmpdir = tmpZip)
      dir.create(reportDir)
      medMonFile <- tempfile(pattern = "file", tmpdir = reportDir, fileext = ".html")
      write(x = c('<!DOCTYPE html> </html>'), medMonFile)
      
      newDir <- tempfile("report_dependencies", tmpdir = tmpZip)
      zipFolder <- tempfile("reports", tmpdir = tmpZip, fileext = ".zip")
      
      expect_silent(
          zipClinDataReview(
              reportDir = reportDir,
              newDir = newDir,
              redirectPage = file.path(tmpZip, "report.html"),
              zipFolder = zipFolder       
          )
      )
      
      expect_setequal(
          list.files(tmpZip),
          c(
              basename(newDir),
              "report.html",
              basename(reportDir),
              basename(zipFolder)
          )
      )
      expect_setequal(
          list.files(newDir),
          basename(medMonFile)
      )
      
    })
