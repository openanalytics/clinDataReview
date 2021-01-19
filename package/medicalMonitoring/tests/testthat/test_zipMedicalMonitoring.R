context("Test zip of reports")

tmpdir <- tempdir()

test_that("Error in empty directory", {
      
#      emptyDir <- tempfile()
#      dir.create(emptyDir)
#      expect_true(dir.exists(emptyDir))
#      expect_error(
#          zipMedicalMonitoring(reportDir = tmpdir),
#          "No files available in the 'reportDir'."
#      )
      
    })

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
      
    })

test_that("Error in not existing directory", {
      
      expect_error(
          zipMedicalMonitoring(reportDir = "folderReport"),
          "Directory specified in 'reportDir' does not exist."
      )
      
    })


