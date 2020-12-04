context("Test reporting utility funcitons")

tmpdir <- tempdir()

test_that("Test extraction of md header", {
      
      title <- getMdHeader("A title")
      expect_is(title, "character")
      expect_identical(title, "\n# A title\n")
      
      titleTwo <- getMdHeader("A title", level = 2)
      expect_true(grepl("##", titleTwo))
      
    })

#test_that("Test extraction of md header with settings", {
#      
#      pathFile <- file.path(tmpdir, "aFile.Rmd")
#      file.create(file = pathFile)
#      writeLines("knitr::current_input()", pathFile)
#      
#      settings <- list(
#          rmd_files = pathFile,
#          rmd_file_depth = "2"
#      )
#      titleFromSettings <- getMdHeader("A title", settings = settings)
#      expect_is(titleFromSettings, "character")
#      
#    })
#
test_that("Invisible output from 'knitPrintMedicalMonitoring'", {
      
      emptyList <- list()
      class(emptyList) <- "medicalMonitoring"
      
      expect_silent(
          res <- knitPrintMedicalMonitoring(list = emptyList)
      )
      expect_null(res)
      
    })

test_that("Invisible output from 'knitPrintMedicalMonitoring' for nested list", {
      
	emptyList <- list(firstList = list(A = structure(NULL, "medicalMonitoring")))
#      class(emptyList$firstList) <- "medicalMonitoring"
      
#      expect_silent(
#          res <- knitPrintMedicalMonitoring(list = emptyList)
#      )
      #expect_null(res)
      
    })
