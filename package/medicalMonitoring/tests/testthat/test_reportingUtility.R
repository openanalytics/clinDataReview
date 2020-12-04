context("Test reporting utility funcitons")

tmpdir <- tempdir()

test_that("Test extraction of md header", {
      
      title <- getMdHeader("A title")
      expect_is(title, "character")
      expect_identical(title, "\n# A title\n")
      
      titleTwo <- getMdHeader("A title", level = 2)
      expect_true(grepl("##", titleTwo))
      
    })

test_that("Invisible output from 'knitPrintMedicalMonitoring'", {
      
      emptyList <- list()
      class(emptyList) <- "medicalMonitoring"
      
      expect_silent(
          res <- knitPrintMedicalMonitoring(list = emptyList)
      )
      expect_null(res)
      
    })
