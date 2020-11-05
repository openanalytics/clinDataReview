context("Test reporting utility funcitons")

test_that("Test extraction of md header", {
      
      title <- getMdHeader("A title")
      expect_is(title, "character")
      expect_identical(title, "\n# A title\n")
      
      titleTwo <- getMdHeader("A title", level = 2)
      expect_true(grepl("##", titleTwo))
      
    })

test_that("Test extraction of md header with settings", {
      
      tmpdir <- tempdir()
      pathFile <- file.path(tmpdir, "aFile.Rmd")
      file.create(file = pathFile)
      writeLines("knitr::current_input()", pathFile)
      
      settings <- list(
          rmd_files = pathFile,
          rmd_file_depth = "2"
      )
      titleFromSettings <- getMdHeader("A title", settings = settings)
      expect_is(titleFromSettings, "character")
      
    })