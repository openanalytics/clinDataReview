context("Test skeleton")

dirName <- tempdir()
dirSubfunctions <- file.path(dirName, "testSubfunctions")
dir.create(dirSubfunctions)
dirData <- file.path(dirSubfunctions, "data")
dir.create(dirData)
dirSkeletonFiles <- file.path(dirSubfunctions, "files")
dir.create(dirSkeletonFiles)
dirSkeleton <- file.path(dirName, "skeleton")
dir.create(dirSkeleton)

test_that("Error in skeleton creation", {
      
      expect_error(
          reportSkeleton(dirName = "ciao"),
          "Directory .+ does not exist"
      )
      
    })

test_that("Move data from clinUtils to folder", {
      
      expect_silent(
          moveXpt(dirData)
      )
      res <- list.files(dirData)
      expect_length(res, 8)
      expect_true(all(grepl("xpt", res)))
      
    })

test_that("Create example metadata file", {
      
      expect_silent(
          createExampleMetadata(dirData)
      )
      res <- list.files(dirData)
      expect_length(res, 9)
      expect_true(any(grepl("metadata.yml", res)))
      
    })

test_that("Move skeleton files", {
      
      expect_silent(
          moveSkeletonFiles(dirSkeletonFiles)
      )
      res <- list.files(dirSkeletonFiles)
      expect_identical(
          res,
          c("config", "figures", "index.Rmd")
      )
      resConfig <- list.files(file.path(dirSkeletonFiles, "config"))
      expect_true(all(grepl("yml", resConfig)))
      
      resFigures <- list.files(file.path(dirSkeletonFiles, "figures"))
      expect_true(all(grepl("svg|png", resFigures)))
      
    })

test_that("Create example config file", {
      
      expect_silent(
          createMainConfigSkeleton(
              dirSkeletonFiles,
              dirData
          )
      )
      res <- list.files(dirSkeletonFiles)
      expect_true(any("config.yml" %in% res))
      
    })

test_that("Create skeleton", {
      
      expect_message(
          reportSkeleton(dirSkeleton),
          "The skeleton of the report is ready!"
      )
      res <- list.files(dirSkeleton)
      expect_identical(
          res,
          c("config", "data", "figures", "index.Rmd")
      )
      
      
    })

test_that("Warning of skeleton creation", {
      
      expect_warning(
          reportSkeleton(dirSkeleton),
          ".+ is not empty."
      )
      
    })


