context("Test skeleton")

dirName <- tempfile("skeleton")
dirSubfunctions <- file.path(dirName, "testSubfunctions")
dirData <- file.path(dirSubfunctions, "data")
dirSkeletonFiles <- file.path(dirSubfunctions, "files")
dirSkeleton <- file.path(dirName, "skeleton")

test_that("Move data from clinUtils to folder", {
      
	expect_silent(
		clinDataReview:::moveXpt(dirData)
	)
	res <- list.files(dirData)
	expect_length(res, 8)
	expect_true(all(grepl("xpt", res)))
      
})

test_that("Create example metadata file", {
      
      expect_silent(
          clinDataReview:::createExampleMetadata(dirData)
      )
      res <- list.files(dirData)
      expect_length(res, 9)
      expect_true(any(grepl("metadata.yml", res)))
      
    })

test_that("Move skeleton files", {
      
      expect_silent(
          clinDataReview:::moveSkeletonFiles(dirSkeletonFiles)
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
          clinDataReview:::createMainConfigSkeleton(
              dirSkeletonFiles,
              dirData
          )
      )
      res <- list.files(dirSkeletonFiles)
      expect_true(any("config.yml" %in% res))
      
    })

test_that("Create skeleton", {
      
      expect_message(
          createClinDataReviewReportSkeleton(dirSkeleton),
          "The skeleton of the report is ready!"
      )
      res <- list.files(dirSkeleton)
      expect_identical(
          res,
          c("config", "data", "figures", "index.Rmd")
      )
	  unlink(dirSkeleton, recursive = TRUE)
      
      
    })

test_that("Warning of skeleton creation", {
      
	createClinDataReviewReportSkeleton(dirSkeleton)
	expect_warning(
		createClinDataReviewReportSkeleton(dirSkeleton),
		".+ is not empty."
	)
	unlink(dirSkeleton, recursive = TRUE)
      
})

test_that("skeleton report is successfully executed", {
			
	skip_on_cran() 
		
	createClinDataReviewReportSkeleton(dirSkeleton)
	
	# Track warnings during execution of example report:
	warn <- NULL
	resReport <- withCallingHandlers(
		render_clinDataReviewReport(
			inputDir = dirSkeleton,
			outputDir = file.path(dirSkeleton, "report"), 
			intermediateDir = file.path(dirSkeleton, "interim"),
			quiet = TRUE
		),
		warning = function(w){
			warn <<- append(warn, conditionMessage(w))
			invokeRestart("muffleWarning")
		}
	)
	expect_true(file.exists(resReport))
	
	# check that import parameters is successful & all chapters are successfully created
	expect_false(any(
		grepl(
			"Extraction of the parameters.*failed|Rendering of the.*report failed",
			warn
		)
	))
			
})


