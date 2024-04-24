context("Test report skeleton")

library(tools)

test_that("Example xpt files are correctly extracted to the specified folder", {
			
	dirData <- tempfile("data")
	clinDataReview:::moveXpt(dirData)
	filesCopied <- list.files(dirData)
	
	# check that the file extensions are xpt
	expect_setequal(object = tools::file_ext(filesCopied), expected = "xpt")
	
	# check that all files available in the clinUtils package have been copied
	filesClinUtils <- list.files(
		path = system.file("extdata", "cdiscpilot01", "SDTM", 
			package = "clinUtils"),
		pattern = "*.xpt"
	)
	expect_setequal(object = filesClinUtils, expected = filesCopied)

})

test_that("An example metadata file is correctly created", {
      
	dirData <- tempfile("data")
	clinDataReview:::createExampleMetadata(dirData)
	res <- list.files(dirData)
	expect_length(res, 1)
	expect_equal(object = basename(res), expected = "metadata.yml")
      
})

test_that("Report skeleton files are correctly copied to the specified folder", {
			
	dirSkeletonFiles <- tempfile("skeleton")
	tmp <- clinDataReview:::moveSkeletonFiles(dirSkeletonFiles)
	res <- list.files(dirSkeletonFiles)
	expect_setequal(
		object = res,
		expected = c("config", "figures", "index.Rmd")
	)
	resConfig <- list.files(file.path(dirSkeletonFiles, "config"))
	expect_setequal(object = file_ext(resConfig), expected = "yml")
      
	resFigures <- list.files(file.path(dirSkeletonFiles, "figures"))
	expect_setequal(object = file_ext(resFigures), expected = c("svg", "png"))
      
})

test_that("Example of the main config file is correctly created", {
      
	dirSkeleton <- tempfile("config")
	clinDataReview:::createMainConfigSkeleton(
		dir = dirSkeleton,
		dirData = tempfile("data")
	)
	res <- list.files(dirSkeleton)
	expect_equal(object = res, expected = "config.yml")
      
})

test_that("A report skeleton, consisting of config files, XPT datasets, figures and index file is correctly created", {

	dirSkeleton <- tempfile("skeleton")
	expect_message(
		createClinDataReviewReportSkeleton(dirSkeleton),
		"The skeleton of the report is ready!"
	)
	res <- list.files(dirSkeleton)
	expect_identical(
		object = res,
		expected = c("config", "data", "figures", "index.Rmd")
	)

})

test_that("A warning is generated during the skeleton creation when the specified folder is not empty", {

	dirSkeleton <- tempfile("skeleton")
      
	createClinDataReviewReportSkeleton(dirSkeleton)
	expect_warning(
		createClinDataReviewReportSkeleton(dirSkeleton),
		".+ is not empty."
	)
      
})

test_that("A skeleton report is successfully executed", {
			
	skip_on_cran() 
	
	# fix for: 'Using anchor_sections requires Pandoc 2.0+'
	skip_if_not(
			condition = rmarkdown::pandoc_available(version = "2.0"), 
			message = "pandoc 2.0 is not available"
	)
		
	dirSkeleton <- tempfile("skeleton")
	createClinDataReviewReportSkeleton(dirSkeleton)
	
	# Track warnings during execution of example report:
	warn <- NULL
	resReport <- withCallingHandlers(
		expr = render_clinDataReviewReport(
			inputDir = dirSkeleton,
			outputDir = file.path(dirSkeleton, "report"), 
			intermediateDir = file.path(dirSkeleton, "interim"),
			quiet = TRUE, # suppress printing of pandoc cmd line
			verbose = FALSE
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

test_that("A skeleton report is successfully executed in parallel", {
  
  skip_on_cran() 
  
  # fix for: 'Using anchor_sections requires Pandoc 2.0+'
  skip_if_not(
    condition = rmarkdown::pandoc_available(version = "2.0"), 
    message = "pandoc 2.0 is not available"
  )
  
  dirSkeleton <- tempfile("skeleton")
  createClinDataReviewReportSkeleton(dirSkeleton)
  
  outputDir <- file.path(dirSkeleton, "report")
  resReport <- render_clinDataReviewReport(
    inputDir = dirSkeleton,
    outputDir = outputDir, 
    intermediateDir = file.path(dirSkeleton, "interim"),
    quiet = TRUE, # suppress printing of pandoc cmd line
    verbose = FALSE,
    nCores = max(floor(parallel::detectCores()/2), 2)
  )
  resReport <- file.path(dirSkeleton, "report", "1-introduction.html")
  expect_true(file.exists(resReport))
  
})
