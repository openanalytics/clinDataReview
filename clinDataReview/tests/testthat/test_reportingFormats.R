context("Test reporting formats")

test_that("A gitbook clinical data review format is correctly created", {
      
	output <- gitbook_clinDataReview_report()
	expect_s3_class(output, "rmarkdown_output_format")
      
	output <- gitbook_clinDataReview_report(
		split_by = 'section',
	)
	expect_s3_class(output, "rmarkdown_output_format")
	unlink("gitbook.css")
      
})

test_that("An HTML clinical data review format is correctly created", {
      
	res <- html_clinDataReview_report()
	expect_s3_class(res, "rmarkdown_output_format")
	expect_identical(
		object = res$pandoc$to,
		expected = "html"
	)
      
})

test_that("The reference to a logo is correctly created", {
    
	# create an example logo
	data(iris)
	vars <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")	
	pathPlotBasic <- tempfile(pattern = "scatterplotIris-basic", fileext = ".png")
	png(pathPlotBasic)
	pairs(iris[, vars])
	tmp <- dev.off()
			
	expect_silent(
		res <- clinDataReview:::addLogoGitbook(
			logo = pathPlotBasic
		)
	)
	expect_type(res, "list")
	expect_named(res, "in_header")
	expect_type(res$in_header, "character")
      
})

test_that("A logo is correctly added to a gitbook clinical data review report", {
			
	# create an example logo
	data(iris)
	vars <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")	
	pathPlotBasic <- tempfile(pattern = "scatterplotIris-basic", fileext = ".png")
	png(pathPlotBasic)
	pairs(iris[, vars])
	tmp <- dev.off()
      
	expect_silent(
 		res <- gitbook_clinDataReview_report(logo = pathPlotBasic)
	)
	expect_s3_class(res, "rmarkdown_output_format")
	unlink("gitbook.css")
      
})