context("Test utility functions for plot facetting")

test_that("Facetting layout is correctly set when the number of columns is not specified", {

	data <- data.frame(A = c(1, 2), B = c(3, 4))
	facetPars <- list(
		facets = 2,
		ncol = NULL,
		nrow = 1
	)
	res <- clinDataReview:::setFacetLayoutWrap(data = data, facetPars = facetPars)
	expect_type(res, "list")
	expect_equal(object = res$nrow, expected = 1)
	expect_equal(object = res$ncol, expected = 2)
      
})

test_that("Facetting layout is correctly set when the number of rows is not specified", {
			
	data <- data.frame(A = c(1, 2), B = c(3, 4))
      
	facetPars <- list(
		facets = 2,
		ncol = 3,
		nrow = NULL
	)
      
	res <- clinDataReview:::setFacetLayoutWrap(data = data, facetPars = facetPars)
	expect_type(res, "list")
	expect_equal(object = res$ncol, expected = 3)
	expect_equal(object = res$nrow, expected = 1)
      
})