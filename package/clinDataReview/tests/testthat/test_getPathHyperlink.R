context("Get path hyperlink")

test_that("Path is correctly extracted from a single URL", {
			
	xPath <- paste0("./this-is_a_test/report", 1:10, ".pdf")
	xLink <- paste0(
		'<a href="', xPath, 
		'" target="_blank">', seq_along(xPath), '</a>'
	)
	
	expect_identical(
		object = getPathHyperlink(xLink),
		expected = xPath,
		info = "unique URL"
	)
	
})

test_that("Path is correctly extracted from multiple URLs", {
			
	xPath <- paste0("./this-is_a_test/report", 1:10, ".pdf")
	xLink <- paste0(
		'<a href="', xPath, 
		'" target="_blank">', seq_along(xPath), '</a>'
	)
	xLinkCombined <- toString(xLink)
	
	expect_identical(
		object = getPathHyperlink(xLinkCombined),
		expected = toString(xPath),
		info = "multiple URLs"
	)
	
})

test_that("Path is correctly extracted from a path", {
			
	xPath <- paste0("./this-is_a_test/report", 1:10, ".pdf")
	expect_identical(
		object = getPathHyperlink(xPath),
		expected = xPath,
		info = "multiple paths (no URL)"
	)
	
})

test_that("Original value is returned if no path is available in the input", {
			
	xNoUrl <- "blabla, blabla1"
	expect_identical(
		object = getPathHyperlink(xNoUrl),
		expected = xNoUrl,
		info = "non URL"
	)
	
})
