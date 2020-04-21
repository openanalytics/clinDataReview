context("getPathHyperlink")

test_that("Correct extraction of hyperlinks", {
			
	xPath <- paste0("./this-is_a_test/report", 1:10, ".pdf")
	xLink <- paste0(
		'<a href="', xPath, 
		'" target="_blank">', seq_along(xPath), '</a>'
	)
	xLinkCombined <- toString(xLink)
	
	expect_identical(
		object = getPathHyperlink(xLink),
		expected = xPath,
		info = "unique URL"
	)
	
	expect_identical(
		object = getPathHyperlink(xLinkCombined),
		expected = toString(xPath),
		info = "multiple URLs"
	)
	
	expect_identical(
		object = getPathHyperlink(xPath),
		expected = xPath,
		info = "multiple paths (no URL)"
	)
	
	xNoUrl <- "blabla, blabla1"
	expect_identical(
		object = getPathHyperlink(xNoUrl),
		expected = xNoUrl,
		info = "non URL"
	)
	
})
