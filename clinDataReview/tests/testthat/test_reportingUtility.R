context("Test reporting utility functions")

test_that("A report header is correctly set", {
      
	title <- getMdHeader("A title")
	expect_type(title, "character")
	expect_identical(object = title, expected = "\n# A title\n")
	
})

test_that("A level is correctly set to a report header", {
      
	titleTwo <- getMdHeader("A title", level = 2)
	expect_match(object = titleTwo, regexp = "##")
      
})


