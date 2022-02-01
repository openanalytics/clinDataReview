context("Format to hierarchical data")

test_that("A warning is generated when missing records are filtered when formatting hierarchical data", {
			
	data <- data.frame(
		parent = c("A", "A", "A", "B", "B", NA_character_),
		child = c("a", "b", "Total", "c", "Total", NA_character_),
		n = c(1, 2, 1, 2, 1, 3)
	)
	expect_warning(
		clinDataReview:::formatToHierarchicalData(
			data = data,
			vars = c("parent", "child")
		),
		regexp = "record(s) are filtered from the data",
		fixed = TRUE
	)
   
})