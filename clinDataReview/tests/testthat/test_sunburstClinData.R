context("Visualize clinical data with a sunburst")

test_that("A sunburst is successfully created", {
			
	data <- data.frame(
		parent = c("A", "A", "A", "B", "B"),
		child = c("a", "b", "Total", "c", "Total"),
		n = c(1, 2, 6, 5, 5)
	)
	pl <- sunburstClinData(
		data = data,
		vars = c("parent", "child"),
		valueVar = "n"
	)
    expect_s3_class(pl, "plotly")

})

test_that("An interactive table is created in addition to the sunburst", {
			
	data <- data.frame(
		parent = c("A", "A", "A", "B", "B"),
		child = c("a", "b", "Total", "c", "Total"),
		n = c(1, 2, 6, 5, 5)
	)
	res <- sunburstClinData(
		data = data,
		vars = c("parent", "child"),
		valueVar = "n", 
		table = TRUE
	)
	
	expect_s3_class(res$table, "datatables")
	
})

test_that("A warning is generated if the count type is set to 'total' but a parent node is smaller than its children in the sunburst", {
			
	data <- data.frame(
		parent = c("A", "A", "A", "B", "B"),
		child = c("a", "b", "Total", "c", "Total"),
		n = c(1, 2, 1, 5, 5)
	)
			
	# returns a warning: valueType: 'total' -> 'relative'
	expect_warning(
		pl <- sunburstClinData(
			data = data,
			vars = c("parent", "child"), valueVar = "n", 
			valueType = "total"
		),
		regexp = "'valueType' is set to 'relative' (instead of 'total')",
		fixed = TRUE
	)
	
})

test_that("A sunburst is successfully created with the count type set to 'total' in the sunburst", {
			
	data <- data.frame(
		parent = c("A", "A", "A", "B", "B"),
		child = c("a", "b", "Total", "c", "Total"),
		n = c(1, 2, 6, 5, 5)
	)
	
	expect_silent(
		sunburstClinData(
			data = data,
			vars = c("parent", "child"), valueVar = "n", 
			valueType = "total"
		)
	)
	
})
			
