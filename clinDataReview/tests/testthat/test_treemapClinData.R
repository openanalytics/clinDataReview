context("Visualize clinical data with a treemap")

library(plotly)

test_that("A treemap is successfully created", {
			
	data <- data.frame(
		parent = c("A", "A", "A", "B", "B"),
		child = c("a", "b", "Total", "c", "Total"),
		n = c(1, 2, 6, 5, 5)
	)
			
	pl <- treemapClinData(
		data = data,
		vars = c("parent", "child"),
		valueVar = "n"
	)
    expect_s3_class(pl, "plotly")
    
})

test_that("An interactive table is created in addition to the treemap", {
		
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

test_that("A treemap is successfully created with the count type set to 'total'", {
	
	data <- data.frame(
		parent = c("A", "A", "A", "B", "B"),
		child = c("a", "b", "Total", "c", "Total"),
		n = c(1, 2, 6, 5, 5)
	)
	expect_silent(
		treemapClinData(
			data = data,
			vars = c("parent", "child"), 
			valueVar = "n",
			valueType = "total"
		)
	)
	
})

test_that("A color variable is correctly set in the treemap", {
			
	data <- data.frame(
		parent = c("A", "A", "A", "B", "B"),
		child = c("a", "b", "Total", "c", "Total"),
		n = c(1, 2, 6, 5, 5),
		mean = c(0.5, 5, 3, 4, 2)
	)
			
	# create plot
	pl <- treemapClinData(
		data = data,
		vars = c("parent", "child"),
		valueVar = "n", valueLab = "Number of patients with adverse events",
		colorVar = "mean", colorLab = "Mean severity"
	)
	data$ids <- with(data, paste(parent, child, sep = "-"))
	data$ids <- sub("-Total", "", data$ids)
	
	plData <- plotly_build(pl)$x$data[[1]]
	colors <- plData$marker$colors
	ids <- plData$ids
	
	colorsData <- data[match(ids, data$ids), "mean"]
	statPerColor <- tapply(colorsData, colors, function(x) range(x))
	statPerColorRank <- statPerColor[order(sapply(statPerColor, min), decreasing = FALSE)]
	isColorGroupOfStat <- all(diff(unlist(statPerColorRank)) >= 0)
	expect_true(
		object = isColorGroupOfStat, 
		label = "color var doesn't represent groups of specified summary statistic"
	)

})