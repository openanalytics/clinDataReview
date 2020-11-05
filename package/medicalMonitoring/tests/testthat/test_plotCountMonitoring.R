context("Visualization of count data for medical monitoring")

library(plotly)
library(jsonlite)

test_that("dataset with missing parent value returns an error", {
			
	data <- data.frame(
		varParent = c("A", "A", "B"),
		varChild = c("a", "b", "c"),
		n = seq.int(3)
	)
	expect_error(
		plotCountMonitoring(data, 
			vars = c("varParent", "varChild"), 
			valueVar = "n"
		),
		"Missing parent value"
	)
	
})
	
test_that("parent variable(s) being not the sum of their children is flagged", {				
	
	data <- data.frame(
		varParent = c("A", "A", "A", "B", "B"),
		varChild = c("a", "b", "Total", "c", "Total"),
		n = c(1, 2, 1, 2, 1)
	)
	expect_warning(
		plotCountMonitoring(data, 
			vars = c("varParent", "varChild"), 
			valueVar = "n"
		),
		"are smaller than the sum of their children"
	)
	
})

test_that("creation of count visualization for specified one parent variable is successful", {				
			
	data <- data.frame(
		varParent = c("A", "A", "A", "B", "B"),
		varChild = c("a", "b", "Total", "c", "Total"),
		n = c(1, 2, 3, 5, 5)
	)
	expect_silent(
		pl <- plotCountMonitoring(data, 
			vars = c("varParent", "varChild"), 
			valueVar = "n"
		)
	)
	expect_s3_class(pl, "plotly")
	plotData <- plotly_data(pl)
	
	# all records being retained?
	vars <- c("varParent", "varChild")
	plotDataOrder <- plotData[do.call(order, plotData[, vars]), ]
	expect_equal(plotDataOrder[, colnames(data)], data, check.attributes = FALSE)
		
	# check extraction of hierarchical data
	dataHierar <- data.frame(
		hierarID = c("A-a", "A-b", "A", "B-c", "B"),
		hierarParent = c("A", "A", "", "B", ""),
		hierarLabel = c("a", "b", "A", "c", "B"),
		stringsAsFactors = FALSE
	)
	expect_equal(plotDataOrder[, colnames(dataHierar)], dataHierar, check.attributes = FALSE)
			
})

test_that("specification of color categorical variable is successful", {				
			
	data <- data.frame(
		varParent = c("A", "A", "A", "B", "B"),
		varChild = c("a", "b", "Total", "c", "Total"),
		n = c(1, 2, 3, 5, 5)
	)
	expect_silent(
		pl <- plotCountMonitoring(
			data, 
			vars = c("varParent", "varChild"), 
			valueVar = "n",
			colorVar = "varParent"
		)
	)
	
	plotJson <- fromJSON(txt = plotly_json(pl)$x$data, simplifyDataFrame = FALSE)
	colors <- plotJson$data[[1]]$marker$colors
	groups <- plotJson$data[[1]]$ids
	groupsParent <- sub("(\\w)-.+", "\\1", groups)
	nColorsPerGroup <- tapply(colors, groupsParent, function(x) length(unique(x)) == 1)
	expect_true(all(nColorsPerGroup))
	
})

