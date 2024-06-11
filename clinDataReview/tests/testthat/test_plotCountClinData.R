context("Visualize clinical count data")

library(plotly)
library(jsonlite)

test_that("An error is generated if a dataset with incorrect parent-child structure is provided for the count visualization", {
			
	data <- data.frame(
		parent = c("A", "A", "B"),
		child = c("a", "b", "c"),
		n = seq.int(3)
	)
	expect_error(
		plotCountClinData(data, 
			vars = c("parent", "child"), 
			valueVar = "n"
		),
		"Missing parent value"
	)
	
})
	
test_that("A warning is generated when parent variable(s) are smaller than the sum of their children", {				
	
	data <- data.frame(
		parent = c("A", "A", "A", "B", "B"),
		child = c("a", "b", "Total", "c", "Total"),
		n = c(1, 2, 1, 2, 1)
	)
	expect_warning(
		plotCountClinData(data, 
			vars = c("parent", "child"), 
			valueVar = "n"
		),
		"are smaller than the sum of their children"
	)
	
})

test_that("Counts with one specified parent variable are correctly visualized", {				
			
	data <- data.frame(
		parent = c("A", "A", "A", "B", "B"),
		child = c("a", "b", "Total", "c", "Total"),
		n = c(1, 2, 3, 5, 5)
	)
	pl <- plotCountClinData(data, 
		vars = c("parent", "child"), 
		valueVar = "n"
	)
	expect_s3_class(pl, "plotly")
	plotData <- as.data.frame(plotly_data(pl))
	
	# all records being retained?
	plotDataInput <- plotData[, colnames(data)]
	plotDataInputOrder <- plotDataInput[do.call(order, plotData), ]
	dataOrder <- data[do.call(order, data), ]
	expect_equal(
		object = plotDataInputOrder, 
		expected = dataOrder, 
		check.attributes = FALSE
	)
		
	# check extraction of hierarchical data
	dataHierar <- data.frame(
		hierarID = c("A-a", "A-b", "A", "B-c", "B"),
		hierarParent = c("A", "A", "", "B", ""),
		hierarLabel = c("a: 1", "b: 2", "A: 3", "c: 5", "B: 5"),
		stringsAsFactors = FALSE
	)
	# orders df the same
	dataHierarOrder <- dataHierar[do.call(order, dataHierar), ]
	plotDataInternal <- plotData[, colnames(dataHierar)]
	plotDataInternalOrder <- plotDataInternal[do.call(order, plotDataInternal), ]
	expect_equal(
		object = plotDataInternalOrder, 
		expected = dataHierarOrder, 
		check.attributes = FALSE
	)
			
})

test_that("Counts with child and parent elements with the same element are correctly visualized", {
			
	data <- data.frame(
		parent = c("A", "A", "A", "B", "B"),
		child = c("A", "b", "Total", "c", "Total"),
		n = c(1, 2, 3, 5, 5)
	)
	pl <- plotCountClinData(data, 
		vars = c("parent", "child"), 
		valueVar = "n"
	)
	
	plotData <- as.data.frame(plotly_data(pl))
	
	# check extraction of hierarchical data
	dataHierar <- data.frame(
		hierarID = c("A-A", "A-b", "A", "B-c", "B"),
		hierarParent = c("A", "A", "", "B", ""),
		hierarLabel = c("A: 1", "b: 2", "A: 3", "c: 5", "B: 5"),
		stringsAsFactors = FALSE
	)
	
	# orders df the same
	dataHierarOrder <- dataHierar[do.call(order, dataHierar), ]
	plotData <- plotData[,  colnames(dataHierar)]
	plotDataOrder <- plotData[do.call(order, plotData), ]
	expect_equal(
		object = plotDataOrder, 
		expected = dataHierarOrder, 
		check.attributes = FALSE
	)
	
})


test_that("A categorical color variable is correctly set in the count visualization", {				
			
	data <- data.frame(
		parent = c("A", "A", "A", "B", "B"),
		child = c("a", "b", "Total", "c", "Total"),
		n = c(1, 2, 3, 5, 5)
	)
	pl <- plotCountClinData(
		data, 
		vars = c("parent", "child"), 
		valueVar = "n",
		colorVar = "parent"
	)
	
	plotJson <- plotly_json(pl, jsonedit = FALSE)
	plotJson <- fromJSON(txt = plotJson, simplifyDataFrame = FALSE)
	colors <- plotJson$data[[1]]$marker$colors
	groups <- plotJson$data[[1]]$ids
	groupsParent <- sub("(\\w)-.+", "\\1", groups)
	nColorsPerGroup <- tapply(colors, groupsParent, function(x) length(unique(x)) == 1)
	expect_true(all(nColorsPerGroup))
	
})

test_that("A numeric color numerical variable with color range is correctly set in the count visualization", {				
			
	set.seed(123)
	data <- data.frame(
		parent = sample(LETTERS[seq.int(6)], 100, replace = TRUE),
		child = sample(letters[seq.int(6)], 100, replace = TRUE),
		n = seq.int(100)
	)
	data <- data[!duplicated(data[, c("parent", "child")]), ]
	totalN <- with(data, tapply(n, parent, sum))
	data <- rbind(data,
		data.frame(
			parent = names(totalN),
			child = "Total",
			n = totalN
		)
	)
	
	pl <- plotCountClinData(
		data, 
		vars = c("parent", "child"), 
		valueVar = "n",
		colorVar = "n"
	)
	
	plData <- plotly_build(pl)$x$data[[1]]
	colors <- plData$marker$colors
	ids <- plData$ids
	
	data$ids <- with(data, paste(parent, child, sep = "-"))
	data$ids <- sub("-Total", "", data$ids)
	colorsData <- data[match(ids, data$ids), "n"]
	statPerColor <- tapply(colorsData, colors, range)
	statPerColorRank <- statPerColor[order(sapply(statPerColor, min), decreasing = FALSE)]
	isColorGroupOfStat <- all(diff(unlist(statPerColorRank)) >= 0)
	expect_true(
		object = isColorGroupOfStat, 
		label = "color var doesn't represent groups of specified summary statistic"
	)
	
})

test_that("An interactive table is correctly included in the count visualization", {				
			
	data <- data.frame(
		parent = c("A", "A", "A", "B", "B"),
		child = c("a", "b", "Total", "c", "Total"),
		n = c(1, 2, 3, 5, 5),
		stringsAsFactors = TRUE
	)

	res <- plotCountClinData(
		data, 
		vars = c("parent", "child"), 
		valueVar = "n",
		table = TRUE
	)
	table <- res$table
	expect_s3_class(table, "datatables")
	
	tableData <- table$x$data
	
	tableInput <- cbind(
		data, 
		hierarID = c("A-a", "A-b", "A", "B-c", "B"),
		stringsAsFactors = TRUE
	)
	expect_setequal(
		object = colnames(tableInput), 
		expected = colnames(tableData)
	)
	
	tableData <- tableData[, colnames(tableInput)]
	
	expect_equal(
		object = tableInput[do.call(order, tableInput), ], 
		expected = tableData[do.call(order, tableData), ]
	)
			
})

test_that("The overall total is correctly included in the count visualization", {				
			
	data <- data.frame(
		parent = c("A", "A", "A", "B", "B", "Total"),
		child = c("a", "b", "Total", "c", "Total", "Total"),
		n = c(1, 2, 3, 4, 4, 7)
	)
	pl <- plotCountClinData(data, 
		vars = c("parent", "child"), 
		valueVar = "n"
	)
	expect_s3_class(pl, "plotly")
	plotData <- as.data.frame(plotly_data(pl))
			
	# all records being retained?
	plotDataInput <- plotData[, colnames(data)]
	plotDataInputOrder <- plotDataInput[do.call(order, plotData), ]
	dataOrder <- data[do.call(order, data), ]
	expect_equal(
		object = plotDataInputOrder, 
		expected = dataOrder, 
		check.attributes = FALSE
	)
			
	# check extraction of hierarchical data
	dataHierar <- data.frame(
		hierarID = c("A-a", "A-b", "A", "B-c", "B", "Overall"),
		hierarParent = c("A", "A", "Overall", "B", "Overall", ""),
		hierarLabel = c("a: 1", "b: 2", "A: 3", "c: 4", "B: 4", "Overall: 7"),
		stringsAsFactors = FALSE
	)
	# orders df the same
	dataHierarOrder <- dataHierar[do.call(order, dataHierar), ]
	plotDataInternal <- plotData[, colnames(dataHierar)]
	plotDataInternalOrder <- plotDataInternal[do.call(order, plotDataInternal), ]
	expect_equal(
		object = plotDataInternalOrder, 
		expected = dataHierarOrder, 
		check.attributes = FALSE
	)
			
})

test_that("A watermark is correctly included in a count visualization", {
  
  data <- data.frame(
    parent = c("A", "A", "A", "B", "B"),
    child = c("a", "b", "Total", "c", "Total"),
    n = c(1, 2, 3, 5, 5)
  )
  
  file <- tempfile(pattern = "watermark", fileext = ".png")
  getWatermark(file = file)
  
  pl <- plotCountClinData(
    data = data, 
    vars = c("parent", "child"), 
    valueVar = "n",
    watermark = file
  )
  
  # check that an image has been included below the plot
  plBuild <- plotly::plotly_build(pl)
  expect_equal(
    object = sapply(plBuild$x$layout$images, `[[`, "layer"),
    expected = "below"
  )
  
})