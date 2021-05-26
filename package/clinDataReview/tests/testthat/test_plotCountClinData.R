context("Visualization of count data for clinical data")

library(plotly)
library(jsonlite)

test_that("dataset with missing parent value returns an error", {
			
	data <- data.frame(
		varParent = c("A", "A", "B"),
		varChild = c("a", "b", "c"),
		n = seq.int(3)
	)
	expect_error(
		plotCountClinData(data, 
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
		plotCountClinData(data, 
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
		pl <- plotCountClinData(data, 
			vars = c("varParent", "varChild"), 
			valueVar = "n"
		)
	)
	expect_s3_class(pl, "plotly")
	plotData <- as.data.frame(plotly_data(pl))
	
	# all records being retained?
	plotDataInput <- plotData[, colnames(data)]
	plotDataInputOrder <- plotDataInput[do.call(order, plotData), ]
	dataOrder <- data[do.call(order, data), ]
	expect_equal(plotDataInputOrder, dataOrder, check.attributes = FALSE)
		
	# check extraction of hierarchical data
	dataHierar <- data.frame(
		hierarID = c("A-a", "A-b", "A", "B-c", "B"),
		hierarParent = c("A", "A", "", "B", ""),
		hierarLabel = c("a", "b", "A", "c", "B"),
		stringsAsFactors = FALSE
	)
	# orders df the same
	dataHierarOrder <- dataHierar[do.call(order, dataHierar), ]
	plotDataInternal <- plotData[, colnames(dataHierar)]
	plotDataInternalOrder <- plotDataInternal[do.call(order, plotDataInternal), ]
	expect_equal(plotDataInternalOrder, dataHierarOrder, check.attributes = FALSE)
			
})

test_that("child and parent variables can contain the same element", {
			
	data <- data.frame(
		varParent = c("A", "A", "A", "B", "B"),
		varChild = c("A", "b", "Total", "c", "Total"),
		n = c(1, 2, 3, 5, 5)
	)
	expect_silent(
		pl <- plotCountClinData(data, 
			vars = c("varParent", "varChild"), 
			valueVar = "n"
		)
	)
	
	plotData <- as.data.frame(plotly_data(pl))
	
	# check extraction of hierarchical data
	dataHierar <- data.frame(
		hierarID = c("A-A", "A-b", "A", "B-c", "B"),
		hierarParent = c("A", "A", "", "B", ""),
		hierarLabel = c("A", "b", "A", "c", "B"),
		stringsAsFactors = FALSE
	)
	
	# orders df the same
	dataHierarOrder <- dataHierar[do.call(order, dataHierar), ]
	plotData <- plotData[,  colnames(dataHierar)]
	plotDataOrder <- plotData[do.call(order, plotData), ]
	expect_equal(plotDataOrder, dataHierarOrder, check.attributes = FALSE)
	
})


test_that("specification of color categorical variable is successful", {				
			
	data <- data.frame(
		varParent = c("A", "A", "A", "B", "B"),
		varChild = c("a", "b", "Total", "c", "Total"),
		n = c(1, 2, 3, 5, 5)
	)
	expect_silent(
		pl <- plotCountClinData(
			data, 
			vars = c("varParent", "varChild"), 
			valueVar = "n",
			colorVar = "varParent"
		)
	)
	
	plotJson <- plotly_json(pl, jsonedit = FALSE)
	plotJson <- fromJSON(txt = plotJson, simplifyDataFrame = FALSE)
	colors <- plotJson$data[[1]]$marker$colors
	groups <- plotJson$data[[1]]$ids
	groupsParent <- sub("(\\w)-.+", "\\1", groups)
	nColorsPerGroup <- tapply(colors, groupsParent, function(x) length(unique(x)) == 1)
	expect_true(all(nColorsPerGroup))
	
})

test_that("specification of color numerical variable with color range is successful", {				
			
	set.seed(123)
	data <- data.frame(
		varParent = sample(LETTERS[seq.int(6)], 100, replace = TRUE),
		varChild = sample(letters[seq.int(6)], 100, replace = TRUE),
		n = seq.int(100)
	)
	data <- data[!duplicated(data[, c("varParent", "varChild")]), ]
	totalN <- with(data, tapply(n, varParent, sum))
	data <- rbind(data,
		data.frame(
			varParent = names(totalN),
			varChild = "Total",
			n = totalN
		)
	)
	
	expect_silent(
		pl <- plotCountClinData(
			data, 
			vars = c("varParent", "varChild"), 
			valueVar = "n",
			colorVar = "n"
		)
	)
	
	plData <- plotly_build(pl)$x$data[[1]]
	colors <- plData$marker$colors
	ids <- plData$ids
	
	data$ids <- with(data, paste(varParent, varChild, sep = "-"))
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

test_that("table is created with count visualization", {				
			
	data <- data.frame(
		varParent = c("A", "A", "A", "B", "B"),
		varChild = c("a", "b", "Total", "c", "Total"),
		n = c(1, 2, 3, 5, 5),
		stringsAsFactors = TRUE
	)

	expect_silent(
		res <- plotCountClinData(
			data, 
			vars = c("varParent", "varChild"), 
			valueVar = "n",
			table = TRUE
		)
	)
	table <- res$table
	expect_s3_class(table, "datatables")
	
	tableData <- table$x$data
	
	tableInput <- cbind(
		data, 
		hierarID = c("A-a", "A-b", "A", "B-c", "B"),
		stringsAsFactors = TRUE
	)
	expect_equal(sort(colnames(tableInput)), sort(colnames(tableData)))
	
	tableData <- tableData[, colnames(tableInput)]
	
	expect_equal(
		tableInput[do.call(order, tableInput), ], 
		tableData[do.call(order, tableData), ]
	)
			
})
