context("Visualize clinical data with a barplot")

library(plotly)

test_that("A barplot is correctly created", {
			
	data <- data.frame(
		parent = c("A", "A", "B"),
		child = c("a", "b", "c"),
		n = c(1, 2, 5),
		stringsAsFactors = FALSE
	)
			
	# create plot
	pl <- barplotClinData(
		data = data,
		xVar = "child", colorVar = "parent",
		yVar = "n"
	)
	
	## check if input == output data
	
	# extract data from output object
	plData <- plotly_build(pl)$x$data
	
	# only bar aes
	plDataBar <- plData[sapply(plData, function(x) x$type == "bar")]
	
	plDataBarDf <- do.call(rbind,
		lapply(plDataBar, function(x) 
			data.frame(
				child = as.character(x[["x"]]), 
				n = x$y, 
				stringsAsFactors = FALSE
			)
		)
	)
	
	dataPlot <- data[, c("child", "n")]
	dataPlot <- dataPlot[match(plDataBarDf$child, dataPlot$child), ]
	expect_equivalent(object = plDataBarDf, expected = dataPlot)
	
})

test_that("An interactive table is created in addition to the barplot", {
		
	data <- data.frame(
		child = c("a", "b", "c"),
		n = c(1, 2, 5)
	)
			
	res <- barplotClinData(
		data = data,
		xVar = "child", yVar = "n",
		table = TRUE
	)
	
	expect_s3_class(res$table, "datatables")
	
})

test_that("A barplot is successfully created with selected hover variables", {
			
	data <- data.frame(
		parent = c("A", "A", "B"),
		child = c("a", "b", "c"),
		n = c(1, 2, 5)
	)
      
	plOutput <- barplotClinData(
		data = data, 
		xVar = "child", yVar = "n",
		hoverVars = c("parent", "child", "n")
	)
	expect_s3_class(plOutput, "plotly")
      
})

test_that("A warning is generated if a stacked barplot is generated and the x and color variables are not nested", {
		
	data <- data.frame(
		ANRIND = c("Low", "Normal", "High", "Normal"),
		AEDECOD = c("a", "a", "b", "b"),
		n = c(2, 3, 4, 1)
	)
	# create plot
	expect_warning(
		pl <- barplotClinData(
			data = data,
			xVar = "AEDECOD", colorVar = "ANRIND",
			yVar = "n",
			barmode = "stack"
		),
		"ordering of the x-variable"
	)
	
})

test_that("A text variable is correctly displayed in the barplot", {
	
	data <- data.frame(
		child = c("a", "b", "c"),
		n = c(1, 2, 5),
		`%` = c("12.5", "25.0", "62.5"),
		stringsAsFactors = FALSE,
		check.names = FALSE
	)
			
	# create plot
	pl <- barplotClinData(
		data = data,
		xVar = "child",
		yVar = "n", 
		textVar = "%"
	)
	
	# extract data from output object
	plData <- plotly_build(pl)$x$data
	
	# only bar aes
	plDataBar <- plData[sapply(plData, function(x) x$type == "bar")]
	
	# format plot data
	plDataBar <- lapply(plDataBar, `[`, c("x", "y", "text"))
	plDataBar <- lapply(plDataBar, cbind.data.frame)
	plDataBar <- do.call(rbind, plDataBar)
	plDataBar <- plDataBar[do.call(order, plDataBar), ]
	# text is converted to factor
	plDataBar[["text"]] <- as.character(plDataBar[["text"]])
	plDataBar[["x"]] <- as.character(plDataBar[["x"]])
	
	dataRef <- data[, c("child" ,"n", "%")]
	dataRef <- dataRef[do.call(order, dataRef), ]
	
	expect_equal(
		object = plDataBar, 
		expected = dataRef, 
		checkNames = FALSE,
		check.attributes = FALSE # ignore row.names
	)
	
})

