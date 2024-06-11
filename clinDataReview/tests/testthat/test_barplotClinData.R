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


test_that("A selection variable is correctly included in a barplot", {
  
  data <- data.frame(
    parent = factor(c("A", "A", "B"), levels = c("B", "A")),
    child = c("a", "b", "c"),
    n = c(1, 2, 5),
    stringsAsFactors = FALSE
  )
  
  # create plot
  res <- barplotClinData(
    data = data,
    xVar = "child", 
    selectVars = "parent",
    yVar = "n"
  )
  
  # check the output:
  expect_s3_class(res, "clinDataReview")
  expect_named(res, expected = c("buttons", "plot"))
  expect_s3_class(res$plot, "plotly")
  
  expect_length(res$buttons, 1)
  
  # check button values
  btnScriptTag <- htmltools::tagQuery(res$buttons)$find("script")$selectedTags()
  buttonData <- jsonlite::fromJSON(txt = as.character(btnScriptTag[[1]]$children))
  expect_equal(object = buttonData$items$value, expected = levels(data$parent))
  
})

test_that("A watermark is correctly included in a barplot", {
  
  data <- data.frame(
    parent = c("A", "A", "B"),
    child = c("a", "b", "c"),
    n = c(1, 2, 5),
    stringsAsFactors = FALSE
  )
  
  file <- tempfile(pattern = "watermark", fileext = ".png")
  getWatermark(file = file)
  
  # create plot
  pl <- barplotClinData(
    data = data,
    xVar = "child", colorVar = "parent",
    yVar = "n",
    watermark = file
  )
  
  # check that an image has been included below the plot
  plBuild <- plotly::plotly_build(pl)
  expect_equal(
    object = sapply(plBuild$x$layout$images, `[[`, "layer"),
    expected = "below"
  )
  
})

test_that("Axis variable(s) are correctly included in a barplot", {
  
  data <- data.frame(
    child = c("a", "b", "c"),
    cat = c("A", "A", "B"),
    n = c(1, 2, 5),
    unit = "patients",
    stringsAsFactors = FALSE
  )
  
  # create plot
  pl <- barplotClinData(
    data = data,
    xVar = "child", xLabVar = "cat",
    yVar = "n", yLabVar = "unit",
    labelVars = c(
      child = "Child", cat = "Category", 
      n = "Number", unit = "Unit"
    )
  )
  
  plLayout <- plotly::plotly_build(pl)$x$layout
  
  # title for the x-axis
  expect_match(
    object = plLayout$xaxis$title$text, 
    regexp = "Child.+Category: A, B"
  )
  
  # title for the y-axis
  expect_match(
    object = plLayout$yaxis$title$text, 
    regexp = "Number.+Unit: patients"
  )
  
  # general title
  expect_match(
    object = plLayout$title$text, 
    regexp = "Number vs Child"
  )
  
})
