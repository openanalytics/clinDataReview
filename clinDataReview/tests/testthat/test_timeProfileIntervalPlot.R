context("Visualize time intervals in clinical data")

library(plotly)
library(jsonlite)

test_that("A time interval plot is succesfully created", {
			
	data <- data.frame(
		subjectID = as.character(rep(c(1, 2, 3), each = 3)),
		startDay = c(NA, NA, 11, 44, 12, 7, 48, 54, 11),
		endDay = c(NA, NA, 12, NA, 26, 9, 50, NA, 13),
		stringsAsFactors = FALSE
	)
      
	pl <- timeProfileIntervalPlot(
		data = data,
		paramVar = "subjectID",
		timeStartVar = "startDay",
		timeEndVar = "endDay"
	)
	expect_s3_class(pl, "plotly")
      
})

test_that("A warning is generated if a custom subject variable is not specified in the time interval plot", {
      
	data <- data.frame(
		subjectID = as.character(rep(c(1, 2, 3), each = 3)),
		startDay = c(NA, NA, 11, 44, 12, 7, 48, 54, 11),
		endDay = c(NA, NA, 12, NA, 26, 9, 50, NA, 13),
		stringsAsFactors = FALSE
	)
	expect_warning(
		timeProfileIntervalPlot(
			data = data,
			paramVar = "subjectID",
			timeStartVar = "startDay",
			timeEndVar = "endDay",
			table = TRUE
		),
		"Subject ID variable: USUBJID is not available in the data, so it is ignored." 
	)
	  
})

test_that("An interactive table is created in addition to the time interval plot", {
			
	data <- data.frame(
		subjectID = as.character(rep(c(1, 2, 3), each = 3)),
		startDay = c(NA, NA, 11, 44, 12, 7, 48, 54, 11),
		endDay = c(NA, NA, 12, NA, 26, 9, 50, NA, 13),
		stringsAsFactors = FALSE
	)
	res <- timeProfileIntervalPlot(
		data = data,
		paramVar = "subjectID",
		timeStartVar = "startDay",
		timeEndVar = "endDay",
		table = TRUE,
		idVar = "subjectID"
	)
	
	expect_s3_class(res$table, "datatables")
	
})

test_that("Multiple parameter variables are successfully set in the time interval plot", {
      
	data <- data.frame(
		subjectID = as.character(rep(c(1, 2, 3), each = 3)),
		startDay = c(NA, NA, 11, 44, 12, 7, 48, 54, 11),
		endDay = c(NA, NA, 12, NA, 26, 9, 50, NA, 13),
		stringsAsFactors = FALSE
	)
	pl <- timeProfileIntervalPlot(
		data = data,
		paramVar = c("subjectID", "startDay"),
		timeStartVar = "startDay",
		timeEndVar = "endDay"
	)

	expect_s3_class(pl, "plotly")
     
})

test_that("Shapes of data points at the start or the end of a time interval are correctly set based on a variable", {
      
	data <- data.frame(
		subjectID = as.character(rep(c(1, 2, 3), each = 3)),
		startDay = c(NA, NA, 11, 44, 12, 7, 48, 54, 11),
		endDay = c(NA, NA, 12, NA, 26, 9, 50, NA, 13),
		stringsAsFactors = FALSE
	)
			
	pl <- timeProfileIntervalPlot(
		data = data,
		paramVar = "subjectID",
		timeStartVar = "startDay",
		timeEndVar = "endDay",
		timeStartShapeVar = "startDay",
		timeEndShapeVar = "endDay"
	)
	
	expect_s3_class(pl, "plotly")
            
})

test_that("A time profile plot is successfully created with selected hover variables", {
		
	data <- data.frame(
		subjectID = as.character(rep(c(1, 2, 3), each = 3)),
		startDay = c(NA, NA, 11, 44, 12, 7, 48, 54, 11),
		endDay = c(NA, NA, 12, NA, 26, 9, 50, NA, 13),
		stringsAsFactors = FALSE
	)
	pl <- timeProfileIntervalPlot(
		data = data,
		paramVar = "subjectID",
		timeStartVar = "startDay",
		timeEndVar = "endDay",
		timeStartShapeVar = "startDay",
		timeEndShapeVar = "endDay",
		hoverVars = c("startDay", "endDay")
	)
	
	expect_s3_class(pl, "plotly")  
      
})

test_that("A color variable is correctly set in the time interval plot", {
      
	data <- data.frame(
		subjectID = as.character(rep(c(1, 2, 3), each = 3)),
		startDay = c(NA, NA, 11, 44, 12, 7, 48, 54, 11),
		endDay = c(NA, NA, 12, NA, 26, 9, 50, NA, 13),
		stringsAsFactors = FALSE
	)
	
	pl <- timeProfileIntervalPlot(
		data = data,
		paramVar = "subjectID",
		timeStartVar = "startDay",
		timeEndVar = "endDay",
		timeStartShapeVar = "startDay",
		timeEndShapeVar = "endDay",
		colorVar = "startDay"
	)
	
	expect_s3_class(pl, "plotly")
      
})

test_that("A selection variable is correctly included in the time interval plot", {
  
  data <- data.frame(
    group = factor(c("A", "B", "B"), levels = c("B", "A")),
    subjectID = c(1, 2, 3),
    startDay = c(1, 3, 4),
    endDay = c(2, 5, 7),
    stringsAsFactors = FALSE
  )
  
  res <- timeProfileIntervalPlot(
    data = data,
    paramVar = "subjectID",
    timeStartVar = "startDay",
    timeEndVar = "endDay",
    selectVars = "group"
  )
  
  # check the output:
  expect_s3_class(res, "clinDataReview")
  expect_named(res, expected = c("buttons", "plot"))
  expect_s3_class(res$plot, "plotly")
  
  expect_length(res$buttons, 1)
  
  # check button values
  btnScriptTag <- htmltools::tagQuery(res$buttons)$find("script")$selectedTags()
  buttonData <- jsonlite::fromJSON(txt = as.character(btnScriptTag[[1]]$children))
  expect_equal(object = buttonData$items$value, expected = levels(data$group))
  
})

test_that("A label is correctly set for the selection variable in the time interval plot", {
  
  data <- data.frame(
    group1 = c("A", "B", "B"), 
    group2 = c("A1", "B1", "B2"), 
    subjectID = c(1, 2, 3),
    startDay = c(1, 3, 4),
    endDay = c(2, 5, 7),
    stringsAsFactors = FALSE
  )
  
  selectVars <- c("group1", "group2")
  selectLab <- c(group2 = "Group 2", group1 = "Group 1")
  res <- timeProfileIntervalPlot(
    data = data,
    paramVar = "subjectID",
    timeStartVar = "startDay",
    timeEndVar = "endDay",
    selectVars = selectVars, 
    selectLab = selectLab
  )
  
  expect_length(res$buttons, length(selectVars))
  
  for(iButton in seq_along(selectVars)){
    
    button <- res$buttons[[iButton]]
    buttonCnt <- button[sapply(button, `[[`, "name") == "div"]
    buttonCntChild <- buttonCnt[[1]]$children
    idx <- which(sapply(buttonCntChild, `[[`, "name") == "label")
    expect_equal(
      object = unname(unlist(buttonCntChild[[idx]]$children)),
      expected = unname(selectLab[selectVars[iButton]])
    )
  }
  
})

test_that("A watermark is correctly included in a time interval plot", {
  
  data <- data.frame(
    subjectID = as.character(seq_len(5)),
    startDay = c(11, 12, 7, 48, 11),
    endDay = c(12, 26, 9, 50, 13),
    stringsAsFactors = FALSE
  )
  
  file <- tempfile(pattern = "watermark", fileext = ".png")
  getWatermark(file = file)
  
  pl <- timeProfileIntervalPlot(
    data = data,
    paramVar = "subjectID",
    timeStartVar = "startDay",
    timeEndVar = "endDay",
    watermark = file
  )
  
  # check that an image has been included below the plot
  plBuild <- plotly::plotly_build(pl)
  expect_equal(
    object = sapply(plBuild$x$layout$images, `[[`, "layer"),
    expected = "below"
  )
  
})
