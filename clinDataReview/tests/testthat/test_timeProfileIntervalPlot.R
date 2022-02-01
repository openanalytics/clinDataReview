context("Visualize time intervals in clinical data")

library(plotly)

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