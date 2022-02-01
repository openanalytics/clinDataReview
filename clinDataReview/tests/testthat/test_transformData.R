context("Transform data")

test_that("Data is correctly transformed from a long to a wide format", {
			
	data <- data.frame(
		USUBJID = c("1", "1", "2", "2"),
		PARAM = c("param1", "param2", "param1", "param2"),
		VALUE = c(1, 2, 3, 4),
		stringsAsFactors = FALSE
	)
			
	transformation <- list(
		type = "pivot_wider",
		varsID = "USUBJID",
		varPivot = "PARAM",
		varsValue = "VALUE"
	)
	dataTransform <- transformData(
		data = data, 
		transformations = transformation
	)
	expect_s3_class(dataTransform, "data.frame")
	
	expect_equal(
		object = dataTransform,
		expected = data.frame(
			USUBJID = c("1", "2"),
			`VALUE.param1` = c(1, 3),
			`VALUE.param2` = c(2, 4),
			stringsAsFactors = FALSE
		),
		check.attributes = FALSE
	)
	
	expect_mapequal(
		object = attr(dataTransform, "labelVars"),
		expected = c(
			`VALUE.param1` = "param1 VALUE",
			`VALUE.param2` = "param2 VALUE"
		)
	)

})

test_that("An error is generated if the data is not available when the data is transformed", {
	expect_error(transformData(data))
})

test_that("An error is generated if the type of transformation is not specified", {

	transformation <- list(type = NULL)
	expect_error(transformData(data, transformations = transformation))

})

test_that("An error is generated if the specified transformation is not available", {
			
	transformation <- list(type = "transform")
	expect_error(transformData(data, transformations = transformation))
	
})

test_that("An error is generated if the ID variable is not specified for a transformation from long to wide format", {

	transformation <- list(type = "pivot_wider")
	expect_error(transformData(data, transformations = transformation))
	
})

test_that("An error is generated if the variable to pivot is not specified for a transformation from long to wide format", {
			
	transformation <- list(type = "pivot_wider", varsID = "TRTP")
	expect_error(transformData(data, transformations = transformation))
      
})

test_that("A message is generated when requested when data is transformed from a long to a wide format", {

	data <- data.frame(
		USUBJID = c("1", "1", "2", "2"),
		PARAM = c("param1", "param2", "param1", "param2"),
		VALUE = c(1, 2, 3, 4),
		stringsAsFactors = FALSE
	)
	transformation <- list(
		type = "pivot_wider",
		varsID = "USUBJID",
		varPivot = "PARAM",
		varsValue = "VALUE"
	)
	expect_message(
		transformData(
			data = data,
			transformations = transformation, 
			verbose = TRUE
		)
	)

})

test_that("Labels for the variables in the transformation are correctly extracted from the labels of all variables ", {
			
	data <- data.frame(
		USUBJID = c("1", "1", "2", "2"),
		PARAM = c("param1", "param2", "param1", "param2"),
		VALUE = c(1, 2, 3, 4),
		stringsAsFactors = FALSE
	)
	transformation <- list(
		type = "pivot_wider",
		varsID = "USUBJID",
		varPivot = "PARAM",
		varsValue = "VALUE"
	)
      
	labelVars <- c(
		USUBJID = "Subject ID",
		PARAM = "Parameter",
		VALUE = "Value"
	)
	dataTransform <- transformData(
		data = data,
		transformations = transformation,
		labelVars = labelVars
	)
	
	attrDataTransf <- attr(dataTransform, "labelVars")
	attrDataTransf <- attrDataTransf[
		grep("VALUE\\.param.+", names(attrDataTransf), invert = TRUE)
	]
	expect_mapequal(
		object = attrDataTransf,
		expected = labelVars["USUBJID"]
	)
	
})

test_that("The data is correctly transformed with multiple successive transformations", {

	data <- data.frame(
		USUBJID = c("1", "1", "2", "2"),
		PARAMGR1 = c("param1", "param2", "param1", "param2"),
		PARAMGR2 = c("param3", "param3", "param4", "param4"),
		VALUE = c(1, 2, 3, 4),
		stringsAsFactors = FALSE
	)
      
	transformation <- list(
		list(
			type = "pivot_wider",
			varsID = c("USUBJID", "PARAMGR2"),
			varPivot = "PARAMGR1",
			varsValue = "VALUE"
          ),
		list(
			type = "pivot_wider",
			varsID = "USUBJID",
			varPivot = "PARAMGR2",
			varsValue = c("VALUE.param1", "VALUE.param2")
		)
	)
	dataTransform <- transformData(
		data = data, 
		transformations = transformation
	)
	expect_equal(
		object = dataTransform,
		expected = data.frame(
			USUBJID = c("1", "2"),
			`VALUE.param1.param3` = c(1, NA_real_),
			`VALUE.param2.param3` = c(2, NA_real_),
			`VALUE.param1.param4` = c(NA_real_, 3),
			`VALUE.param2.param4` = c(NA_real_, 4),
			stringsAsFactors = FALSE
		),
		check.attributes = FALSE
	)
	
	expect_mapequal(
		object = attr(dataTransform, "labelVars"),
		expected = c(
			`VALUE.param1.param3` = "param3 param1 VALUE",
			`VALUE.param2.param3` = "param3 param2 VALUE",
			`VALUE.param1.param4` = "param4 param1 VALUE",
			`VALUE.param2.param4` = "param4 param2 VALUE"
		)
	)

})