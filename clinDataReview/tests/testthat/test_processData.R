context("Process data")

test_that("Data is correctly processed with multiple annotations and filtering steps", {
			
	dataLB <- data.frame(
		USUBJID = 1:5,
		AVAL = rnorm(5),
		stringsAsFactors = FALSE
	)
			
	dataDM <- data.frame(
		USUBJID = 1 : 5,
		SEX = c("F", "F", "M", "M", "M"),
		RACE = c("WHITE", "HISPANIC", "WHITE", "NOT HISPANIC", "OTHER"),
		COUNTRY = c("US", "BE", "FR", "BE", "US"),
		ACTARM = c("Screen Failure", "TRT", "Screen Failure", "Placebo", "Screen Failure"),
		stringsAsFactors = FALSE
	)
	
	labelVars <- c(
		USUBJID = "Sub ID",
		AVAL = "Values",
		SEX = "Sex",
		RACE = "Race",
		COUNTRY = "Country",
		ACTARM = "Actual Arm"
	)
			
	 # each pre-processing function separately
	dataLBAnnot <- annotateData(
		data = dataLB, 
		annotations = list(data = dataDM, vars = c("SEX", "RACE", "COUNTRY", "ACTARM")), 
		labelVars = labelVars
	)
	labelVarsAnnot <- attr(dataLBAnnot, "labelVars")
	dataLBAnnotTreatment <- filterData(
		data = dataLBAnnot, 
		filters = list(var = "ACTARM", value = "Screen Failure", rev = TRUE), 
		labelVars = labelVarsAnnot
	)
      
	# all at once
	dataLBAnnotTreatment2 <- processData(
		data = dataLB,
		processing = list(
			list(annotate = list(data = dataDM, vars = c("SEX", "RACE", "COUNTRY", "ACTARM"))),
			list(filter = list(var = "ACTARM", value = "Screen Failure", rev = TRUE))
 		),
		labelVars = labelVars
	)
      
	expect_identical(
		object = dataLBAnnotTreatment2, 
		expected = dataLBAnnotTreatment
	)
      
})

test_that("An error is generated when each processing step is not specified as a list", {
			
	dataLB <- data.frame(
		USUBJID = 1:5,
		AVAL = rnorm(5),
		stringsAsFactors = FALSE
	)
			
	dataDM <- data.frame(
		USUBJID = 1 : 5,
		SEX = c("F", "F", "M", "M", "M"),
		RACE = c("WHITE", "HISPANIC", "WHITE", "NOT HISPANIC", "OTHER"),
		COUNTRY = c("US", "BE", "FR", "BE", "US"),
		ACTARM = c("Screen Failure", "TRT", "Screen Failure", "Placebo", "Screen Failure"),
		stringsAsFactors = FALSE
	)
      
	# in case user forgets the '-' in the config file
	expect_error(
		res <- processData(
			data = dataLB,
			processing = list(
				annotate = list(data = dataDM, vars = c("SEX", "RACE", "COUNTRY", "ACTARM")),
				filter = list(var = "ACTARM", value = "Screen Failure", rev = TRUE)
			)
		)
	)
      
})
