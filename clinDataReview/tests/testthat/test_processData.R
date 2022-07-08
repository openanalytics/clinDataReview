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

test_that("Filter steps are run on the entire dataset when specified in the same filter step", {
  
  data <- data.frame(
    PARAM = c("A", "B", "B"),
    ANL01FL = c("Y", "Y", "N"),
    ANRIND = c("L", "N", "H"), 
    stringsAsFactors = FALSE
  )
  
  dataProcessed <- processData(
    data = data,
    processing = list(
      list(filter = list(
        list(var = "ANL01FL", value = "Y"),
        list(var = "ANRIND", value = c("L", "H"), 
          postFct = any, varsBy = "PARAM")
      ))
    )
  )
  
  expect_equal(
    object = dataProcessed,
    expected = subset(data, (PARAM == "A") | (PARAM == "B" & ANL01FL == "Y")),
    check.attributes = FALSE # no message
  )
  
})

test_that("Filter steps are run sequentially when specified in separate filter steps", {
  
  data <- data.frame(
    PARAM = c("A", "B", "B"),
    ANL01FL = c("Y", "Y", "N"),
    ANRIND = c("L", "N", "H"), 
    stringsAsFactors = FALSE
  )
  
  expect_warning(
    dataProcessed <- processData(
      data = data,
      processing = list(
        list(filter = list(var = "ANL01FL", value = "Y")),
        list(filter = list(var = "ANRIND", value = c("L", "H"), 
          postFct = any, varsBy = "PARAM"))
      )
    ),
    "No data is retained"
  )
  
  expect_equal(
    object = dataProcessed,
    expected = subset(data, PARAM == "A"),
    check.attributes = FALSE # no message
  )
  
})

