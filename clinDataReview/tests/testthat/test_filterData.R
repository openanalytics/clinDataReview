context("Filter data")

test_that("Data is correctly filtered based on a inclusion criteria", {
     
	data <- data.frame(
		USUBJID = seq.int(5),
		SEX = c("F", "F", "M", "M", "M")
	)	
	expect_message(
		dataFiltered <- filterData(
			data = data, 
			filters = list(var = "SEX", value = "M"),
			verbose = TRUE
		),
		"2 records.*are filtered"
	)
	expect_equal(
		object = dataFiltered, 
		expected = subset(data, SEX == "M"),
		check.attributes = FALSE # no message
	)
      
})

test_that("Data is correctly filtered based on a non inclusion criteria", {
			
	data <- data.frame(
		USUBJID = seq.int(5),
		SEX = c("F", "F", "M", "M", "M")
	)
	expect_message(
		dataFiltered <- filterData(
			data = data, 
			filters = list(var = "SEX", value = "M", rev = TRUE),
			verbose = TRUE
		),
		"3 records.*are filtered"
	)
	expect_equal(
		object = dataFiltered, 
		expected = subset(data, SEX != "M"),
		check.attributes = FALSE # no message
	)
      
})

test_that("Data is correctly filtered based on a specified operator", {

	data <- data.frame(
		USUBJID = seq.int(5),
		AGE = c(25, 78, 34, 51, 67)
	)
	expect_message(
		dataFiltered <- filterData(
			data = data, 
			filters = list(var = "AGE", value = 50, op = "<="), 
			verbose = TRUE
		),
		"3 records.*are filtered"
	)
	expect_equal(
		object = dataFiltered, 
		expected = subset(data, AGE <= 50),
		check.attributes = FALSE # no message
	)
      
})

test_that("Missing values are included in the filtered dataset by default", {
      
	data <- data.frame(
		USUBJID = seq.int(5),
		AGE = c(NA_real_, NA_real_, 34, 51, 67)
	)
	expect_message(
		dataFiltered <- filterData(
	      	data = data, 
			filters = list(var = "AGE", value = 50, op = "<="), 
			verbose = TRUE
		),
		"2 records.*are filtered"
	)
	expect_equal(
		object = dataFiltered, 
		expected = subset(data, AGE <= 50 | is.na(AGE)),
		check.attributes = FALSE # no message
	)
      
})

test_that("Missing values are included in the filtered dataset when requested", {
			
	data <- data.frame(
		USUBJID = seq.int(5),
		AGE = c(NA_real_, NA_real_, 34, 51, 67)
	)
	expect_message(
		dataFiltered <- filterData(
			data = data, 
			filters = list(var = "AGE", value = 50, op = "<="),
			keepNA = TRUE,
			verbose = TRUE
		),
		"2 records.*are filtered"
	)
	expect_equal(
		object = dataFiltered, 
		expected = subset(data, AGE <= 50 | is.na(AGE)),
		check.attributes = FALSE # no message
	)
			
})

test_that("Missing values are removed in the filtered dataset when specified", {
      
	data <- data.frame(
		USUBJID = seq.int(5),
		AGE = c(NA_real_, NA_real_, 34, 51, 67)
	)
	expect_message(
		dataFiltered <- filterData(
			data = data, 
			filters = list(var = "AGE", value = 50, op = "<="), 
			verbose = TRUE,
			keepNA = FALSE
		),
		"4 records.*are filtered"
	)
	expect_equal(
		object = dataFiltered, 
		expected = subset(data, AGE <= 50 & !is.na(AGE)),
		check.attributes = FALSE # no message
	)
	
})

test_that("Data is correctly filtered based on multiple conditions with and without a specified operator", {

	data <- data.frame(
		USUBJID = seq.int(5),
		SEX = c("F", "F", "M", "M", "M"),
		AGE = c(54, 78, 34, 51, 67),
		ARMCD = c("SCRNFAIL", "PLACEBO", "TRT", "TRT", "SCRNFAIL")
	)	
	filters <- list(
		list(var = "AGE", value = 50, op = "<="),
		"|",
		list(var = "SEX", value = "M"),
		list(var = "ARMCD", value = "SCRNFAIL")
	)
	expect_message(
		dataFiltered <- filterData(
			data = data, 
			filters = filters, 
			verbose = TRUE
		),
		"4 records.*are filtered"
	)
	expect_equal(
		object = dataFiltered, 
		expected = subset(data, (AGE <= 50 | SEX == "M") & ARMCD == "SCRNFAIL"),
		check.attributes = FALSE # no message
	)
      
})

test_that("An error is generated if the value of interest is not specified", {
			
	data <- data.frame(
		USUBJID = seq.int(5),
		SEX = c(NA_character_, NA_character_, "M", "M", "M")
	)
	expect_error(
		filterData(data = data, filters = list(var = "SEX")), 
		regexp = "'value' of interest or 'valueFct' to obtain it, or filtering of missing values should be specified"
	)
	
})

test_that("Missing values are removed in the filtered dataset for a variable if specified", {
      
	data <- data.frame(
		USUBJID = seq.int(5),
		SEX = c(NA_character_, NA_character_, "M", "M", "M")
	)
	expect_message(
		dataFiltered <- filterData(
			data = data, 
			filters = list(var = "SEX", keepNA = FALSE),
			verbose = TRUE
		),
		"2 records.*are filtered"
	)
	expect_equal(
		object = dataFiltered, 
		expected = subset(data, !is.na(SEX)),
		check.attributes = FALSE # no message
	)
	
})

test_that("Missing values are included in the filtered dataset for a variable if specified", {
			
	data <- data.frame(
		USUBJID = seq.int(5),
		SEX = c(NA_character_, NA_character_, "M", "M", "M")
	)
	expect_message(
		dataFiltered <- filterData(
			data = data, 
			filters = list(
				var = "SEX", 
				value = NA_character_, 
				keepNA = FALSE, 
				rev = TRUE
			),
			verbose = TRUE
		),
		"2 records.*are filtered"
	)
	expect_equal(
		object = dataFiltered, 
		expected = subset(data, !is.na(SEX)),
		check.attributes = FALSE # no message
	)
			
})

test_that("Data is correctly filtered for a single inclusion criteria", {
      
	data <- data.frame(
		A = c(1, 2, 3),
		B = c(4, 5, 6),
		C = c("a", "a", "b"),
		stringsAsFactors = FALSE      
	)
      
	dataFiltered <- clinDataReview::filterData(
		data = data,
		filters = list(
			var = "C", value = "a"
		)
	)
	expect_equal(
		object = dataFiltered,
		expected = subset(data, C == "a"),
		check.attributes = FALSE # no message
	)      
	attrData <- attributes(dataFiltered)
	expect_true("msg" %in% names(attrData))
	expect_false("labelVars" %in% names(attrData))
      
})

test_that("An error is generated if no filtering variable is specified", {
      
	expect_error(
		filterData(
			data = data.frame(),
			filters = list(value = "a")
		),
		"'var' used for filtering of the data should be specified."
	)
	  
})

test_that("An additional variable, specifying which values match the filter condition is correctly included", {
      
	data <- data.frame(
		A = c(1, 2, 3),
		B = c(4, 5, 6),
		C = c("a", "a", "b"),
		stringsAsFactors = FALSE      
	)
      
	dataFiltered <- filterData(
		data = data,
		filters = list(var = "C", value = "a"),
		returnAll = TRUE
	)
	
	expect_equal(
		object = dataFiltered,
		expected = data.frame(
			A = c(1, 2, 3),
			B = c(4, 5, 6),
			C = c("a", "a", "b"),
			keep = c(TRUE, TRUE, FALSE),
			stringsAsFactors = FALSE      
		),
		check.attributes = FALSE # no message
	)
      
})

test_that("A warning is generated when a variable is specified, but not present in the data", {
      
	data <- data.frame(
		A = c(1, 2, 3),
		stringsAsFactors = FALSE      
	)
	expect_warning(
		dataFiltered <- filterData(
			data = data,
			filters = list(var = "D", value = "a"),
			keepNA = TRUE
		),
		"Data is not filtered based on the variable: .* is not available in the data."
	)
	expect_equal(
		object = dataFiltered, 
		expected = data,
		check.attributes = FALSE # no message
	)

})

test_that("Data is correctly filtered based on a grouping variable", {

	# example of selection of worst-case records (for one subject)
	data <- data.frame(
		USUBJID = c("1", "1", "1", "1"),
		AEDECOD = c("a", "a", "b", "b"),
		AESEV = c("Mild", "Moderate", "Moderate", "Severe"),
		AESEVN = c(1, 2, 2, 3) 
	)
	
	dataFiltered <- filterData(
		data = data,
		filters = list(
			var = "AESEVN",		
			valueFct = function(x) x[which.max(x)],
			varsBy = "AEDECOD"
		)
	)
	expect_equal(
		object = dataFiltered,
		expected = subset(data, 
			(AEDECOD == "a" & AESEVN == 2) |
			(AEDECOD == "b" & AESEVN == 3)
		),
		check.attributes = FALSE # no message
	)
	
})

test_that("Data is correctly filtered based on multiple filters and one filter with a grouping variable", {
  
  # Note: this test is set up such as the categories of the grouping variable are
  # not ordered alphabetically in the input data
  data <- data.frame(
    AEDECOD = c("b", "b", "a", "a", "c", "c"),
    AESEV = c("Mild", "Moderate", "Moderate", "Severe", "Mild", "Severe")
  )
  
  expect_warning(
    dataFiltered <- filterData(
      data = data,
      filters = list(
        list(var = "AEDECOD", value = c("b", "c")),
        list(
          var = "AESEV",		
          value = "Severe",
          varsBy = "AEDECOD",
          postFct = any
        )
      )
    )
  )
  expect_equal(
    object = dataFiltered,
    expected = subset(data, AEDECOD == "c"),
    check.attributes = FALSE
  )
  
})

test_that("A new variable tracking if a record fulfills the filtering condition is correctly created when requested", {
      
	data <- data.frame(
		A = c(1, 2, 3),
		B = c(4, 5, 6),
		C = c("a", "a", "b"),
		stringsAsFactors = FALSE      
	)
	dataFiltered <- filterData(
		data = data,
		filters = list(var = "C", value = "a", varNew = "newVar"),
	)
	expect_equal(
		object = dataFiltered,
		expected = cbind.data.frame(
			subset(data, C == "a"),
			newVar = c(TRUE, TRUE)
		),
		check.attributes = FALSE # no message
	)
	
})

test_that("A warning is generated if a new variable is created with the same name as a variable in the data", {
			
	data <- data.frame(
		B = c("1", "2", "3"),
		C = c("a", "a", "b"),
		stringsAsFactors = FALSE      
	)
	expect_warning(
		clinDataReview::filterData(
			data = data,
			filters = list(var = "C", value = "a", varNew = "B"),
		),
		".* is overwritten in the data."
	)
	  
})

test_that("Data is correctly filtered based on a specified function", {
      
	data <- data.frame(
		A = c(1, 2, 3, 3),
		B = c(4, 5, 6, 1),
		C = c("a", "a", "b", "b"),
		stringsAsFactors = FALSE      
	)
	dataFiltered <- filterData(
		data = data,
		filters = list(var = "A", valueFct = max)
	)
	expect_equal(
		object = dataFiltered, 
		expected = subset(data, A == max(A)), 
		check.attributes = FALSE
	)
	
})

test_that("Data is correctly filtered based on a function specified as a character", {
			
	data <- data.frame(
		A = c(1, 2, 3, 3),
		B = c(4, 5, 6, 1),
		C = c("a", "a", "b", "b"),
		stringsAsFactors = FALSE      
	)
	dataFiltered <- filterData(
		data = data,
		filters = list(var = "A", valueFct = "function(x) max(x)")
	)
	expect_equal(
		object = dataFiltered, 
		expected = subset(data, A == max(A)), 
		check.attributes = FALSE
	)
			
})

test_that("An error is generated if the filtering function is not correctly specified", {
			
	data <- data.frame(
		C = c("a", "a", "b", "b"),
		stringsAsFactors = FALSE      
	)
	expect_error(
		filterData(
              data = data,
              filters = list(var = "C", valueFct = grepl("a", data$C))
		),
		"'valueFct' should be a character or a function."
	)
	
})

test_that("Data is correctly filtered based on a post-processing function", {
  
  data <- data.frame(
    AEDECOD = c("a", "b", "c"),
    AESEV = c("Mild", "Moderate", "Severe")
  )
  
  expect_message(
    expect_warning(
      dataFiltered <- filterData(
        data = data,
        filters = list(
          var = "AESEV",		
          value = "Severe",
          postFct = all
        ),
        verbose = TRUE
      )
    ),
    "3 records with all of AESEV .+not %in% .+Severe.+ are filtered"
  )
  expect_equal(
    object = dataFiltered,
    expected = data[0, ], 
    check.attributes = FALSE
  )
  
})

test_that("Data is correctly filtered based on a post-processing function specified as a character", {
  
  data <- data.frame(
    AEDECOD = c("a", "b", "c"),
    AESEV = c("Mild", "Moderate", "Severe")
  )
  
  expect_message(
    expect_warning(
      dataFiltered <- filterData(
        data = data,
        filters = list(
          var = "AESEV",		
          value = "Severe",
          postFct = "all"
        ),
        verbose = TRUE
      )
    ),
    "3 records with all of AESEV .+not %in% .+Severe.+ are filtered"
  )
  expect_equal(
    object = dataFiltered,
    expected = data[0, ], 
    check.attributes = FALSE
  )
  
})

test_that("Data is correctly filtered based on a post-processing function and grouping variables", {
  
  data <- data.frame(
    USUBJID = c("1", "1", "2", "1", "2", "2"),
    PARAM = c("a", "a", "a", "b", "b", "b"),
    ANRIND = c("Abnormal", "Normal", "Normal", "Normal", "Abnormal", "Normal")
  )
  
  expect_message(
    expect_warning(
      dataFiltered <- filterData(
        data = data,
        filters = list(
          var = "ANRIND",		
          value = "Abnormal",
          varsBy = c("PARAM", "USUBJID"),
          postFct = any
        ),
        verbose = TRUE
      )
    ),
    "Records with any of ANRIND.+not %in% .+Abnormal.+ by PARAM.+, USUBJID.+are filtered"
  )
  expect_equal(
    object = dataFiltered,
    expected = subset(data, (PARAM == "a" & USUBJID == "1") | (PARAM == "b" & USUBJID == "2")), 
    check.attributes = FALSE
  )
  
})
