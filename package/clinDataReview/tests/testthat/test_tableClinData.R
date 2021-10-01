context("Get clinical data as a table")

# 'tableClinData' is also tested through the other plot functionalities,
# and via the tests for getClinDT in clinUtils
# so other tests are skipped

test_that("A table is successfully created for clinical data", {
      
	data <- data.frame(USUBJID = c("ID1", "ID2", "ID3", "ID4"))
	tableMon <- tableClinData(data = data)
	expect_s3_class(tableMon, "datatables")
      
})

test_that("A warning is generated if the variable for the patient profile path is not available", {
      
	data <- data.frame(USUBJID = c("ID1", "ID2", "ID3", "ID4"))	
	expect_warning(
		tableClinData(
			data = data,
			pathVar = "varName"
		),
		"Variable with path to subject profile: .* is not available"
	)
      
})

test_that("The variable for the patient profile path is successfully included in a clinical data table", {
			
	data <- data.frame(
		USUBJID = c("ID1", "ID2", "ID3", "ID4"),
		path = sprintf("<a href=\"./path-to-report-%d\">label</a>", 1:4),
		stringsAsFactors = FALSE
	)
      
	tableMon <- tableClinData(
		data = data,
		pathVar = "path"
	)
	expect_s3_class(tableMon, "datatables")
           
})

test_that("The variable for the patient profile path is successfully specified as expandable in a clinical data table", {
      
	data <- data.frame(
		USUBJID = c("ID1", "ID2", "ID3", "ID4"),
		path = sprintf("<a href=\"./path-to-report-%d\">label</a>", 1:4),
		stringsAsFactors = FALSE
	)
			
	tableMon <- tableClinData(
		data = data,
		pathVar = "path",
		pathExpand = TRUE
	)
	expect_s3_class(tableMon, "datatables")
	
})