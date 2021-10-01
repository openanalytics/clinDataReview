context("Knit print clinical data outputs in a report")

library(rmarkdown)
library(xml2)
library(plyr)

# utility function to test inclusion of objects in a Rmd report
includeCodeInRmdDoc <- function(code, fileRmd, fileInput){
  unlink(fileRmd)
  # create a Rmd document loading specified data and using specified code to include objects
  cat(
      "---",
      "title: test inclusion clinical data output",
      "output: rmarkdown::html_document",
      "---",
      '```{r results = "asis", echo = FALSE, warning = FALSE, message = FALSE}',
      "library(clinDataReview)",
      paste0("medMonRes <- readRDS(", shQuote(fileInput), ")"),
      code,
      "```",
      file = fileRmd, 
      sep = "\n"
  )
  # run the report, should run without errors
  expect_error(outputRmd <- rmarkdown::render(fileRmd, quiet = TRUE), NA)
  # import created html file
  htmlReport <- xml2::read_html(outputRmd)
  unlink(c(fileRmd, fileInput, outputRmd))
  return(htmlReport)
}


test_that("A clinical data review output is correctly included in a report", {
			
	skip_on_cran()
      
	data <- data.frame(
		USUBJID = c(1, 1, 2, 2),
		LBDY = c(NA, -19, 1, 53),
		LBSTRESN = c(10, 30, 29, 40)
	)
	
	medMonRes <- scatterplotClinData(
		data = data, 
		xVar = "LBDY", yVar = "LBSTRESN",
		table = TRUE
	)
      
	code <- c("knitPrintClinDataReview(list = medMonRes)")
	  
	fileInput <- tempfile(
		pattern = "knitPrintClinDataReview-input", 
		fileext = ".RData"
	)
	saveRDS(medMonRes, file = fileInput)
	  
	fileRmd <- tempfile(
		pattern = "knitPrintClinDataReview", 
		fileext = ".Rmd"
	)
	htmlReport <- includeCodeInRmdDoc(code, fileRmd, fileInput)
      
	# check that output contains a datatables and a plotly object
	expect_length(
		object = xml2::xml_find_all(htmlReport, '//body//div[contains(@class, "plotly")]'), 
		n = 1
	)
	expect_length(
		object = xml2::xml_find_all(htmlReport, '//body//div[contains(@class, "datatables")]'), 
		n = 1
	)
})

test_that("A named list of clinical data review outputs is correctly included in a report", {
			
	skip_on_cran()
      
	data <- data.frame(
		USUBJID = c(1, 1, 2, 2),
		VISIT = c("Week 1", "Day 1", "Week 1", "Day 1"),
		LBTPT = c("PREDOSE", "", "PREDOSE", ""),
		LBDY = c(NA, -19, 1, 53),
		LBSTRESN = c(10, 30, 29, 40)
	)
	medMonRes <- dlply(data, c("VISIT", "LBTPT"), function(dataI)
		scatterplotClinData(
			data = dataI, 
			xVar = "LBDY", yVar = "LBSTRESN",
			table = TRUE
		)
	)
      
	code <- c("knitPrintClinDataReview(list = medMonRes, level = 2)")
	fileInput <- tempfile(
		pattern = "knitPrintClinDataReview-input", 
		fileext = ".RData"
	)
	saveRDS(medMonRes, file = fileInput)
	  
	fileRmd <- tempfile(
		pattern = "knitPrintClinDataReview", 
		fileext = ".Rmd"
	)
 	htmlReport <- includeCodeInRmdDoc(code, fileRmd, fileInput)
      
	# check that output contains some datatables and plotly objects
	groups <- unique(data[, c("VISIT", "LBTPT")])
	expect_length(
		object = xml2::xml_find_all(htmlReport, '//body//div[contains(@class, "plotly")]'), 
		n = nrow(groups)
	)
	expect_length(
		object = xml2::xml_find_all(htmlReport, '//body//div[contains(@class, "datatables")]'), 
		n = nrow(groups)
	)
      
	# check order and if all sections are present
	sections <- as.vector(t(attr(medMonRes, "split_labels")))
	htmlHeaders <- xml2::xml_find_all(htmlReport, '//body//*[self::h2 or self::h3]')
	expect_identical(
		object = xml2::xml_text(htmlHeaders), 
		expected = sections
	)
      
})

test_that("A nested list of clinical data review outputs is correctly included in a report", {
			
	skip_on_cran()
	
	data <- data.frame(
		USUBJID = c(1, 1, 2, 2),
		VISIT = c("Day 1", "Week 1", "Day 1", "Week 1"),
		LBDY = c(NA, -19, 1, 53),
		LBSTRESN = c(10, 30, 29, 40)
	)
      
	medMonRes <- dlply(data, "VISIT", function(dataI)
		scatterplotClinData(
			data = dataI, 
			xVar = "LBDY", yVar = "LBSTRESN",
			table = TRUE
		)
	)
	medMonResNested <- list(B = medMonRes, A = medMonRes)
      
	code <- c("knitPrintClinDataReview(list = medMonRes, level = 2)")
	  
	fileInput <- tempfile(
		pattern = "knitPrintClinDataReview-input", 
		fileext = ".RData"
	)
	saveRDS(medMonResNested, file = fileInput)
	  
	fileRmd <- tempfile(
		pattern = "knitPrintClinDataReview", 
		fileext = ".Rmd"
	)
	htmlReport <- includeCodeInRmdDoc(code, fileRmd, fileInput)
      
	# check that output contains some datatables and plotly objects
	nEl <- sum(sapply(medMonRes, length))
	expect_length(
		object = xml2::xml_find_all(htmlReport, '//body//div[contains(@class, "plotly")]'), 
		n = nEl
	)
	expect_length(
		object = xml2::xml_find_all(htmlReport, '//body//div[contains(@class, "datatables")]'), 
		n = nEl
	)
      
	# check order and if all sections are present
	htmlH2 <- xml2::xml_find_all(htmlReport, '//body//h2')
	expect_identical(object = xml2::xml_text(htmlH2), expected = names(medMonResNested))
	htmlH3 <- xml2::xml_find_all(htmlReport, '//body//h3')
	expect_identical(object = unique(xml2::xml_text(htmlH3)), expected = names(medMonRes))
      
})

test_that("A list of clinical data review outputs with missing names is correctly included in a report", {
			
	skip_on_cran()
	
	data <- data.frame(
		USUBJID = c(1, 1, 2, 2),
		LBDY = c(NA, -19, 1, 53),
		LBSTRESN = c(10, 30, 29, 40)
	)
      
	medMonRes <- dlply(data, NULL, function(dataI)
		scatterplotClinData(
			data = dataI, 
			xVar = "LBDY", yVar = "LBSTRESN",
			table = TRUE
		)
	)
	code <- c("knitPrintClinDataReview(list = medMonRes, level = 3)")
	  
	fileInput <- tempfile(
		pattern = "knitPrintClinDataReview-input", 
		fileext = ".RData"
	)
	saveRDS(medMonRes, file = fileInput)
	  
	fileRmd <- tempfile(
		pattern = "knitPrintClinDataReview", 
		fileext = ".Rmd"
	)
	htmlReport <- includeCodeInRmdDoc(code, fileRmd, fileInput)
      
	# check that output contains some datatables and plotly objects
	expect_length(
		object = xml2::xml_find_all(htmlReport, '//body//div[contains(@class, "plotly")]'), 
		n = 1
	)
	expect_length(
		object = xml2::xml_find_all(htmlReport, '//body//div[contains(@class, "datatables")]'), 
		n = 1
	)
	
	htmlH3 <- xml2::xml_find_all(htmlReport, '//body//h3')
	expect_length(object = xml2::xml_text(htmlH3), n = 0)
      
})

test_that("A list of clinical data review outputs with names with a custom separator is correctly included in a report", {
			
	skip_on_cran()
      
	data <- data.frame(
		USUBJID = c(1, 1, 2, 2),
		VISIT = c("Day 1", "Week 1", "Day 1", "Week 1"),
		LBTPT = c("", "PREDOSE", "", "PREDOSE"),
		LBDY = c(NA, -19, 1, 53),
		LBSTRESN = c(10, 30, 29, 40)
	)
	
	medMonRes <- dlply(data, c("VISIT", "LBTPT"), function(dataI)
		scatterplotClinData(
			data = dataI, 
			xVar = "LBDY", yVar = "LBSTRESN",
			table = TRUE
		)
	)
	names(medMonRes) <- sub(".", "[SEP]", names(medMonRes), fixed = TRUE)
	h2 <- unique(sub("(.+)\\[SEP\\].*", "\\1", names(medMonRes)))
	
	code <- c("knitPrintClinDataReview(list = medMonRes, level = 2, sep = '[SEP]')")
	fileInput <- tempfile(pattern = "knitPrintClinDataReview-input", fileext = ".RData")
	saveRDS(medMonRes, file = fileInput)
	
	fileRmd <- tempfile(pattern = "knitPrintClinDataReview", fileext = ".Rmd")
	htmlReport <- includeCodeInRmdDoc(code, fileRmd, fileInput)
      
	htmlH2 <- xml2::xml_find_all(htmlReport, '//body//h2')
	expect_identical(object = xml2::xml_text(htmlH2), expected = h2)
      
})

test_that("An error is generated if the input list is not named", {
      
	expect_error(
		knitPrintClinDataReview(list = list(data.frame())), 
		pattern = "named"
	)
	
})

      
test_that("An error is generated if the class of the input object is not supported", {
			
	expect_error(
		knitPrintClinDataReview(list = list(A = data.frame())), 
		pattern = "should be among the classes"
	)
	  
})

test_that("An error is generated if the names of the input list are not correctly formatted", {
      
	expect_error(
		knitPrintClinDataReview(list = list(`A.1` = plot_ly(), B = plot_ly())), 
		pattern = "extraction of labels"
	)
      
})

test_that("A list of clinical data review outputs with empty elements is correctly included in a report", {
			
	skip_on_cran()
	
	data <- data.frame(
		USUBJID = c(1, 1, 2, 2),
		LBDY = c(NA, -19, NA_real_, 53),
		LBSTRESN = c(NA_real_, 30, NA_real_, 40),
		VISIT = c("Day 1", "Week 1", "Day 1", "Week 1")
	)
	expect_warning(
		medMonRes <- dlply(data, "VISIT", function(dataI)
			scatterplotClinData(
				data = dataI, 
				xVar = "LBDY", yVar = "LBSTRESN",
				table = TRUE
			)
		),
		regexp = "filtering of missing values"
	)
	idxEmptyPlot <- which(sapply(medMonRes, is.null))
	expect_length(object = idxEmptyPlot, n = 1)
	
	code <- c("knitPrintClinDataReview(list = medMonRes, level = 2)")
	fileInput <- tempfile(pattern = "knitPrintClinDataReview-input", fileext = ".RData")
	saveRDS(medMonRes, file = fileInput)
	
	fileRmd <- tempfile(pattern = "knitPrintClinDataReview", fileext = ".Rmd")
	htmlReport <- includeCodeInRmdDoc(code, fileRmd, fileInput)
      
	# check that output contains some datatables and plotly objects
	expect_length(
		object = xml2::xml_find_all(htmlReport, '//body//div[contains(@class, "plotly")]'), 
		n = 1
	)
	expect_length(
		object = xml2::xml_find_all(htmlReport, '//body//div[contains(@class, "datatables")]'), 
		n = 1
	)
      
})

test_that("An empty clinical data review output produces an empty output when included in a report", {
			
	emptyList <- list()
	class(emptyList) <- "clinDataReview"
			
	expect_silent(
		res <- knitPrintClinDataReview(list = emptyList)
	)
	expect_null(res)
			
})