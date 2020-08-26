context("'Knit print' medical monitoring outputs")

library(knitr)
library(rmarkdown)
library(xml2)
library(plyr)
library(tools)

# example dataset
library(glpgUtilityFct)
data(SDTMDataPelican)
data <- subset(SDTMDataPelican$LB, LBTESTCD == "ALT")
data$VISIT <- with(data, reorder(VISIT, VISITNUM))
data$LBTPT <- with(data, reorder(LBTPT, LBTPTNUM))

fileInput <- "knitPrintMedicalMonitoring-input.RData"
fileRmd <- "knitPrintMedicalMonitoring.Rmd"

# utility function to test inclusion of objects in a Rmd report
includeCodeInRmdDoc <- function(code, fileRmd, fileInput){
	unlink(fileRmd)
	# create a Rmd document loading specified data and using specified code to include objects
	cat(
		"---",
		"title: test inclusion medical monitoring output",
		"output: rmarkdown::html_document",
		"---",
		'```{r results = "asis", echo = FALSE, warning = FALSE, message = FALSE}',
		"library(medicalMonitoring)",
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


test_that("Inclusion of direct med mon output in report", {
	
	medMonRes <- scatterplotMonitoring(
		data = data, 
		xVar = "LBDY", yVar = "LBSTRESN",
		table = TRUE
	)
	
	code <- c("knitPrintMedicalMonitoring(list = medMonRes)")
	saveRDS(medMonRes, file = fileInput)
	htmlReport <- includeCodeInRmdDoc(code, fileRmd, fileInput)
	
	# check that output contains a datatables and a plotly object
	expect_length(xml_find_all(htmlReport, '//body//div[contains(@class, "plotly")]'), 1)
	expect_length(xml_find_all(htmlReport, '//body//div[contains(@class, "datatables")]'), 1)
			
})

test_that("Inclusion of a named list of level 1 in report", {

	medMonRes <- dlply(data, c("VISIT", "LBTPT"), function(dataI)
		scatterplotMonitoring(
			data = dataI, 
			xVar = "LBDY", yVar = "LBSTRESN",
			table = TRUE
		)
	)
			
	code <- c("knitPrintMedicalMonitoring(list = medMonRes, level = 2)")
	saveRDS(medMonRes, file = fileInput)
	htmlReport <- includeCodeInRmdDoc(code, fileRmd, fileInput)

	# check that output contains some datatables and plotly objects
	groups <- unique(data[, c("VISIT", "LBTPT")])
	nEl <- nrow(groups)
	expect_length(xml_find_all(htmlReport, '//body//div[contains(@class, "plotly")]'), nEl)
	expect_length(xml_find_all(htmlReport, '//body//div[contains(@class, "datatables")]'), nEl)
	
	# check order and if all sections are present
	groups <- groups[with(groups, order(VISIT, LBTPT)), ]
	sections <- dlply(groups, "VISIT", function(x) unique(c(as.character(x$VISIT), as.character(x$LBTPT))))
	sections <- unname(unlist(sections))
	htmlHeaders <- xml_find_all(htmlReport, '//body//*[self::h2 or self::h3]')
	expect_identical(xml_text(htmlHeaders), sections)
	
})

test_that("Inclusion of a nested list in the report", {
			
	medMonRes <- dlply(data, "VISIT", function(dataI)
		scatterplotMonitoring(
			data = dataI, 
			xVar = "LBDY", yVar = "LBSTRESN",
			table = TRUE
		)
	)
	medMonResNested <- list(B = medMonRes, A = medMonRes)
			
	code <- c("knitPrintMedicalMonitoring(list = medMonRes, level = 2)")
	saveRDS(medMonResNested, file = fileInput)
	htmlReport <- includeCodeInRmdDoc(code, fileRmd, fileInput)
	
	# check that output contains some datatables and plotly objects
	nEl <- sum(sapply(medMonRes, length))
	expect_length(xml_find_all(htmlReport, '//body//div[contains(@class, "plotly")]'), nEl)
	expect_length(xml_find_all(htmlReport, '//body//div[contains(@class, "datatables")]'), nEl)
	
	# check order and if all sections are present
	htmlH2 <- xml_find_all(htmlReport, '//body//h2')
	expect_identical(xml_text(htmlH2), names(medMonResNested))
	htmlH3 <- xml_find_all(htmlReport, '//body//h3')
	expect_identical(unique(xml_text(htmlH3)), names(medMonRes))
			
})

test_that("Inclusion of list with missing names", {
			
	medMonRes <- dlply(data, NULL, function(dataI)
		scatterplotMonitoring(
			data = dataI, 
			xVar = "LBDY", yVar = "LBSTRESN",
			table = TRUE
		)
	)
	code <- c("knitPrintMedicalMonitoring(list = medMonRes, level = 3)")
	saveRDS(medMonRes, file = fileInput)
	htmlReport <- includeCodeInRmdDoc(code, fileRmd, fileInput)
	
	# check that output contains some datatables and plotly objects
	expect_length(xml_find_all(htmlReport, '//body//div[contains(@class, "plotly")]'), 1)
	expect_length(xml_find_all(htmlReport, '//body//div[contains(@class, "datatables")]'), 1)
	
	htmlH3 <- xml_find_all(htmlReport, '//body//h3')
	expect_length(xml_text(htmlH3), 0)
			
})

test_that("Custom separator", {
			
	medMonRes <- dlply(data, c("VISIT", "LBTPT"), function(dataI)
		scatterplotMonitoring(
			data = dataI, 
			xVar = "LBDY", yVar = "LBSTRESN",
			table = TRUE
		)
	)
	names(medMonRes) <- sub(".", "[SEP]", names(medMonRes), fixed = TRUE)
	h2 <- unique(sub("(.+)\\[SEP\\].*", "\\1", names(medMonRes)))
			
	code <- c("knitPrintMedicalMonitoring(list = medMonRes, level = 2, sep = '[SEP]')")
	saveRDS(medMonRes, file = fileInput)
	htmlReport <- includeCodeInRmdDoc(code, fileRmd, fileInput)
	
	htmlH2 <- xml_find_all(htmlReport, '//body//h2')
	expect_identical(xml_text(htmlH2), h2)
			
})

test_that("General label", {
		
	knitr::opts_knit$set("rmarkdown.pandoc.to" = "pdf")
			
	pl <- plot_ly(data = iris, x = ~`Sepal.Length`, y = ~`Petal.Length`, 
			type = "scatter", mode = "markers")
	listPlots <- list(A = pl, B = pl)
	label <- "testLabel"
	expect_error(
		res <- capture.output(
			knitPrintMedicalMonitoring(list = listPlots, generalLabel = label)
		),
		NA
	)
	expect_length(grep(label, res), length(listPlots))
			
})

test_that("Wrong input", {
	
	expect_error(
		knitPrintMedicalMonitoring(list = list(data.frame())), 
		pattern = "named"
	)
	
	expect_error(
		knitPrintMedicalMonitoring(list = list(A = data.frame())), 
		pattern = "should be among the classes"
	)	
	
	expect_error(
		knitPrintMedicalMonitoring(list = list(`A.1` = plot_ly(), B = plot_ly())), 
		pattern = "extraction of labels"
	)
		
})