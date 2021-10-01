context("Zip a clinical data review report")

test_that("The redirection page of the HTML report is successfully created", {
      
	tmpRedirect <- tempfile()
	dir.create(tmpRedirect)
	redirectPageFile <- file.path(tmpRedirect, "report.html")
	clinDataReview:::createRedirectPage(
		dir = tmpRedirect,
		redirectPage = redirectPageFile
	)
	expect_true(file.exists(redirectPageFile))
      
	textInHtml <- readLines(redirectPageFile)
	expect_type(textInHtml, "character")
	expect_true(any(grepl("1-introduction.html", textInHtml)))
      
})

test_that("An error is generated if the directory of the report to zip is not correctly specified", {
      
	expect_error(
		zipClinDataReview(reportDir = 1),
		"Input arguments should be characters."
	)
	  
})

test_that("An error is generated if the new directory of the report to zip is not correctly specified", {
      
	expect_error(
		zipClinDataReview(newDir = 1),
		"Input arguments should be characters."
	)
	
})

test_that("An error is generated if the redirect page of the report to zip is not correctly specified", {
			
	expect_error(
		zipClinDataReview(redirectPage = 1),
		"Input arguments should be characters."
	)
	
})

test_that("An error is generated if the zip folder is not correctly specified", {
			
	expect_error(
		zipClinDataReview(zipFolder = 1),
		"Input arguments should be characters."
	)
      
})

test_that("An error is generated if the directory of the report to zip does not exist", {
      
	expect_error(
		zipClinDataReview(reportDir = "folderReport"),
		"Directory specified in 'reportDir' does not exist."
	)
      
})

test_that("An error is generated if the directory of the report to zip is empty", {
      
	emptyDir <- tempfile()
	dir.create(emptyDir)
	newDir <- tempfile("report_dependencies")
	expect_error(
		zipClinDataReview(
			reportDir = emptyDir, 
			newDir = newDir
		),
		"No files available in the 'reportDir'."
	)
      
})

test_that("The zip folder of the report is correctly created", {
      
	tmpZip <- tempfile()
	dir.create(tmpZip)

	reportDir <- tempfile("reportFolder", tmpdir = tmpZip)
	dir.create(reportDir)
      
	medMonFile <- tempfile(pattern = "file", tmpdir = reportDir, fileext = ".html")
	write(x = c('<!DOCTYPE html> </html>'), medMonFile)
      
	newDir <- tempfile("report_dependencies", tmpdir = tmpZip)
	zipFolder <- tempfile("reports", tmpdir = tmpZip, fileext = ".zip")
      
	expect_silent(
		zipClinDataReview(
			reportDir = reportDir,
			newDir = newDir,
			redirectPage = file.path(tmpZip, "report.html"),
			zipFolder = zipFolder       
		)
	)
      
	expect_setequal(
		object = list.files(tmpZip),
		expected = c(
			basename(newDir),
			"report.html",
			basename(reportDir),
			basename(zipFolder)
		)
	)
	expect_setequal(
		object = list.files(newDir),
		expected = basename(medMonFile)
	)
	
})