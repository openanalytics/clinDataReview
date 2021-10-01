context("Test reporting template functions")

library(yaml)
library(jsonlite)

test_that("A config file with all required parameters is checked successfully", {
      
	# R CMD check runs on package binary, so without 'inst' folder:
	refConfig <- system.file(package = "clinDataReview", "template", "divisionTemplate.json")
      
	configFileDivision <- tempfile(pattern = "configDivision", fileext = ".yml")
	write_yaml(
		list(
			template = "divisionTemplate.Rmd",
			templatePackage = "clinDataReview",
			reportTitle = "Study name"
		),
		configFileDivision
	)
	expect_silent(
		checkConfigFile(configFileDivision, configSpecFile = refConfig)
	)
      
})

test_that("An error is generated if a config file with missing parameters is checked", {
			
	## Division config file
	configFileDivision <- tempfile(pattern = "configDivision", fileext = ".yml")
	write_yaml(
		list(
			reportTitle = "Study name"
		),
		configFileDivision
	)
			
	# R CMD check runs on package binary, so without 'inst' folder:
	refConfig <- system.file(package = "clinDataReview", "template", "divisionTemplate.json")
			
	expect_error(
		checkConfigFile(configFileDivision, configSpecFile = refConfig)
	)
	
})

test_that("The path to a template report is correctly extracted from the installed package", {
      
	path <- getPathTemplate(file = "divisionTemplate.Rmd")
	expect_type(path, "character")
	expect_true(file.exists(path))
	
})

test_that("A warning is generated if a path to a template report, not available in the package, is requested", {

	expect_warning(
		pathEmpty <- getPathTemplate(file = "divisionTempl.Rmd"),
		"not available"
	)
	expect_identical(pathEmpty, "")
      
})

test_that("Documentation of template reports is created even if no reports are available", {
      
	# R CMD check runs on package binary, so without 'inst' folder:
	doc <- clinDataReview:::createTemplateDoc()
	expect_type(doc, "character")
      
	docRoxParType <- paste0(
		"\\section{Parameter type}{Please note that the type mentioned below ",
		"corresponds to the type in the config file (in YAML/JSON format).",
		"The mapping to R data type is as followed:",
		"\\itemize{",
		"\\item{string: }{character vector of length 1}",
		"\\item{integer: }{integer vector of length 1}",
		"\\item{array: }{vector/list without names}",
		"\\item{object: }{list with names}",
		"}}"
	)
	expect_identical(docRoxParType, doc[1])
      
})

test_that("Documentation for template reports is not created if template folder doesn't exist", {
      
	expect_silent(
		res <- clinDataReview:::createTemplateDoc(system.file("inst", "template2", package = "clinDataReview"))
	)
	expect_type(res, "character")
	expect_identical(res, "")
      
})

test_that("Parameter documentation is correctly converted from JSON schema to R documentation", {
			
	# R CMD check runs on package binary, so without 'inst' folder:
	jsonFileName <- tempfile(pattern = "template", fileext = ".json")
	jsonCnt <- c(
		"{",
		'  "title" : "test title",',
		'  "description": "this is example of json file for unit tests",',
		'  "properties": {',
		'    "param1": {',
		'      "type": "array",',
		'      "doc": "first parameter"',
		'    }',
		'  }',
		'}'
	)
	cat(jsonCnt, file = jsonFileName, sep = "\n")    
	templateSpec <- jsonlite::fromJSON(jsonFileName)
      
	jsonSchemaDoc <- clinDataReview:::JSONSchToRd(JSONSch = templateSpec, title = "templateName")
	expect_type(jsonSchemaDoc, "character")
	# title is parsed
	expect_match(jsonSchemaDoc, regexp = "\\section{test title templateName}", fixed = TRUE, all = FALSE)
	# description is parsed
	expect_match(jsonSchemaDoc, regexp = "this is example of json file for unit tests", all = FALSE)
	# parameter doc is parsed
	expect_match(jsonSchemaDoc, 
		regexp = ".*parameters are available.*param1.*array.*first parameter", 
		all = FALSE
	)
      
})

test_that("Documentation for template reports contains parameter description from JSON schema file", {
			
	# create new empty tmp folder
	tmpFolder <- tempfile("test")
	dir.create(tmpFolder)
			
	templateName <- file.path(tmpFolder, "template.Rmd")
	file.create(templateName)
	
	jsonFileName <- file.path(tmpFolder, "template.json")
	jsonCnt <- c(
		"{",
		'  "title" : "test title",',
		'  "description": "this is example of json file for unit tests",',
		'  "properties": {',
		'    "param1": {',
		'      "type": "array",',
		'      "doc": "first parameter"',
		'    }',
		'  }',
		'}'
	)
	cat(jsonCnt, file = jsonFileName, sep = "\n")
			
	expect_silent(
		res <- clinDataReview:::createTemplateDoc(templatePath = tmpFolder)
	)
	expect_type(res, "character")
	
	# description is parsed
	expect_match(res, regexp = "this is example of json file for unit tests", all = FALSE)
	# parameter doc is parsed
	expect_match(res, 
		regexp = ".*parameters are available.*param1.*array.*first parameter", 
		all = FALSE
	)
	
})

test_that("Documentation for template reports is created even if no JSON schema parameter file is available", {
      
	# create new empty tmp folder
	tmpFolder <- tempfile("test")
	dir.create(tmpFolder)
      
	templateName <- file.path(tmpFolder, "template.Rmd")
	file.create(templateName)
      
	expect_silent(
		res <- clinDataReview:::createTemplateDoc(templatePath = tmpFolder)
	)
	expect_type(res, "character")
	expect_match(res, regexp = "\\section{template}", fixed = TRUE, all = FALSE)
      
})
