context("Test miscellaneous functions")

library(DT)

test_that("Paths and links to patient profiles are successfully created", {
			
	data <- data.frame(USUBJID = c("subj1", "subj2", "subj3"))	
	patientProfilePath <- tempfile(pattern = "patientProfiles")

	dataUpdated <- createPatientProfileVar(
		data = data,
		patientProfilePath = patientProfilePath, 
		checkExist = FALSE
	)
	
	# Patient profile columns properly created
	patientProfileVars <- c("patientProfileLink", "patientProfilePath")
	expect_true(all(patientProfileVars %in% colnames(dataUpdated)))
	expect_true(all(sapply(dataUpdated[, patientProfileVars], inherits, "character")))
	
})

test_that("An error is generated if the path to the patient profiles does not exist", {
			
	data <- data.frame(USUBJID = c("subj1", "subj2", "subj3"))		
	
	expect_error(
		createPatientProfileVar(
			data = data,
			patientProfilePath = "unExistingFolder"
		),
		regex  = "*not found"
	)
	
})

test_that("A warning is generated if the subject variable to extract the patient profile paths and links is not available in the data", {
			
	data <- data.frame(USUBJID = c("subj1", "subj2", "subj3"))	

	patientProfilePath <- tempfile(pattern = "patientProfiles")
	dir.create(patientProfilePath)
	expect_warning(
		createPatientProfileVar(
				data = data,
				patientProfilePath = patientProfilePath,
				subjectVar = "usubjid"
		),
		regex = "patient profile variable is not created"
	)
	
})


test_that("Hover text is correctly formatted", {
      
	x <- c("A", "B", "C", "D")
	myLabel <- "myLabel"
	formatText <- formatHoverText(x = x, label = myLabel)
	myLabelFormat <- sprintf("%s: %s", myLabel, paste(x, collapse = ", "))
	expect_identical(
		object = formatText, 
		expected = setNames(myLabelFormat, myLabelFormat)
	)
	
})

test_that("Hover text is correctly formatted with a specified width", {
			
	x <- c("A", "B", "C", "D")
	myLabel <- "myLabel"
	formatTextShort <- formatHoverText(x = x, label = myLabel, width = 3)      
	textFormat <- sprintf("%s:<br>%s", myLabel, paste(x, collapse = ",<br>"))
	names(textFormat) <- sprintf("%s: %s", myLabel, paste(x, collapse = ", "))
	expect_identical(object = formatTextShort, expected = textFormat)
      
})

test_that("One variable is correctly converted to a formula", {
      
	varFormula <- varToFm("CHG")
	expect_s3_class(varFormula, "formula")
	expect_equal(
		object = varFormula, 
		expected = as.formula("~CHG"), 
		check.attributes = FALSE
	)
	  
})

test_that("Multiple variables are correctly combined when converted to a formula", {

	varsFormula <- varToFm(c("AVAL", "CHG"))
	expect_s3_class(varsFormula, "formula")
	expect_equal(
		object = as.formula("~AVAL + CHG"), 
		expected = varsFormula, 
		check.attributes = FALSE
	)
      
})

test_that("A variable with non syntactically valid name is correctly converted to a formula", {
			
	varFormula <- varToFm("%m")
	expect_s3_class(varFormula, "formula")
	expect_equal(
		object = as.formula("~`%m`"), 
		expected = varFormula, 
		check.attributes = FALSE
	)
			
})

test_that("All JavaScript dependencies for the clinical data review report are correctly extracted", {
      
	dependencies <- getJsDepClinDataReview()
	expect_type(dependencies, "list")
	expect_length(dependencies, 5)
	matrixRes <- sapply(dependencies, expect_length, 10)
	
	for(iDep in seq_along(dependencies))
      expect_s3_class(dependencies[[!!iDep]], "html_dependency")
  
})

test_that("The Javascript dependency for collapsible button in the clinical data review report is correctly extracted", {
			
	dependency <- getJsDepClinDataReview(type = "collapsibleButton")
	expect_type(dependency, "list")
	expect_length(dependency, n = 1)
	expect_s3_class(dependency[[1]], "html_dependency")
	expect_equal(object = dependency[[1]]$name, expected = "collapsibleButton")
	
})

test_that("The Javascript dependency for patient profiles in the clinical data review report is correctly extracted", {
			
	dependency <- getJsDepClinDataReview("patientProfiles")
	expect_type(dependency, "list")
	expect_length(dependency, n = 4)
	for(iDep in seq_along(dependency))
		expect_s3_class(dependency[[!!iDep]], "html_dependency")
	expect_setequal(
		object = sapply(dependency, `[[`, "name"),
		expected = c("FileSaver", "jszip", "jszip-utils", "PatientProfiles")
	)
			
})

test_that("HTML content is correctly collapsed", {
      
	x <- matrix(LETTERS[1 : 10])
	button <- collapseHtmlContent(input = DT::datatable(x))
	expect_s3_class(button, "shiny.tag.list")
	
	names <- sapply(button, function(x) x$name)
	names(names) <- NULL
	expect_identical(
		object = unlist(names),
		c("input", "div", "br", "br")
	)
      
	outputConsole <- capture.output(button)
	expect_type(outputConsole, "character")
	expect_true(any(grepl("datatables", outputConsole)))
      
})