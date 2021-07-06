context("Test miscellaneous functions")

library(DT)

test_that("createPatientProfileVar", {
			
	data <- data.frame(USUBJID = c("subj1", "subj2", "subj3"))		
	
	expect_error(
		createPatientProfileVar(
			data = data,
			patientProfilePath = "unExistingFolder"
		),
		regex  = "*not found"
	)
	
	patientProfilePath <- tempfile(pattern = "patientProfiles")
	
	expect_silent(
		dataUpdated <- createPatientProfileVar(
			data = data,
			patientProfilePath = patientProfilePath, checkExist = FALSE
		)
	)
	# Patient profile columns properly created
	patientProfileVars <- c("patientProfileLink", "patientProfilePath")
	expect_true(all(patientProfileVars %in% colnames(dataUpdated)))
	expect_true(all(sapply(dataUpdated[, patientProfileVars], inherits, "character")))
	
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

test_that("Formatting of hover text", {
      
      x <- LETTERS
      myLabel <- "myLabel"
      
      formatText <- formatHoverText(x = LETTERS, label = "myLabel")
      myLabelFormat <- sprintf("%s: %s", myLabel, paste(LETTERS, collapse = ", "))
      
      expect_is(formatText, "character")
      expect_identical(names(formatText), myLabelFormat)
      
      # Change default width
      formatTextShort <- formatHoverText(x = LETTERS, label = "myLabel", width = 3)
      expect_is(formatTextShort, "character")
      expect_identical(names(formatText), names(formatTextShort))
      
      textFormat <- sprintf("%s:<br>%s", myLabel, paste(LETTERS, collapse = ",<br>"))
      names(textFormat) <- myLabelFormat
      expect_identical(textFormat, formatTextShort)
      
    })

test_that("One variable is correctly converted to a formula", {
      
	# One variable
	varFormula <- varToFm("CHG")
	expect_s3_class(varFormula, "formula")
	expect_equal(as.formula("~CHG"), varFormula, check.attributes = FALSE)
	  
})

test_that("Multiple variables are correctly combined when converted to a formula", {

	# Two variables
	varsFormula <- varToFm(c("AVAL", "CHG"))
	expect_s3_class(varsFormula, "formula")
	expect_equal(as.formula("~AVAL + CHG"), varsFormula, check.attributes = FALSE)
      
})

test_that("A variable with non syntactically valid name is correctly converted to a formula", {
			
	# One variable
	varFormula <- varToFm("%m")
	expect_s3_class(varFormula, "formula")
	expect_equal(as.formula("~`%m`"), varFormula, check.attributes = FALSE)
			
})

test_that("Get JS dependencies", {
      
      dependencies <- getJsDepClinDataReview()
      expect_is(dependencies, "list")
      expect_length(dependencies, 5)
      matrixRes <- sapply(dependencies, expect_length, 10)
      
      dependencyOne <- getJsDepClinDataReview("collapsibleButton")
      expect_is(dependencyOne, "list")
      expect_length(dependencyOne[[1]], 10)
      expect_is(dependencyOne[[1]], "html_dependency")
      
      dependencyOne <- getJsDepClinDataReview("patientProfiles")
      expect_is(dependencyOne, "list")
      expect_length(dependencyOne[[1]], 10)
      expect_length(dependencyOne[[2]], 10)
      expect_length(dependencyOne[[3]], 10)
      expect_length(dependencyOne[[4]], 10)
      expect_is(dependencyOne[[1]], "html_dependency")
      expect_is(dependencyOne[[2]], "html_dependency")
      expect_is(dependencyOne[[3]], "html_dependency")
      expect_is(dependencyOne[[4]], "html_dependency")
      
    })

test_that("Collapse Html content", {
      
      x <- matrix(LETTERS[1 : 10])
      button <- collapseHtmlContent(DT::datatable(x))
      expect_is(button, "shiny.tag.list")
      expect_is(button, "list")
      names <- sapply(button, function(x) x$name)
      names(names) <- NULL
      expect_identical(
          unlist(names),
          c("input", "div", "br", "br")
      )
      
      outputConsole <- capture.output(button)
      expect_is(outputConsole, "character")
      
    })