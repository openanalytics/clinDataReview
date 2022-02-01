context("Annotate data")

library(haven)

test_that("Data is correctly annotated from one dataset to another", {
			
	dataLB <- data.frame(
		USUBJID = seq.int(5),
		AVAL = c(1, 2, 3, 4, 5),
		VISIT = c("Day 1", "Day 1", "Day 2", "Day 2", "Day 1"),
		PARAMCD = c("ALS", "ALT", "ALT", "BILI", "BILI"),
		stringsAsFactors = FALSE
	)
	
	dataDM <- data.frame(
		USUBJID = seq.int(5),
		SAFFL = "Y",
		SEX = c("F", "F", "M", "M", "M"),
		AGE = c(54, 78, 34, 51, 67),
		YEAR = c(1967, 1943, 1987, 1970, 1954),
		stringsAsFactors = FALSE
	)
			
	dataAnnot <- annotateData(
		data = dataLB, 
		annotations = list(data = dataDM)
	)

	newCols <- setdiff(colnames(dataAnnot), colnames(dataLB))
	expect_equivalent(
		object = dataDM[match(dataLB$USUBJID, dataDM$USUBJID), newCols],
		expected = dataAnnot[, newCols]
	)	
      
})

test_that("A warning is generated if the annotation data and data to annotate contain variables with the same name", {
			
	dataLB <- data.frame(
		USUBJID = seq.int(5),
		AVAL = c(1, 2, 3, 4, 5),
		SAFFL = "Y"
	)

	dataDM <- data.frame(
		USUBJID = seq.int(5),
		SAFFL = "Y"
	)

	expect_warning(
		dataAnnot <- annotateData(
			data = dataLB, 
			annotations = list(data = dataDM)
		),
		"SAFFL.*already available"
	)
	
})

test_that("Data is correctly annotated with a new variable, computed based on a combination of multiple variables", {
      
	data <- data.frame(
		AVAL = c(1, 2, 3, 4, 5),
		AVAL2 = 10,
		stringsAsFactors = FALSE
	)
			
	dataAnnot <- annotateData(
		data = data, 
		annotations = list(vars = "RATIO", varFct = "AVAL / AVAL2")
	)
      
	expect_equal(
		object = dataAnnot$RATIO,
		expected = with(dataAnnot, AVAL / AVAL2),
		check.attributes = FALSE
	)
      
})

test_that("Data is correctly annotated with a new variable, from a function of the variables", {
      
	data <- data.frame(
		VISIT = c("Day 1", "Day 1", "Day 2", "Day 2", "Day 1"),
		stringsAsFactors = FALSE
	)
			
	varFct <- "sub('Day .* - (.+)', '\\\\1', VISIT)"
	dataAnnot <- annotateData(
		data = data, 
		annotations = list(
			vars = "PERIOD", 
			varFct = varFct
		)
	)
      
	expect_equal(
		object = dataAnnot$PERIOD,
		expected = eval(expr = parse(text = varFct), envir = data),
		check.attributes = TRUE
	)
	
})

test_that("An error is generated if a new variable is not specified when the data is annotated based on a function of the variables", {
		
	data <- data.frame(
		AGE = c(54, 78, 34, 51, 67),
		YEAR = c(1967, 1943, 1987, 1970, 1954)
	)
			
	expect_error(
		annotateData(
			data = data,
			annotations = list(
				varFct = 'sprintf("%s %s", AGE, YEAR)',
				varLabel = "Age and year"
			)
		),
		"'vars' should be specified"
	)
	
})

test_that("Data is correctly annotated with a new variable, from a function of the data, specified as a character", {
		
	data <- data.frame(
		AGE = c(54, 78, 34, 51, 67),
		YEAR = c(1967, 1943, 1987, 1970, 1954)
	)
			
	dataAnnot <- annotateData(
		data = data,
		annotations = list(
			vars = "AGEYEAR",
			varFct = 'function(data) with(data, sprintf("%s %s", data$AGE, data$YEAR))',
			varLabel = "Age and year"
		)
	)
	
	expect_equal(
		object = dataAnnot$AGEYEAR,
		expected = with(data, sprintf("%s %s", AGE, YEAR)),
		check.attributes = TRUE
	)
	
})

test_that("Data is correctly annotated with a new variable, from a function of the data", {
			
	data <- data.frame(
		AGE = c(54, 78, 34, 51, 67),
		YEAR = c(1967, 1943, 1987, 1970, 1954)
	)		
	
	dataAnnot <- annotateData(
		data = data,
          annotations = list(
			vars = "AGEYEAR",
			varFct = function(data) with(data, sprintf("%s %s", data$AGE, data$YEAR)),
			varLabel = "Age and year"
		)
	)
	
	expect_equal(
		object = dataAnnot$AGEYEAR,
		expected = with(data, sprintf("%s %s", AGE, YEAR)),
		check.attributes = TRUE
	)
	
})

test_that("A warning is generated if the exported data for annotation is not available in the specified path", {
			
	data <- data.frame(
		USUBJID = seq.int(5),
		AVAL = c(1, 2, 3, 4, 5)
	)
			
	expect_warning(
		dataAnnot <- annotateData(
			data = data,
			dataPath = tempfile(),
			annotations = list(dataset = "dm")
		),
		"not annotated with.*dm.*dataset"
	)
	expect_identical(object = dataAnnot, expected = data)
			
})

test_that("Data is correctly annotated based on an exported dataset", {

	data <- data.frame(
		USUBJID = seq.int(5),
		AVAL = c(1, 2, 3, 4, 5)
	)
	# export annotation dataset
	dataDM <- data.frame(
		USUBJID = seq.int(5),
		SAFFL = "Y",
		SEX = c("F", "F", "M", "M", "M"),
		AGE = c(54, 78, 34, 51, 67),
		YEAR = c(1967, 1943, 1987, 1970, 1954),
		stringsAsFactors = FALSE
	)
	dir <- tempdir("annotation")
	dataPath <- file.path(dir, "dm.xpt")
	write_xpt(data = dataDM, path = dataPath)
	
	dataAnnot <- annotateData(
		data = data,
		dataPath = dir,
		annotations = list(dataset = "dm")
	)
	
	expect_equal(
		object = dataAnnot[, colnames(dataDM)],
		expected = dataDM[match(dataAnnot$USUBJID, dataDM$USUBJID), ]
	)
	  
})

test_that("Data is correctly annotated with a specified variable based on an exported dataset", {
			
	data <- data.frame(
		USUBJID = seq.int(5),
		AVAL = c(1, 2, 3, 4, 5)
	)
	# export annotation dataset
	dataDM <- data.frame(
		USUBJID = seq.int(5),
		AGE = c(54, 78, 34, 51, 67),
		YEAR = c(1967, 1943, 1987, 1970, 1954),
		stringsAsFactors = FALSE
	)
	dir <- tempdir("annotation")
	dataPath <- file.path(dir, "dm.xpt")
	write_xpt(data = dataDM, path = dataPath)
		
	dataAnnot <- annotateData(
		data = data,
		dataPath = dir,
		annotations = list(dataset = "dm", vars = "AGE")
	)
	expect_identical(
		object = colnames(dataAnnot),
		expected = c(colnames(data), "AGE")
	)
	expect_equal(
		object = dataAnnot$AGE, 
		expected = dataDM[match(data$USUBJID, dataDM$USUBJID), "AGE"]
	)
    
})

test_that("Data is correctly annotated based on a filtered dataset", {
      
	dataLB <- data.frame(
		USUBJID = seq.int(5),
		AVAL = c(1, 2, 3, 4, 5)
	)
	dataDM <- data.frame(
		USUBJID = seq.int(5),
		AGE = c(54, 78, 34, 51, 67),
		YEAR = c(1967, 1943, 1987, 1970, 1954),
		SEX = c("F", "F", "M", "M", "M")
	)
	
	dataAnnotFilter <- annotateData(
		data = dataLB,
		annotations = list(
			data = dataDM,
			filters = list(var = "SEX", value = "M")
		)
	)
	idsFiltered <- subset(dataDM, SEX == "M")$USUBJID
	newCols <- setdiff(colnames(dataDM), colnames(dataLB))
	
	# records filtered have missing info
	dataAnnotFilteredMissing <- subset(dataAnnotFilter, !USUBJID %in% idsFiltered, select = newCols)
	expect_true(all(is.na(dataAnnotFilteredMissing)))

	# selected records have correct info filled
	expect_equal(
		object = subset(dataAnnotFilter, USUBJID %in% idsFiltered, select = newCols),
		expected = subset(dataDM, USUBJID %in% idsFiltered, select = newCols),
		check.attributes = FALSE
	)
      
})

test_that("Data is correctly annotated based on multiple annotations", {
      
	data <- data.frame(
		USUBJID = seq.int(5),
		AGE = c(54, 78, 34, 51, 67),
		YEAR = c(1967, 1943, 1987, 1970, 1954),
		SEX = c("F", "F", "M", "M", "M"),
		stringsAsFactors = TRUE
	)
			
	dataAnnot <- annotateData(
		data = data,
		annotations = list(
			list(
				vars = "SEXFCT",
 				varFct = 'as.factor(SEX)'
			),
			list(
				vars = "AGESTRING",
				varFct = 'sprintf("%s %s", AGE, YEAR)',
				varLabel = "Age and year"
			)
		)
	)
	
	expect_identical(
		object = dataAnnot$SEXFCT, 
		expected = as.factor(data$SEX)
	)

	expect_identical(
		object = dataAnnot$AGESTRING, 
		expected = with(data, sprintf("%s %s", AGE, YEAR))
	)
      
})
test_that("An error is generated if the preset annotation option is not available", {
			
	dataLB <- data.frame(
		USUBJID = seq.int(5),
		AVAL = c(1, 2, 3, 4, 5)
	)
			
	expect_error(
		annotateData(data = dataLB, annotations = "demographic"),
		"Data is not annotated, because 'annotations' should be one of"
	)
			
})

test_that("A warning is generated if the data is annotated with the preset 'demographics' option but no such data is available", {
			
	dataLB <- data.frame(
		USUBJID = seq.int(5),
		AVAL = c(1, 2, 3, 4, 5)
	)
	expect_warning(
		annotateData(dataLB, annotations = "demographics"),
		"Data is not annotated with demographics data because no such data"
	)
			
})

test_that("Data is correctly annotated based on the preset 'demographics' option", {
			
	dataLB <- data.frame(
		USUBJID = seq.int(5),
		AVAL = c(1, 2, 3, 4, 5)
	)
	
	# export annotation dataset
	dataDM <- data.frame(
		USUBJID = seq.int(5),
		SEX = c("F", "F", "M", "M", "M"),
		AGE = c(54, 78, 34, 51, 67),
		YEAR = c(1967, 1943, 1987, 1970, 1954),
		stringsAsFactors = FALSE
	)
	dir <- tempdir("annotation")
	dataPath <- file.path(dir, "dm.xpt")
	write_xpt(data = dataDM, path = dataPath)
	
	dataAnnot <- annotateData(
		data = dataLB,
		dataPath = dir,
		annotations = "demographics"
	)
	expect_equal(
		object = dataAnnot[match(dataLB$USUBJID, dataDM$USUBJID), c("AGE", "SEX")],
		expected = dataDM[, c("AGE", "SEX")]
	)
			
})

test_that("A warning is generated if the data is annotated with the preset 'functional groups' option but no parameter code variable is available", {
			
	data <- data.frame(
		USUBJID = seq.int(5),
		AVAL = c(1, 2, 3, 4, 5)
	)
			
	expect_warning(
		dataAnnot <- annotateData(data = data, annotations = "functional_groups_lab"),
		"Data is not annotated with functional groups, because no variable"
	)
	expect_identical(object = dataAnnot, expected = data)
			
})

test_that("Data is correctly annotated based on the preset 'functional_groups_lab' option", {
			
	data <- data.frame(
		USUBJID = seq.int(4),
		AVAL = c(1, 2, 3, 4),
		PARAMCD = c("ALS", "ALT", "ALP", "CHOL"),
		stringsAsFactors = TRUE
	)
	dataAnnot <- annotateData(
		data = data,
		annotations = "functional_groups_lab"
	)
	expect_s3_class(dataAnnot, "data.frame")
	expect_identical(
		object = colnames(dataAnnot),
		expected = c(colnames(data), "LBFCTGRP")
	)
	expect_s3_class(dataAnnot$LBFCTGRP, "factor")
	expect_identical(
		object = as.character(dataAnnot$LBFCTGRP),
		expected = c("Other", "Liver function", "Liver function", "Lipids")
	)
			
})


test_that("A warning is generated when annotating with the preset 'exposed_subjects' option, but no exposure data is present", {
     
	data <- data.frame(USUBJID = seq.int(5))
			
	expect_warning(
		annotateData(
			data = data,
			dataPath = "path/To/Data",
			annotations = "exposed_subjects"
		),
		"Data is not annotated with exposure data, because no such data"
	)
      
})

test_that("Data is correctly annotated based on the preset 'exposed_subjects' option", {

	dataLB <- data.frame(
		USUBJID = seq.int(5),
		AVAL = c(1, 2, 3, 4, 5)
	)
	
	# export annotation dataset
	dataEX <- data.frame(
		USUBJID = seq.int(4),
		STDY = c("0103", "0102", "0104", ""),
		stringsAsFactors = FALSE
	)
	dir <- tempdir("annotation")
	dataPath <- file.path(dir, "ex.xpt")
	write_xpt(data = dataEX, path = dataPath)
	
	dataAnnotEx <- annotateData(
		data = dataLB,
		dataPath = dir,
		annotations = "exposed_subjects"
	)
	# subjects with start date in exposure dataset
	expect_setequal(
		object = subset(dataAnnotEx, USUBJID %in% seq.int(3))$EXFL,
		expected = TRUE
	)
	# subjects without start date in exposure dataset
	expect_setequal(
		object = subset(dataAnnotEx, !USUBJID %in% seq.int(3))$EXFL,
		expected = FALSE
	)
      
})

test_that("An error is generated when annotating with the preset 'exposed_subjects' option, but this dataset does not contain a subject variable", {

	dataLB <- data.frame(
		USUBJID = seq.int(5),
		AVAL = c(1, 2, 3, 4, 5)
	)
			
	# export annotation dataset
	dataEX <- data.frame(
		STDY = c("0103", "0102", "0104", ""),
		stringsAsFactors = FALSE
	)
	dir <- tempdir("annotation")
	dataPath <- file.path(dir, "ex.xpt")
	write_xpt(data = dataEX, path = dataPath)		
			
	expect_error(
		annotateData(
			data = dataLB,
			dataPath = dir,
			annotations = "exposed_subjects",
			subjectVar = "USUBJID"
		),
		"doesn't contain the subject variable"
	)
      
})

test_that("An error is generated when annotating with the preset 'exposed_subjects' option, but this dataset does not contain a start time variable", {
			
	dataLB <- data.frame(
		USUBJID = seq.int(5),
		AVAL = c(1, 2, 3, 4, 5)
	)
			
	# export annotation dataset
	dataEX <- data.frame(USUBJID = seq.int(4))
	dir <- tempdir("annotation")
	dataPath <- file.path(dir, "ex.xpt")
	write_xpt(data = dataEX, path = dataPath)		
			
	expect_error(
		annotateData(
			data = dataLB,
			dataPath = dir,
			annotations = "exposed_subjects",
			subjectVar = "USUBJID"
		),
		"doesn't contain a start time variable"
	)
			
})

test_that("Data is correctly annotated from a filtered subsetted variable by group", {
		
	# extract baseline records
	data <- data.frame(
		AVISIT = c("Baseline", "Week 4", "Baseline", "Week 4"),
		USUBJID = c(1, 1, 2, 2),
		AVAL = c(4, 2, 6, 3)
	)
	
	annotatedData <- annotateData(
		data = data,
		annotations = list(
			vars = "BASE",
			varFct = "AVAL",
			varsBy = "USUBJID",
			filters = list(var = "AVISIT", value = "Baseline")
		)
	)
	
	expect_setequal(
		object = subset(annotatedData, USUBJID == 1)$BASE, 
		expected = 4
	)
	expect_setequal(
		object = subset(annotatedData, USUBJID == 2)$BASE, 
		expected = 6
	)
			
})

test_that("A warning is generated if the variable to group by has replicates", {
		
	data <- data.frame(
		AVISIT = c("Baseline", "Week 4", "Baseline", "Week 4"),
		USUBJID = c(1, 1, 2, 2),
		AVAL = c(4, 2, 6, 3)
	)
			
	expect_warning(
		annotateData(
			data = data,
			annotations = list(
				vars = "BASE",
				varFct = "AVAL",
				varsBy = "USUBJID"
			)
		),
		"Duplicated records.*"
	)
			
})
