context("Get metadata")

library(yaml)

test_that("Metadata which is present is correctly identified", {
      
	listName <- list(A = c(1, 2))
	expectation <- matrix(listName$A, nrow = 1)
	rownames(expectation) <- "A"
      
	expect_identical(
		object = clinDataReview:::checkAvailabilityMetadata(listName, subListName = "A"),
		expected = expectation
	)
	
})

test_that("Metadata which is not present is correctly identified", {
		
	listName <- list(A = c(1, 2))
	expect_identical(
		object = clinDataReview:::checkAvailabilityMetadata(listName, subListName = "B"),
		expected = "Not available"
	)
      
})

test_that("An error is generated when the metadata file is not specified as a character", {
      
	expect_error(
		object = getMetadata(filePath = 1),          
          "'filePath' argument should be a character."
	)
      
})

test_that("An error is generated when the metadata file does not exist", {
      
	expect_error(
		getMetadata("fileNotExist"),          
		"Metadata file does not exist."
	)
      
})

test_that("A warning is generated when more than one metadata file is provided", {
      
	# create metadata
	tmpYamlFileExtra <- tempfile(pattern = "file", fileext = ".yml")
	write_yaml(
		x = list(path1 = "pathSDTMs"),
		file = tmpYamlFileExtra
	)
	
	filePaths <- c(tmpYamlFileExtra, tmpYamlFileExtra)
	expect_warning(
		resMetadata1 <- getMetadata(filePaths),
		"More than one 'filePath' provided. Only the first one will be used."
	)
	expect_s3_class(resMetadata1, "list")
	expect_s3_class(resMetadata1, "clinDataReviewMetadata")
	expect_named(resMetadata1, c("summaryInfo", "datasetInfo"))
      
	summaryInfos <- resMetadata1$summaryInfo
	expect_identical(
		object = rownames(summaryInfos),
		expected = c("path1", "dateTime")
	)
	expect_identical(object = summaryInfos[2], expected = "Not available")
      
})

test_that("Unavailable metadata inputs are correctly identified", {
      
	# create metadata
	tmpYamlFileNA <- tempfile(pattern = "file", fileext = ".yml")
	listArgsNA <- list(
		path1 = "pathSDTMs",
		path2 = "pathNewSDTMs",
		date = "20200101"
	)
	write_yaml(x = listArgsNA, file = tmpYamlFileNA)   
	
	resMetadataNA <- getMetadata(tmpYamlFileNA)
	expect_s3_class(resMetadataNA, "list")
	expect_s3_class(resMetadataNA, "clinDataReviewMetadata")
	expect_named(object = resMetadataNA, expected = c("summaryInfo", "datasetInfo"))
      
	expect_identical(
		object = colnames(resMetadataNA$datasetInfo),
		expected = "Not.Available"
	)
      
})

test_that("Metadata is correctly extracted from a metadata file", {

	# create metadata
	tmpYamlFile <- tempfile(pattern = "file", fileext = ".yml")
	listArgs <- list(
		pathSDTMs = "pathSDTMs",
		pathNewSDTM = "pathNewSDTM",
		dateTime = "20200101",
		datasetInfo = list(
			list(
				column1 = "ex.xpt",
				column2 = "20200101"
			),
			list(
				column1 = "sl.xpt",
				column2 = "20200101",
				column3 = "OK"
			)
		)
	)
	write_yaml(x = listArgs, file = tmpYamlFile)
	
	resMetadata <- getMetadata(filePath = tmpYamlFile)
	expect_s3_class(resMetadata, "list")
	expect_s3_class(resMetadata, "clinDataReviewMetadata")
	expect_named(resMetadata, c("summaryInfo", "datasetInfo"))
	expect_s3_class(resMetadata$datasetInfo, "data.table")
	expect_s3_class(resMetadata$datasetInfo, "data.frame")
      
	summaryInfo <- resMetadata$summaryInfo
	expect_identical(
		object = rownames(summaryInfo),
		expected = c("pathSDTMs", "pathNewSDTM", "dateTime")
	)
      
	dfInfo <- resMetadata$datasetInfo
	expect_identical(
		object = dfInfo$column3,
		expected = c(NA, "OK")
	)
      
})

test_that("Metadata information is correctly renamed with similar names as metadata tags", {
			
	# create metadata
	tmpYamlFile <- tempfile(pattern = "file", fileext = ".yml")
	listArgs <- list(
		pathSDTMs = "pathSDTMs",
		pathNewSDTM = "pathNewSDTM",
		dateTime = "20200101"
	)
	write_yaml(x = listArgs, file = tmpYamlFile)
      
	resMetadata <- getMetadata(filePath = tmpYamlFile)
	summaryInfo <- resMetadata$summaryInfo
	namesInfo <- setNames(rownames(summaryInfo), rownames(summaryInfo))
      
	resRename <- clinDataReview:::renamePathDateInfoMetadata(
		summaryInfo = summaryInfo, 
		namesInfo = namesInfo
	)
	expect_identical(
		object = rownames(resRename),
		expected = rownames(summaryInfo)
	)
	
})

test_that("Metadata information is correctly renamed with new names", {
			
	# create metadata
	tmpYamlFile <- tempfile(pattern = "file", fileext = ".yml")
	listArgs <- list(
		pathSDTMs = "pathSDTMs",
		pathNewSDTM = "pathNewSDTM",
		dateTime = "20200101"
	)
	write_yaml(x = listArgs, file = tmpYamlFile)
			
	resMetadata <- getMetadata(filePath = tmpYamlFile)
	summaryInfo <- resMetadata$summaryInfo
	namesInfo <- setNames(c("Name 1", "Name 2", "Name 3"), rownames(summaryInfo))
			
	resRename <- clinDataReview:::renamePathDateInfoMetadata(
		summaryInfo = summaryInfo, 
		namesInfo = namesInfo
	)
	expect_identical(
		object = rownames(resRename),
		expected = c("Name 1", "Name 2", "Name 3")
	)
	
})

test_that("Metadata information is correctly renamed with new names in a different order than the metadata tags", {
			
	# create metadata
	tmpYamlFile <- tempfile(pattern = "file", fileext = ".yml")
	listArgs <- list(
		pathSDTMs = "pathSDTMs",
		pathNewSDTM = "pathNewSDTM",
		dateTime = "20200101"
	)
	write_yaml(x = listArgs, file = tmpYamlFile)
      
	resMetadata <- getMetadata(filePath = tmpYamlFile)
	summaryInfo <- resMetadata$summaryInfo
	
	namesInfoUnordered <- c(
		"pathSDTMs" = "Name 1",
		"dateTime" = "Name 3",
		"pathNewSDTM" = "Name 2"
	)
	resRenameUnordered <- clinDataReview:::renamePathDateInfoMetadata(
		summaryInfo = summaryInfo, 
		namesInfo = namesInfoUnordered
	)
	expect_identical(
		object = rownames(resRenameUnordered),
		expected = unname(namesInfoUnordered[names(listArgs)])
	)
      
})

test_that("The report creation date is correctly added to the metadata", {
			
	# create metadata
	tmpYamlFile <- tempfile(pattern = "file", fileext = ".yml")
	listArgs <- list(
		pathSDTMs = "pathSDTMs",
		pathNewSDTM = "pathNewSDTM",
		dateTime = "20200101"
	)
	write_yaml(x = listArgs, file = tmpYamlFile)
      
	resMetadata <- getMetadata(filePath = tmpYamlFile)
	summaryInfo <- resMetadata$summaryInfo
	resDate <- clinDataReview:::addDateOfReportRun(summaryInfo)
	expect_equal(
		object = nrow(resDate), 
		expected = nrow(summaryInfo) + 1
	)
	expect_identical(
		object = rownames(resDate)[nrow(resDate)],
		expected = "dateToday"
	)
      
})

test_that("Path information is outputted in the correct format", {
		
	# create metadata
	tmpYamlFile <- tempfile(pattern = "file", fileext = ".yml")
	listArgs <- list(
		pathSDTMs = "pathSDTMs",
		pathNewSDTM = "pathNewSDTM",
		dateTime = "20200101"
	)
	write_yaml(x = listArgs, file = tmpYamlFile)
      
	resMetadata <- getMetadata(filePath = tmpYamlFile)
	summaryInfo <- resMetadata$summaryInfo
	namesInfo <- setNames(rownames(summaryInfo), rownames(summaryInfo))
	kableFormat <- clinDataReview:::formatPathDateInfoMetadata(
		summaryInfo = summaryInfo, 
		namesInfo = namesInfo
	)
	expect_s3_class(kableFormat, "knitr_kable")
      
})

test_that("Metadata is printed in the correct format", {
      
	# create metadata
	tmpYamlFile <- tempfile(pattern = "file", fileext = ".yml")
	listArgs <- list(
		pathSDTMs = "pathSDTMs",
		pathNewSDTM = "pathNewSDTM",
		dateTime = "20200101"
	)
	write_yaml(x = listArgs, file = tmpYamlFile)
	
	resMetadata <- getMetadata(tmpYamlFile)
      
	expect_output(
		resPrintWithoutOptions <- clinDataReview:::knit_print.clinDataReviewMetadata(
	          x = resMetadata
		)
	)
	expect_s3_class(resPrintWithoutOptions, "knit_asis")
	
})

test_that("Metadata is printed in the correct format with a report date", {
			
	# create metadata
	tmpYamlFile <- tempfile(pattern = "file", fileext = ".yml")
	listArgs <- list(
		pathSDTMs = "pathSDTMs",
		pathNewSDTM = "pathNewSDTM",
		dateTime = "20200101"
	)
	write_yaml(x = listArgs, file = tmpYamlFile)
      
	resMetadata <- getMetadata(tmpYamlFile)
	expect_output(
		resPrintWithDate <- clinDataReview:::knit_print.clinDataReviewMetadata(
			x = resMetadata,
			options = list(dateReportRun = TRUE)
		)
	)
	expect_s3_class(resPrintWithDate, "knit_asis")
	
})

test_that("Metadata is printed in the correct format without a report date", {
			
	# create metadata
	tmpYamlFile <- tempfile(pattern = "file", fileext = ".yml")
	listArgs <- list(
		pathSDTMs = "pathSDTMs",
		pathNewSDTM = "pathNewSDTM",
		dateTime = "20200101"
	)
	write_yaml(x = listArgs, file = tmpYamlFile)
      
	resMetadata <- getMetadata(tmpYamlFile)
	expect_output(
		resPrintWithoutDate <- clinDataReview:::knit_print.clinDataReviewMetadata(
			x = resMetadata,
			options = list(dateReportRun = FALSE)
		)
	)
	expect_s3_class(resPrintWithoutDate, "knit_asis")
      
})



