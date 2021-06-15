context("Test metadata functionality")

tmpdir <- tempdir()
library(yaml)

tmpYamlFile <- tempfile(
    pattern = "file", tmpdir = tempdir(), fileext = ".yml"
)
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
write_yaml(
    listArgs,
    file = tmpYamlFile
)

test_that("Check availability of metadata", {
      
      listName <- list(A = c(1, 2))
      
      expectation <- matrix(listName$A, nrow = 1)
      rownames(expectation) <- "A"
      
      expect_identical(
          clinDataReview:::checkAvailabilityMetadata(listName, subListName = "A"),
          expectation
      )
      
      expect_identical(
          clinDataReview:::checkAvailabilityMetadata(listName, subListName = "B"),
          "Not available"
      )
      
    })

test_that("Error in get metadata when input is not character", {
      
      expect_error(
          getMetadata(filePath = 1),          
          "'filePath' argument should be a character."
      )
      
    })

test_that("Error in get metadata when file does not exist", {
      
      expect_error(
          getMetadata("fileNotExist"),          
          "Metadata file does not exist."
      )
      
    })

test_that("Warning in get metadata when more than one file is provided", {
      
      tmpYamlFileExtra <- tempfile(
          pattern = "file", tmpdir = tempdir(), fileext = ".yml"
      )
      write_yaml(
          list(path1 = "pathSDTMs"),
          file = tmpYamlFileExtra
      )
      filePaths <- c(tmpYamlFileExtra, tmpYamlFile)
      expect_warning(
          resMetadata1 <- getMetadata(filePaths),
          "More than one 'filePath' provided. Only the first one will be used."
      )
      expect_is(resMetadata1, "list")
      expect_equal(
          class(resMetadata1), c("clinDataReviewMetadata", "list")
      )
      expect_named(resMetadata1, c("summaryInfo", "datasetInfo"))
      
      summaryInfos <- resMetadata1$summaryInfo
      expect_identical(
          rownames(summaryInfos),
          c("path1", "dateTime")
      )
      expect_identical(summaryInfos[2], "Not available")
      
    })

test_that("Not available metadata inputs", {
      
      tmpYamlFileNA <- tempfile(
          pattern = "file", tmpdir = tempdir(), fileext = ".yml"
      )
      listArgsNA <- list(
          path1 = "pathSDTMs",
          path2 = "pathNewSDTMs",
          date = "20200101"
      )
      write_yaml(
          listArgsNA,
          file = tmpYamlFileNA
      )      
      expect_silent(
          resMetadataNA <- getMetadata(tmpYamlFileNA)
      )
      expect_is(resMetadataNA, "list")
      expect_equal(
          class(resMetadataNA), c("clinDataReviewMetadata", "list")
      )
      expect_named(resMetadataNA, c("summaryInfo", "datasetInfo"))
      
      expect_identical(
          colnames(resMetadataNA$datasetInfo),
          "Not.Available"
      )
      
    })

test_that("Get metadata", {
      
      expect_silent(
          resMetadata <- getMetadata(filePath = tmpYamlFile)
      )
      expect_is(resMetadata, "list")
      expect_equal(
          class(resMetadata), c("clinDataReviewMetadata", "list")
      )
      expect_named(resMetadata, c("summaryInfo", "datasetInfo"))
      expect_is(resMetadata$summaryInfo, "matrix")
      expect_is(resMetadata$datasetInfo, "data.table")
      expect_is(resMetadata$datasetInfo, "data.frame")
      
      summaryInfo <- resMetadata$summaryInfo
      expect_identical(
          rownames(summaryInfo),
          c("pathSDTMs", "pathNewSDTM", "dateTime")
      )
      
      dfInfo <- resMetadata$datasetInfo
      expect_identical(
          dfInfo$column3,
          c(NA, "OK")
      )
      
    })

test_that("Rename metadata paths info", {
      
      expect_silent(
          resMetadata <- getMetadata(filePath = tmpYamlFile)
      )
      summaryInfo <- resMetadata$summaryInfo
      
      namesInfo <- setNames(rownames(summaryInfo), rownames(summaryInfo))
      
      expect_silent(
          resRename <- clinDataReview:::renamePathDateInfoMetadata(summaryInfo, namesInfo)
      )
      expect_identical(
          rownames(resRename),
          rownames(summaryInfo)
      )
      
      namesInfo <- setNames(c("Name 1", "Name 2", "Name 3"), rownames(summaryInfo))
      
      expect_silent(
          resRename <- clinDataReview:::renamePathDateInfoMetadata(summaryInfo, namesInfo)
      )
      expect_identical(
          rownames(resRename),
          c("Name 1", "Name 2", "Name 3")
      )
      
      namesInfoUnordered <- c(
          "pathSDTMs" = "Name 1",
          "dateTime" = "Name 3",
          "pathNewSDTM" = "Name 2"
      )
      expect_silent(
          resRenameUnordered <- clinDataReview:::renamePathDateInfoMetadata(summaryInfo, namesInfoUnordered)
      )
      expect_identical(
          rownames(resRenameUnordered),
          rownames(resRename)
      )
      
    })

test_that("Add date of report running", {
      
      expect_silent(
          resMetadata <- getMetadata(filePath = tmpYamlFile)
      )
      summaryInfo <- resMetadata$summaryInfo
      
      expect_silent(
          resDate <- clinDataReview:::addDateOfReportRun(summaryInfo)
      )
      expect_equal(
          nrow(resDate), nrow(summaryInfo) + 1
      )
      expect_identical(
          rownames(resDate)[nrow(resDate)],
          "dateToday"
      )
      
    })

test_that("Format output of paths info", {
      
      expect_silent(
          resMetadata <- getMetadata(filePath = tmpYamlFile)
      )
      summaryInfo <- resMetadata$summaryInfo
      namesInfo <- setNames(rownames(summaryInfo), rownames(summaryInfo))
      expect_silent(
          kableFormat <- clinDataReview:::formatPathDateInfoMetadata(summaryInfo, namesInfo)
      )
      expect_is(kableFormat, "knitr_kable")
      
    })

test_that("Print metadata", {
      
      resMetadata <- getMetadata(tmpYamlFile)
      
      resPrintWithoutOptions <- clinDataReview:::knit_print.clinDataReviewMetadata(
          getMetadata(tmpYamlFile)
      )
      expect_is(resPrintWithoutOptions, "knit_asis")
      
      resPrintWithDate <- clinDataReview:::knit_print.clinDataReviewMetadata(
          getMetadata(tmpYamlFile),
          options = list(dateReportRun = TRUE)
      )
      expect_is(resPrintWithDate, "knit_asis")
      
      resPrintWithoutDate <- clinDataReview:::knit_print.clinDataReviewMetadata(
          getMetadata(tmpYamlFile),
          options = list(dateReportRun = FALSE)
      )
      expect_is(resPrintWithoutDate, "knit_asis")
      
    })



