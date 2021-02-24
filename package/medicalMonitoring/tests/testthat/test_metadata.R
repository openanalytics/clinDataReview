context("Test metadata functionality")

tmpdir <- tempdir()
library(yaml)

tmpYamlFile <- tempfile(
    pattern = "file", tmpdir = tempdir(), fileext = ".yml"
)
listArgs <- list(
    pathSDTMs = "pathSDTMs",
    pathMeMoADs = "pathMeMoADs",
    dateTimeMeMorun = "20200101",
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
      
      expect_identical(
          checkAvailabilityMetadata(listName, subListName = "A"),
          listName$A
      )
      
      expect_identical(
          checkAvailabilityMetadata(listName, subListName = "B"),
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
          class(resMetadata1), c("medicalMonitoringMetadata", "list")
      )
      expect_named(resMetadata1, c("pathsInfo", "datasetInfo"))
      
      pathsInfos <- resMetadata1$pathsInfo
      expect_identical(
          rownames(pathsInfos),
          c("pathSDTMs", "pathMeMoADs", "dateTimeMeMorun")
      )
      expect_identical(pathsInfos[1], "Not available")
      
    })

test_that("Not available metadata inputs", {
      
      tmpYamlFileNA <- tempfile(
          pattern = "file", tmpdir = tempdir(), fileext = ".yml"
      )
      listArgsNA <- list(
          path1 = "pathSDTMs",
          path2 = "pathMeMoADs",
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
          class(resMetadataNA), c("medicalMonitoringMetadata", "list")
      )
      expect_named(resMetadataNA, c("pathsInfo", "datasetInfo"))
      
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
          class(resMetadata), c("medicalMonitoringMetadata", "list")
      )
      expect_named(resMetadata, c("pathsInfo", "datasetInfo"))
      expect_is(resMetadata$pathsInfo, "matrix")
      expect_is(resMetadata$datasetInfo, "data.table")
      expect_is(resMetadata$datasetInfo, "data.frame")
      
      pathsInfo <- resMetadata$pathsInfo
      expect_identical(
          rownames(pathsInfo),
          c("pathSDTMs", "pathMeMoADs", "dateTimeMeMorun")
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
      pathsInfo <- resMetadata$pathsInfo
      expect_silent(
          resRename <- renamePathDateInfoMetadata(pathsInfo)
      )
      expect_identical(
          rownames(resRename),
          c(
              "Original data (SDTM) path:",
              "Medical Monitoring Analysis Dataset (MeMo-AD) path:",
              "MeMo-AD creation date:"
          )
      )
      
    })

test_that("Add date of report running", {
      
      expect_silent(
          resMetadata <- getMetadata(filePath = tmpYamlFile)
      )
      pathsInfo <- resMetadata$pathsInfo
      
      expect_silent(
          resDate <- addDateOfReportRun(pathsInfo)
      )
      expect_equal(
          nrow(resDate), nrow(pathsInfo) + 1
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
      pathsInfo <- resMetadata$pathsInfo
      expect_silent(
          kableFormat <- formatPathDateInfoMetadata(pathsInfo)
      )
      expect_is(kableFormat, "knitr_kable")
      
    })

test_that("Print metadata", {
      
      resMetadata <- getMetadata(tmpYamlFile)
      
      resPrintWithoutOptions <- medicalMonitoring:::knit_print.medicalMonitoringMetadata(
          getMetadata(tmpYamlFile)
      )
      expect_is(resPrintWithoutOptions, "knit_asis")
      
      resPrintWithDate <- medicalMonitoring:::knit_print.medicalMonitoringMetadata(
          getMetadata(tmpYamlFile),
          options = list(dateReportRun = TRUE)
      )
      expect_is(resPrintWithDate, "knit_asis")
      
      resPrintWithoutDate <- medicalMonitoring:::knit_print.medicalMonitoringMetadata(
          getMetadata(tmpYamlFile),
          options = list(dateReportRun = FALSE)
      )
      expect_is(resPrintWithoutDate, "knit_asis")
      
    })



