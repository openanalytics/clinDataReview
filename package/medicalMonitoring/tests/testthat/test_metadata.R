context("Test metadata functionality")

tmpdir <- tempdir()
library(yaml)

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

test_that("Get metadata", {
      
      expect_error(
          getMetadata("fileNotExist"),          
          "Metadata file does not exist."
      )
      tmpYamlFile <- tempfile(
          pattern = "file", tmpdir = tempdir(), fileext = ".yml"
      )
      listArgs <- list(
          pathSDTMs = "pathSDTMs",
          pathMeMoADs = "pathMeMoADs",
          dateTimeMeMorun = "20200101",
          datasetInfo = list(
              list(
                  dataset = "ex.xpt",
                  datetime = "20200101"
              ),
              list(
                  dataset = "sl.xpt",
                  datetime = "20200101",
                  signatureStatus = "OK"
              )
          )
      )
      write_yaml(
          listArgs,
          file = tmpYamlFile
      )
      resMetadata <- getMetadata(filePath = tmpYamlFile)
      expect_is(resMetadata, "list")
      expect_equal(
          class(resMetadata), c("medicalMonitoringMetadata", "list")
      )
      expect_named(resMetadata, c("pathsInfo", "datasetInfo"))
      expect_is(resMetadata$pathsInfo, "matrix")
      expect_is(resMetadata$datasetInfo, "data.table")
      expect_is(resMetadata$datasetInfo, "data.frame")
      
      pathInfos <- resMetadata$pathsInfo
      expect_identical(
          rownames(pathInfos),
          c("path SDTMs", "path MeMo ADs", "date time MeMo run")
      )
      
      dfInfo <- resMetadata$datasetInfo
      expect_identical(
          dfInfo$signatureStatus,
          c(NA, "OK")
      )
      
    })



