context("Filter data")

dataDM <- data.frame(
    "USUBJID" = 1 : 5,
    "SAFFL" = "Y",
    "SEX" = c("F", "F", "M", "M", "M"),
    "AGE" = c(54, 78, 34, 51, 67),
    "YEAR" = c(1967, 1943, 1987, 1970, 1954),
    "ARMCD" = c("SCRNFAIL", "PLACEBO", "TRT", "TRT", "SCRNFAIL"),
    stringsAsFactors = FALSE
)

test_that("simple condition with inclusion criteria", {
      
      dataFilt <- filterData(
          dataDM, filters = list(var = "SEX", value = "M"), verbose = TRUE
      )
      expect_equal(structure(dataFilt, msg = NULL), subset(dataDM, SEX == "M"))
      
    })

test_that("simple condition with non inclusion criteria", {
      
      dataFilt <- filterData(
          dataDM, filters = list(var = "SEX", value = "M", rev = TRUE), verbose = TRUE
      )
      expect_equal(structure(dataFilt, msg = NULL), subset(dataDM, SEX != "M"))
      
    })

test_that("simple condition with specified operator", {
      
      dataFilt <- filterData(
          dataDM,  list(var = "AGE", value = 50, op = "<="), verbose = TRUE
      )
      expect_equal(structure(dataFilt, msg = NULL), subset(dataDM, AGE <= 50))
      
    })

test_that("missing values are included by default", {
      
      dataDMNA <- dataDM
      dataDMNA[1 : 2, "AGE"] <- NA
      dataFilt <- filterData(
          dataDMNA, list(var = "AGE", value = 50, op = "<="), verbose = TRUE
      )
      expect_equal(structure(dataFilt, msg = NULL), subset(dataDMNA, AGE <= 50 | is.na(AGE)))
      
    })

test_that("missing values are retained (keepNA)", {
      
      dataDMNA <- dataDM
      dataDMNA[1 : 2, "AGE"] <- NA
      dataFilt <- filterData(dataDMNA, list(var = "AGE", value = 50, op = "<=", keepNA = FALSE), verbose = TRUE)
      expect_equal(structure(dataFilt, msg = NULL), subset(dataDMNA, AGE <= 50))
      
    })

test_that("multiple condition with specification operator and without", {
      
      filters <- list(
          list(var = "AGE", value = 50, op = "<="),
          "|",
          list(var = "SEX", value = "M"),
          list(var = "ARMCD", value = "SCRNFAIL")
      )
      dataFilt <- filterData(data = dataDM, filters = filters, verbose = TRUE)
      
      expect_equal(structure(dataFilt, msg = NULL), subset(dataDM, (AGE <= 50 | SEX == "M") & ARMCD == "SCRNFAIL"))
      
    })

test_that("Filtering of missing values", {
      
      var <- "SEX"
      nMissing <- 2
      dataDM[seq_len(nMissing), "SEX"] <- NA_character_
      
      expect_error(
          filterData(data = dataDM, filters = list(var = "SEX")), 
          regexp = "should be specified for the filtering of data"
      )
      
      expect_silent(dataFilterNA <- filterData(data = dataDM, filters = list(var = "SEX", keepNA = FALSE)))
      expect_equal(nrow(dataFilterNA), nrow(dataDM)-nMissing)
      
      expect_silent(
          dataFilterNA2 <- filterData(dataDM, 
              filters = list(var = "SEX", value = NA_character_, keepNA = FALSE, rev = TRUE)
          )
      )
      expect_equivalent(dataFilterNA, dataFilterNA2) # check without message attribute
      
    })

test_that("Filter data for a single filter in the simplest setting", {
      
      data <- data.frame(
          A = c(1, 2, 3),
          B = c(4, 5, 6),
          C = c("a", "a", "b"),
          stringsAsFactors = FALSE      
      )
      
      expect_silent(
          filterData <- clinDataReview:::filterDataSingle(
              data = data,
              filters = list(
                  var = "C", value = "a"
              )
          )
      )
      expect_s3_class(filterData, "data.frame")
      expect_equal(nrow(filterData), 2)
      expect_identical(filterData$C, c("a", "a"))
      expect_identical(class(filterData$C), "character")
      expect_identical(class(attributes(filterData)), "list")
      
      attrData <- attributes(filterData)
      expect_true("msg" %in% names(attrData))
      expect_true(! "labelVars" %in% names(attrData))
      
    })

test_that("Errors in filtering data", {
      
      data <- data.frame(
          A = c(1, 2, 3),
          B = c(4, 5, 6),
          C = c("a", "a", "b"),
          stringsAsFactors = FALSE      
      )
      
      expect_error(
          clinDataReview:::filterDataSingle(
              data = data,
              filters = list(value = "a")
          ),
          "'var' used for filtering of data should be specified."
      )
      
      expect_error(
          clinDataReview:::filterDataSingle(
              data = data,
              filters = list(var = "C")
          ),
          "'value' of interest or 'valueFct' to obtain it, or filtering of missing values should be specified"
      )
      
    })

test_that("Return all in filter data for a single filter", {
      
      data <- data.frame(
          A = c(1, 2, 3),
          B = c(4, 5, 6),
          C = c("a", "a", "b"),
          stringsAsFactors = FALSE      
      )
      
      expect_silent(
          filterData <- clinDataReview:::filterDataSingle(
              data = data,
              filters = list(var = "C", value = "a"),
              returnAll = TRUE
          )
      )
      expect_s3_class(filterData, "data.frame")
      expect_true("keep" %in% colnames(filterData))
      expect_identical(nrow(filterData), nrow(data))
      expect_equal(ncol(filterData), (ncol(data) + 1))
      expect_equal(filterData$keep, c(TRUE, TRUE, FALSE))
      
    })

test_that("Keep NA in filter data for single filter", {
      
      data <- data.frame(
          A = c(NA, 2, 3),
          B = c(4, 5, NA),
          C = c("a", NA, "b"),
          stringsAsFactors = FALSE      
      )
      expect_silent(
          filterData <- clinDataReview:::filterDataSingle(
              data = data,
              filters = list(var = "C", value = "a"),
              keepNA = TRUE
          )
      )
      expect_s3_class(filterData, "data.frame")
      expect_true(any(is.na(filterData)))
      expect_equal(nrow(filterData), 2)
      expect_equal(filterData$A, c(NA, 2))
      expect_equal(filterData$C, c("a", NA))
      
    })

test_that("Not keep NA in filter data for single filter", {
      
      data <- data.frame(
          A = c(NA, 2, 3),
          B = c(4, 5, NA),
          C = c("a", NA, "b"),
          stringsAsFactors = FALSE      
      )
      expect_silent(
          filterData <- clinDataReview:::filterDataSingle(
              data = data,
              filters = list(var = "C", value = "a"),
              keepNA = FALSE
          )
      )
      expect_s3_class(filterData, "data.frame")
      expect_true(any(is.na(filterData)))
      expect_equal(nrow(filterData), 1)
      
    })

test_that("Filter column not in filter data for single filter", {
      
      data <- data.frame(
          A = c(1, 2, 3),
          B = c(4, 5, 6),
          C = c("a", "a", "b"),
          stringsAsFactors = FALSE      
      )
      expect_warning(
          filterData <- clinDataReview:::filterDataSingle(
              data = data,
              filters = list(var = "D", value = "a"),
              keepNA = TRUE
          ),
          "Data is not filtered based on the variable: .* is not available in the input data."
      )
      expect_identical(data, filterData)
      
    })

test_that("Filter with 'byVar' in filter data for single filter", {
      
      data <- data.frame(
          A = c(1, 2, 3, 4),
          B = c(5, 6, 7, 8),
          C = c("a", "a", "b", "c"),
          D = c("cat1", "cat2", "cat1", "cat1"),
          stringsAsFactors = FALSE      
      )
      filterData <- clinDataReview:::filterDataSingle(
          data = data,
          filters = list(
              var = "A", valueFct = function(x) x > 2,
              rev = TRUE, varsBy = c("C", "D")
          )
      )
      expect_s3_class(filterData, "data.frame")
      #expect_true(nrow(filterData) == 2)
      
    })

test_that("Filter with 'varNew' in filter data for single filter", {
      
      data <- data.frame(
          A = c(1, 2, 3),
          B = c(4, 5, 6),
          C = c("a", "a", "b"),
          stringsAsFactors = FALSE      
      )
      expect_silent(
          filterData <- clinDataReview:::filterDataSingle(
              data = data,
              filters = list(var = "C", value = "a", varNew = "newVar"),
          )
      )
      expect_s3_class(filterData, "data.frame")
      expect_equal(ncol(filterData), (ncol(data) + 1))
      expect_equal(filterData$newVar, c(TRUE, TRUE))
      
      expect_warning(
          clinDataReview:::filterDataSingle(
              data = data,
              filters = list(var = "C", value = "a", varNew = "B"),
          ),
          ".* is overwritten in the data."
      )
    })

test_that("Filter with 'valueFct' in filter data for single filter", {
      
      data <- data.frame(
          A = c(1, 2, 3),
          B = c(4, 5, 6),
          C = c("a", "a", "b"),
          stringsAsFactors = FALSE      
      )
      expect_error(
          clinDataReview:::filterDataSingle(
              data = data,
              filters = list(var = "C", valueFct = grepl("a", data$C))
          ),
          "'valueFct' should be a character or a function."
      )
      
      expect_silent(
          filterData <- clinDataReview:::filterDataSingle(
              data = data,
              filters = list(var = "A", valueFct = function(x) x > 1)
          )
      )
      expect_s3_class(filterData, "data.frame")
      expect_equal(nrow(filterData), 1)
      
      expect_silent(
          filterData2 <- clinDataReview:::filterDataSingle(
              data = data,
              filters = list(var = "A", valueFct = "function(x) x > 1")
          )
      )
      expect_s3_class(filterData2, "data.frame")
      #expect_equal(filterData, filterData2)
      
    })