context("Filter data")

# load example data
library(glpgUtilityFct)
data(SDTMDataPelican)
dataDM <- SDTMDataPelican$DM

test_that("simple condition with inclusion criteria", {
      
      dataFilt <- filterData(dataDM, filters = list(var = "SEX", value = "M"), verbose = TRUE)
      expect_equal(structure(dataFilt, msg = NULL), subset(dataDM, SEX == "M"))
      
    })

test_that("simple condition with non inclusion criteria", {
      
      dataFilt <- filterData(dataDM, filters = list(var = "SEX", value = "M", rev = TRUE), verbose = TRUE)
      expect_equal(structure(dataFilt, msg = NULL), subset(dataDM, SEX != "M"))
      
    })

test_that("simple condition with specified operator", {
      
      dataFilt <- filterData(dataDM,  list(var = "AGE", value = 20, op = "<="), verbose = TRUE)
      expect_equal(structure(dataFilt, msg = NULL), subset(dataDM, AGE <= 20))
      
    })

test_that("missing values are included by default", {
      
      dataDMNA <- dataDM
      dataDMNA[1:2, "AGE"] <- NA
      dataFilt <- filterData(dataDMNA, list(var = "AGE", value = 20, op = "<="), verbose = TRUE)
      expect_equal(structure(dataFilt, msg = NULL), subset(dataDMNA, AGE <= 20 | is.na(AGE)))
      
    })

test_that("missing values are retained (keepNA)", {
      
      dataDMNA <- dataDM
      dataDMNA[1:2, "AGE"] <- NA
      dataFilt <- filterData(dataDMNA, list(var = "AGE", value = 20, op = "<=", keepNA = FALSE), verbose = TRUE)
      expect_equal(structure(dataFilt, msg = NULL), subset(dataDMNA, AGE <= 20))
      
    })

test_that("multiple condition with specification operator and without", {
      
      filters <- list(
          list(var = "AGE", value = 20, op = "<="),
          "|",
          list(var = "SEX", value = "M"),
          list(var = "ARMCD", value = "SCRNFAIL")
      )
      dataFilt <- filterData(data = dataDM, filters = filters, verbose = TRUE)
      
      expect_equal(structure(dataFilt, msg = NULL), subset(dataDM, (AGE <= 20 | SEX == "M") & ARMCD == "SCRNFAIL"))
      
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
          filterData <- filterDataSingle(
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
      expect_true(any(grepl("msg", names(attrData))))
      expect_true(any(! grepl("labelVars", names(attrData))))
      
    })

test_that("Errors in filtering data", {
      
      data <- data.frame(
          A = c(1, 2, 3),
          B = c(4, 5, 6),
          C = c("a", "a", "b"),
          stringsAsFactors = FALSE      
      )
      
      expect_error(
          filterDataSingle(
              data = data,
              filters = list(value = "a")
          ),
          "'var' used for filtering of data should be specified."
      )
      
      expect_error(
          medicalMonitoring:::filterDataSingle(
              data = data,
              filters = list(var = "C")
          ),
          "'value' of interest or 'valueFct' to obtain it, or filtering of missing values should be specified"
      )
      
    })







