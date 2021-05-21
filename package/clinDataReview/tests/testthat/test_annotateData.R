context("Data annotation")

##########################################################
## Create data sets for testing 'annotateData' function ##
##########################################################
library(haven)

dataEX <- data.frame(
    "USUBJID" = 1 : 5,
    #"EXSTDTC" = c(),
    "STDY" = c("0103", "0102", "0104", "0125", "0120"),
    "ASTDY" = c("0803", "0802", "0804", "0825", "0820"),
    stringsAsFactors = FALSE
)
testPathData <- tempdir()
write_xpt(dataEX, file.path(testPathData, "adex.xpt"))


dataDM <- data.frame(
    "USUBJID" = 1 : 5,
    "SAFFL" = "Y",
    "SEX" = c("F", "F", "M", "M", "M"),
    "AGE" = c(54, 78, 34, 51, 67),
    "YEAR" = c(1967, 1943, 1987, 1970, 1954),
    stringsAsFactors = FALSE
)

dataLB <- data.frame(
    "USUBJID" = 1 : 5,
    "SAFFL" = "Y",
    "AVAL" = rnorm(5),
    "AVAL2" = rnorm(5),
    "VISIT" = c("Day 1", "Day 1", "Day 2", "Day 2", "Day 1"),
    "PARAMCD" = c("ALS", "ALT", "ALT", "BILI", "BILI"),
    stringsAsFactors = FALSE
)

test_that("Correct extraction of custom annotation", {
      
      dataAnnot <- annotateData(dataLB, annotations = list(data = dataDM))
      
      newCols <- setdiff(colnames(dataAnnot), colnames(dataLB))
      expect_equivalent(
          object = dataDM[match(dataLB$USUBJID, dataDM$USUBJID), newCols],
          expected = dataAnnot[, newCols]
      )	
      
    })

test_that("Correct computation of new variable based on combination of multiple variables", {
      
      dataLB <- annotateData(
          dataLB, annotations = list(vars = "RATIO", varFct = "AVAL / AVAL2")
      )
      expect_equivalent(
          object = dataLB$RATIO,
          expected = with(dataLB, AVAL / AVAL2)
      )
      
    })

test_that("Annotation based on varFct'", {
      
      varFct <- "sub('Day .* - (.+)', '\\\\1', VISIT)"
      dataLB <- annotateData(dataLB, 
          annotations = list(
              vars = "PERIOD", 
              varFct = varFct
          )
      )
      
      expect_equivalent(
          object = dataLB$PERIOD,
          expected = eval(expr = parse(text = varFct), envir = dataLB)
      )
      
      expect_error(
          annotateData(
              dataDM,
              annotations = list(
                  varFct = 'sprintf("%s %s", AGE, YEAR)',
                  varLabel = "Age and year"
              )
          ),
          "'vars' should be specified"
      )
      dataAnnot <- annotateData(
          dataDM,
          annotations = list(
              vars = "AGEYEAR",
              varFct = 'sprintf("%s %s", AGE, YEAR)',
              varLabel = "Age and year"
          )
      )
      expect_message(
          annotateData(
              dataDM,
              annotations = list(
                  vars = "AGEYEAR",
                  varFct = 'sprintf("%s %s", AGE, YEAR)',
                  varLabel = "Age and year"
              ),
              verbose = TRUE
          )
      )
      # Annotation based on 'function(data)' as a string
      dataAnnotBis <- annotateData(
          dataDM,
          annotations = list(
              vars = "AGEYEAR",
              varFct = 'function(data) with(data, sprintf("%s %s", data$AGE, data$YEAR))',
              varLabel = "Age and year"
          )
      )
      expect_identical(dataAnnot, dataAnnotBis)
      # Annotation based on function(data)
      dataAnnotTris <- annotateData(
          dataDM,
          annotations = list(
              vars = "AGEYEAR",
              varFct = function(data) with(data, sprintf("%s %s", data$AGE, data$YEAR)),
              varLabel = "Age and year"
          )
      )
      expect_identical(dataAnnot, dataAnnotTris)
      
    })


test_that("Annotation based on demographics", {
      
      expect_error(
          annotateData(dataLB, annotations = "demographic"),
          "Data is not annotated, because 'annotations' should be one of"
      )
      
      expect_warning(
          annotateData(dataLB, annotations = "demographics"),
          "Data is not annotated with demographics data because no such data"
      )
      
      varsDM <- c("AGE", "SEX", "SITEID")
      dataLB_annot <- annotateData(
          dataLB,
          dataPath = system.file(
              "extdata", "cdiscpilot01", "SDTM", package = "clinUtils"
          ),
          annotations = "demographics"
      )
      expect_is(dataLB_annot, "data.frame")
      expect_true(any(varsDM %in% colnames(dataLB_annot)))
      
      expect_message(
          annotateData(
              dataLB,
              dataPath = system.file(
                  "extdata", "cdiscpilot01", "SDTM", package = "clinUtils"
              ),
              annotations = "demographics",
              verbose = TRUE
          )
      )
      
    })

test_that("Annotation based on functional_groups_lab", {
      
      dataLB_warning <- dataLB
      dataLB_warning$PARAMCD <- NULL
      expect_warning(
          annotateData(dataLB_warning, annotations = "functional_groups_lab"),
          "Data is not annotated with functional groups, because no variable"
      )
      dataNoAnnot <- annotateData(dataLB_warning, annotations = "functional_groups_lab")
      expect_identical(dataNoAnnot, dataLB_warning)
      
      dataLB_annot <- annotateData(
          dataLB,
          annotations = "functional_groups_lab"
      )
      expect_is(dataLB_annot, "data.frame")
      expect_identical(
          colnames(dataLB_annot),
          c(colnames(dataLB), "LBFCTGRP")
      )
      expect_is(dataLB_annot$LBFCTGRP, "factor")
      expect_identical(
          levels(dataLB_annot$LBFCTGRP),
          c("Liver function", "Other")
      )
      expect_message(
          annotateData(
              dataLB,
              annotations = "functional_groups_lab",
              verbose = TRUE
          )
      )
      
    })

test_that("Annotation with 'dataset' custom annotation", {
      
      expect_warning(
          annotateData(
              dataLB,
              dataPath = system.file(
                  "extdata", "cdiscpilot01", "SDTM", package = "clinUtils"
              ),
              annotations = list(dataset = "dm")
          ),
          "Data is not annotated with variable"
      )
      dataAnnot <- annotateData(
          dataLB,
          dataPath = system.file(
              "extdata", "cdiscpilot01", "SDTM",package = "clinUtils"
          ),
          annotations = list(dataset = "dm")
      )
      expect_is(dataAnnot, "data.frame")
      
      dataAnnotVars <- annotateData(
          dataLB,
          dataPath = system.file(
              "extdata", "cdiscpilot01", "SDTM", package = "clinUtils"
          ),
          annotations = list(
              dataset = "dm",
              vars = "AGE"
          )
      )
      expect_identical(
          colnames(dataAnnotVars),
          c(colnames(dataLB), "AGE")
      )
      expect_is(dataAnnotVars$AGE, "numeric")
      
      # Variable already present should not be added
      dataAnnotVars <- annotateData(
          dataLB,
          dataPath = system.file(
              "extdata", "cdiscpilot01", "SDTM",package = "clinUtils"
          ),
          annotations = list(
              dataset = "dm",
              vars = "USUBJID"
          )
      )
      expect_identical(dataAnnotVars, dataLB)
      
    })

test_that("Filtering of data", {
      
      dataAnnotFilter <- annotateData(
          dataLB,
          annotations = list(
              data = dataDM,
              filters = list(var = "SEX", value = "M")
          )
      )
      expect_is(dataAnnotFilter, "data.frame")
      # Test on length...?
      
    })

test_that("Nested annotations", {
      
      dataAnnot <- annotateData(
          dataDM,
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
      expect_is(dataAnnot, "data.frame")
      extraVar <- with(dataDM, sprintf("%s %s", AGE, YEAR))
      expect_identical(dataAnnot$AGESTRING, extraVar)
      
    })

test_that("Warning with exposure data", {
      
      expect_warning(
          annotateData(
              data = dataLB,
              dataPath = "path/To/Data",
              annotations = "exposed_subjects"
          ),
          "Data is not annotated with exposure data, because no such data"
      )
      
    })

test_that("Annotate with exposure data", {
      
      expect_silent(
          dataEx <- annotateData(
              data = dataLB,
              dataPath = testPathData,
              annotations = "exposed_subjects"
          )
      )
      expect_s3_class(dataEx, "data.frame")
      expect_equal(ncol(dataEx), ncol(dataLB) + 1)
      expect_true("EXFL" %in% colnames(dataEx))     
      
    })

test_that("Stop annotate with exposure data because no subjectVar", {
      
      expect_error(
          annotateData(
              data = dataLB,
              dataPath = testPathData,
              annotations = "exposed_subjects",
              subjectVar = "LBTESTCD"
          ),
          "Data is not annotated with exposure data because doesn't contain variable"
      )
      
    })

test_that("Verbose annotation with exposure data", {
      
      expect_message(
          dataEx <- annotateData(
              data = dataLB,
              dataPath = testPathData,
              annotations = "exposed_subjects",
              verbose = TRUE
          ),
          "Data is annotated with exposed subjects"
      )
      
    })
