context("data annotation")

library(glpgUtilityFct)

data(SDTMDataPelican)

dataLB <- SDTMDataPelican$LB
dataDM <- SDTMDataPelican$DM
dataEX <- SDTMDataPelican$EX

test_that("Correct extraction of custom annotation", {
      
      dataAnnot <- annotateData(dataLB, annotations = list(data = dataDM))
      
      newCols <- setdiff(colnames(dataAnnot), colnames(dataLB))
      expect_equivalent(
          object = dataDM[match(dataLB$USUBJID, dataDM$USUBJID), newCols],
          expected = dataAnnot[, newCols]
      )	
      
    })

test_that("Correct computation of new variable based on combination of multiple variables", {
      
      dataLB <- annotateData(dataLB, annotations = list(vars = "ULN_ratio", varFct = "LBSTRESN / LBSTNRHI"))
      expect_equivalent(
          object = dataLB$`ULN_ratio`,
          expected = with(dataLB, LBSTRESN / LBSTNRHI)
      )
      
    })

test_that("Annotation based on text extraction with 'sub'", {
      
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
      
    })


test_that("Annotation based on demographics", {
      
      expect_error(
          annotateData(dataLB, annotations = "demographic")
      )
      
      expect_warning(
          annotateData(dataLB, annotations = "demographics")
      )
      
      varsDM <- c("AGE", "SEX", "RACE", "COUNTRY", "SITEID")
      dataLB_annot <- annotateData(
          dataLB,
          dataPath = system.file("extdata", package = "glpgUtilityFct"),
          annotations = "demographics"
      )
      expect_is(dataLB_annot, "data.frame")
      expect_true(any(varsDM %in% colnames(dataLB_annot)))
      
      expect_message(
          annotateData(
              dataLB,
              dataPath = system.file("extdata", package = "glpgUtilityFct"),
              annotations = "demographics",
              verbose = TRUE
          )
      )
      
    })

test_that("Annotation based on functional_groups_lab", {
      
      dataLB_warning <- dataLB
      dataLB_warning$PARAMCD <- dataLB_warning$LBTESTCD <- NULL
#      expect_warning(
#          annotateData(dataLB_warning, annotations = "functional_groups_lab")
#      )
      
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
          c("Renal function", "Electrolytes",
              "Liver function", "Lipids",
              "Haematology", "Other")
      )
      expect_message(
          annotateData(
              dataLB,
              annotations = "functional_groups_lab",
              verbose = TRUE
          )
      )
      
    })