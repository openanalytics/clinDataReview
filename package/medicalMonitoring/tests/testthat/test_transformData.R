context("Test 'transformData' function")

library(glpgUtilityFct)
data(ADaMDataPelican)
data(labelVarsADaMPelican)

dataAll <- ADaMDataPelican
labelVars <- labelVarsADaMPelican

data <- dataAll[[1]]

test_that("Test errors of 'transformData' function", {
      
      expect_error(transformData(data))
      
      transformation <- list(type = NULL)
      expect_error(transformData(data, transformation = transformation))
      
      transformation <- list(type = "transform")
      expect_error(transformData(data, transformation = transformation))
      
      transformation <- list(type = "pivot_wider")
      expect_error(transformData(data, transformation = transformation))
      
      transformation <- list(type = "pivot_wider", varsID = "TRTP")
      expect_error(transformData(data, transformation = transformation))
      
    })

test_that("Test 'transformData' function", {
      
#      transformation <- list(
#          type = "pivot_wider",
#          varsID = "USUBJID",
#          varPivot = "USUBJID",
#          varsValue = "TRTP"
#      )
#      transformData(data, transformation = transformation)
      
    })