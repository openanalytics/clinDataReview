context("Test 'transformData' function")


test_that("Test errors of 'transformData' function", {
      
      expect_error(transformData(data))
      
      transformation <- list(type = NULL)
      expect_error(transformData(data, transformations = transformation))
      
      transformation <- list(type = "transform")
      expect_error(transformData(data, transformations = transformation))
      
      transformation <- list(type = "pivot_wider")
      expect_error(transformData(data, transformations = transformation))
      
      transformation <- list(type = "pivot_wider", varsID = "TRTP")
      expect_error(transformData(data, transformations = transformation))
      
    })

test_that("Test 'transformData' function", {
      
      testData <- data.frame(
          USUBJID = rep(1 : 10, each = 2),
          PARAM = rep(c("param1", "param2"), length.out = 20),
          VALUE = seq(1, 10, length.out = 20)
      )
            
      transformation <- list(
          type = "pivot_wider",
          varsID = "USUBJID",
          varPivot = "PARAM",
          varsValue = "VALUE"
      )
      dataTransform <- transformData(data = testData, transformations = transformation)
      expect_is(dataTransform, "data.frame")
      expect_identical(dataTransform$USUBJID, 1 : 10)
      expect_is(attributes(dataTransform), "list")
      attribDataTransf <- attr(dataTransform, "labelVars")
      expect_is(attribDataTransf, "character")
      expect_length(attribDataTransf, 2)
      
      # Test message
      expect_message(
          transformData(
              data = testData,
              transformations = transformation, verbose = TRUE
          )
      )
      
      # Test labels
      labels <- setNames(
          c("Subject ID", "Parameter", "Value"), nm = colnames(testData)
      )
      dataTransform <- transformData(
          data = testData,
          transformations = transformation,
          labelVars = labels
      )
      attribDataTransf <- attr(dataTransform, "labelVars")
      expect_is(attribDataTransf, "character")
      expect_length(attribDataTransf, 5)
            
    })