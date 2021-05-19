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

test_that("Test nested list for 'transformData'", {
      
      testData <- data.frame(
          USUBJID = rep(1 : 10, each = 4),
          PARAMGR1 = rep(c("param1", "param2"), length.out = 20),
          PARAMGR2 = rep(c("param3", "param4"), length.out = 20),
          VALUEGR1 = seq(1, 10, length.out = 20)
      )
      
      transformation <- list(
          list(
              type = "pivot_wider",
              varsID = "USUBJID",
              varPivot = "PARAMGR1",
              varsValue = "VALUEGR1"
          ),
          list(
              type = "pivot_wider",
              varsID = "USUBJID",
              varPivot = "PARAMGR2",
              varsValue = "VALUEGR1"
          )
      )
      expect_warning(transformData(data = testData, transformations = transformation))
      dataTransform <- transformData(data = testData, transformations = transformation)
      expect_is(dataTransform, "data.frame")
      
    })