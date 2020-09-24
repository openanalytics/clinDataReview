context("Test miscellaneous functions")

test_that("Formatting of hover text", {
      
      x <- LETTERS
      myLabel <- "myLabel"
      
      formatText <- formatHoverText(x = LETTERS, label = "myLabel")
      myLabelFormat <- sprintf("%s: %s", myLabel, paste(LETTERS, collapse = ", "))
      
      expect_is(formatText, "character")
      expect_identical(names(formatText), myLabelFormat)
      
      # Change default width
      formatTextShort <- formatHoverText(x = LETTERS, label = "myLabel", width = 3)
      expect_is(formatTextShort, "character")
      expect_identical(names(formatText), names(formatTextShort))
      
      textFormat <- sprintf("%s:<br>%s", myLabel, paste(LETTERS, collapse = ",<br>"))
      names(textFormat) <- myLabelFormat
      expect_identical(textFormat, formatTextShort)
      
    })

test_that("Getting formula for variable", {
      
      # One variable
      varFormula <- varToFm("CHG")
      expect_is(varFormula, "formula")
      expect_equal(as.formula("~CHG"), varFormula)
      
      # Two variables
      varsFormula <- varToFm(c("AVAL", "CHG"))
      expect_is(varsFormula, "formula")
      expect_equal(as.formula("~AVAL + CHG"), varsFormula)
      
    })

test_that("Get JS dependencies", {
      
      dependencies <- getJsDepMedicalMonitoring()
      expect_is(dependencies, "list")
      expect_length(dependencies, 5)
      matrixRes <- sapply(dependencies, expect_length, 10)
      
      dependencyOne <- getJsDepMedicalMonitoring("FileSaver")
      expect_is(dependencyOne, "list")
      expect_length(dependencyOne[[1]], 10)
      expect_is(dependencyOne[[1]], "html_dependency")
      
    })