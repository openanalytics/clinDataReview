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

