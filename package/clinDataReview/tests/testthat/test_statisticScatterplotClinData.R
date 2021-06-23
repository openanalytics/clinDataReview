context("Test static scatter plot")

# Seems from 'coverage' report that
# 'staticScatterplotClinData' is also tested in the scatterplotClinData
# So other tests are skipped


data <- data.frame(
    A = c(1, 1, 2, 3),
    B = c(2, 4, 1, 3),
    C = c("trt1", "trt1", "trt2", "trt2"),
    stringsAsFactors = FALSE
)
labelVars <- c(A = "var1", B = "var2", C = "colorVar")

test_that("Missing aesLab without labels", {
      
      expect_silent(
          gPlot <- clinDataReview:::staticScatterplotClinData(
              data = data,
              xVar = "A", yVar = "B",
              aesPointVar = list(color = "C")
          )
      )
      expect_is(gPlot, "ggplot")
      expect_equal(class(gPlot$labels), "list")
      expect_equal(gPlot$labels$x, "A")
      expect_equal(gPlot$labels$y, "B")
      expect_equal(gPlot$labels$colour, "C")
      
    })

test_that("Missing aesLab with labels", {
      
      expect_silent(
          gPlot <- clinDataReview:::staticScatterplotClinData(
              data = data,
              xVar = "A", yVar = "B",
              aesPointVar = list(color = "C"),
              labelVars = labelVars
          )
      )
      expect_is(gPlot, "ggplot")
      expect_equal(class(gPlot$labels), "list")
      expect_equal(gPlot$labels$x, "var1")
      expect_equal(gPlot$labels$y, "var2")
      expect_equal(gPlot$labels$colour, "colorVar")
      
    })

test_that("Warning of transformation of axis", {
      
      expect_warning(
          clinDataReview:::staticScatterplotClinData(
              data = data,
              xVar = "A", yVar = "B",
              aesPointVar = list(color = "C"),
              labelVars = labelVars,
              xTrans = "log",
              xPars = list(trans = "log")
          ),
          "'trans' in parameters for x axis are ignored"
      )
      
    })





