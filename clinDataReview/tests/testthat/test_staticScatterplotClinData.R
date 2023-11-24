context("Visualize clinical data with a static scatterplot")

library(ggplot2)
library(scales)

# 'staticScatterplotClinData' is also tested in the scatterplotClinData
# So other tests are skipped

test_that("The axis and color labels are correctly set to the variable names by default in the scatterplot", {

	data <- data.frame(
		A = c(1, 1, 2, 3),
		B = c(2, 4, 1, 3),
		C = c("trt1", "trt1", "trt2", "trt2"),
		stringsAsFactors = FALSE
	)
	gPlot <- clinDataReview:::staticScatterplotClinData(
		data = data,
		xVar = "A", yVar = "B",
		aesPointVar = list(color = "C")
	)
	expect_s3_class(gPlot, "ggplot")
	expect_type(gPlot$labels, "list")
	expect_equal(object = gPlot$labels$x, expected = "A")
	expect_equal(object = gPlot$labels$y, expected = "B")
	expect_equal(object = gPlot$labels$colour, expected = "C")
      
})

test_that("The axis and color labels are correctly extracted from the labels of all variables in the scatterplot", {
		
	data <- data.frame(
		A = c(1, 1, 2, 3),
		B = c(2, 4, 1, 3),
		C = c("trt1", "trt1", "trt2", "trt2"),
		stringsAsFactors = FALSE
	)
	labelVars <- c(A = "var1", B = "var2", C = "colorVar")
      
	gPlot <- clinDataReview:::staticScatterplotClinData(
		data = data,
		xVar = "A", yVar = "B",
		aesPointVar = list(color = "C"),
		labelVars = labelVars
	)
	expect_s3_class(gPlot, "ggplot")
	expect_type(gPlot$labels, "list")
	expect_equal(object = gPlot$labels$x, expected = "var1")
	expect_equal(object = gPlot$labels$y, expected =  "var2")
	expect_equal(object = gPlot$labels$colour, expected =  "colorVar")
      
})

test_that("A warning is generated if an axis transformation is specified both in the x-axis transformation and general parameters in the scatterplot", {
			
	data <- data.frame(
		A = c(1, 1, 2, 3),
		B = c(2, 4, 1, 3),
		C = c("trt1", "trt1", "trt2", "trt2")
	)
	expect_warning(
		clinDataReview:::staticScatterplotClinData(
			data = data,
			xVar = "A", yVar = "B",
			aesPointVar = list(color = "C"),
			xTrans = "log",
			xPars = list(trans = "log")
		),
		"'trans' in parameters for x axis are ignored"
	)
      
})

test_that("An axis transformation can be specified as a non character in the scatterplot", {
  
  data <- data.frame(
    A = c(1, 1, 2, 3),
    B = c(2, 4, 1, 3),
    C = c("trt1", "trt1", "trt2", "trt2")
  )
  xTrans <- scales::log10_trans()
  expect_silent(
    gg <- clinDataReview:::staticScatterplotClinData(
      data = data,
      xVar = "A", yVar = "B",
      xTrans = xTrans
    )
  )
  expect_equal(
    object = ggplot2::layer_scales(gg)$x$trans,
    expected = xTrans
  )
  
})
