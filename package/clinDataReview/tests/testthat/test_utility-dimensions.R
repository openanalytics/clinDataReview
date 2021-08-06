test_that("Width and height, if specified, are respected", {
			
	res <- clinDataReview:::getSizePlot(
		width = 300,
		height = 100
	)
	expect_is(res, "numeric")
	expect_length(res, 2)
	expect_named(res)
	expect_equal(res[["height"]], 100)
	expect_equal(res[["width"]], 300)
	
})

test_that("Height of the plot is increased if a title is specified", {
			
	expect_gt(
		object = clinDataReview:::getSizePlot(title = "This is a title.")[["height"]], 
		expected = clinDataReview:::getSizePlot()[["height"]]
	)	
			
})

test_that("Height of the plot is increased if a subtitle is specified", {
			
	expect_gt(
		object = clinDataReview:::getSizePlot(subtitle = "This is a subtitle.")[["height"]], 
		expected = clinDataReview:::getSizePlot()[["height"]]
	)	
			
})

test_that("Height of the plot is increased if a label for the x-axis is specified", {
			
	expect_gt(
		object = clinDataReview:::getSizePlot(xLab = "This is a label for the x-axis.")[["height"]], 
		expected = clinDataReview:::getSizePlot()[["height"]]
	)	
			
})

test_that("Height of the plot is increased if a caption is specified", {
			
	expect_gt(
		object = clinDataReview:::getSizePlot(caption = "This is a caption.")[["height"]], 
		expected = clinDataReview:::getSizePlot()[["height"]]
	)	
			
})

test_that("Height of the plot is increased if a legend is specified at the top", {
			
	sizeWithLegend <- clinDataReview:::getSizePlot(
		includeLegend = TRUE, 
		legendPosition = "top"
	)
	sizeWithoutLegend <- clinDataReview:::getSizePlot(
		includeLegend = FALSE
	)
	expect_gt(sizeWithLegend[["height"]], sizeWithoutLegend[["height"]])	
		
})

test_that("Height of the plot is increased if a legend is specified at the bottom", {
			
	sizeWithLegend <- clinDataReview:::getSizePlot(
		includeLegend = TRUE, 
		legendPosition = "bottom"
	)
	sizeWithoutLegend <- clinDataReview:::getSizePlot(
		includeLegend = FALSE
	)
	expect_gt(sizeWithLegend[["height"]], sizeWithoutLegend[["height"]])	
			
})

test_that("Height of the plot is increased if the plot contained multiple rows", {
			
	size4Rows <- clinDataReview:::getSizePlot(nrow = 4)
	size3Rows <- clinDataReview:::getSizePlot(nrow = 3)
	expect_gt(size4Rows[["height"]], size3Rows[["height"]])	
			
})

test_that("Height of the plot is takes into account the number of text elements in the y-axis", {
			
	size2Y <- clinDataReview:::getSizePlot(y = c("a", "b"))
	size1Y <- clinDataReview:::getSizePlot(y = "a")
	expect_gt(
		object = size2Y[["height"]], 
		expected = size1Y[["height"]]
	)
			
})

test_that("Height of the plot is takes into account the number of lines for text elements in the y-axis", {
			
	size2Lines <- clinDataReview:::getSizePlot(y = "a\nb")
	size1Line <- clinDataReview:::getSizePlot(y = "a")
	expect_gt(
		object = size2Lines[["height"]], 
		expected = size1Line[["height"]]
	)
			
})

test_that("A top legend is positioned below the subtitle", {
			
	subtitle <- "Plot subtitle"
	sizeDetails <- clinDataReview:::getPositionAndMargins(
		subtitle = subtitle,
		includeLegend = TRUE, legendPosition = "top"
	)
	expect_lt(
		object = sizeDetails$position$legend,
		expected = sizeDetails$position$subtitle
	)
			
})

test_that("The label for the x-axis, bottom legend and caption are positioned in this order at the bottom of the plot", {
			
	xLab <- "Label for the x-axis"
	caption <- "This is a caption"
	sizeDetails <- clinDataReview:::getPositionAndMargins(
		xLab = xLab,
		caption = caption,
		includeLegend = TRUE, legendPosition = "bottom"
	)
	expect_lt(
		object = sizeDetails$position$xLab,
		expected = sizeDetails$position$legend
	)
	expect_lt(
		object = sizeDetails$position$legend,
		expected = sizeDetails$position$caption
	)
			
})