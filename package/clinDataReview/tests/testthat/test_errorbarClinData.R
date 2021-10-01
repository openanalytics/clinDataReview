context("Visualize clinical data with error bars")

library(plotly)

test_that("The data is correctly displayed in vertical error bars", {
			
	data <- data.frame(
		AVISIT = c("Baseline", "Week 2"),
		Mean = c(25.6, 40),
		SE = c(2, 3),
		stringsAsFactors = FALSE
	)
			
	pl <- errorbarClinData(
		data = data,
		xVar = "AVISIT", 
		yVar = "Mean", 
		yErrorVar = "SE"
	)
	
	expect_s3_class(pl, "plotly")
	
	## check if input == output data
			
	# extract data from output object
	plData <- plotly_build(pl)$x$data
			
	# only 'scatter' aes
	plDataScatter <- plData[sapply(plData, function(x) x$type == "scatter")]
	
	plDataScatterDf <- do.call(rbind,
		lapply(plDataScatter, function(x) 
			data.frame(
				x = as.character(x[["x"]]), 
				y = x[["y"]], 
				yError = x[["error_y"]]$array)
		)
	)
	
	expect_equivalent(
		object = plDataScatterDf, 
		expected = data[, c("AVISIT", "Mean", "SE")]
	)
			
})

test_that("The data is correctly displayed in horizontal error bars", {
			
	data <- data.frame(
		AVISIT = c("Baseline", "Week 2"),
		Mean = c(25.6, 40),
		SE = c(2, 3),
		stringsAsFactors = FALSE
	)
			
	pl <- errorbarClinData(
		data = data,
		yVar = "AVISIT", 
		xVar = "Mean", 
		xErrorVar = "SE"
	)
			
	expect_s3_class(pl, "plotly")
			
	## check if input == output data
			
	# extract data from output object
	plData <- plotly_build(pl)$x$data
			
	# only 'scatter' aes
	plDataScatter <- plData[sapply(plData, function(x) x$type == "scatter")]
			
	plDataScatterDf <- do.call(rbind,
		lapply(plDataScatter, function(x) 
			data.frame(
				x = x[["x"]], 
				xError = x[["error_x"]]$array,
				y = as.character(x[["y"]]),
				stringsAsFactors = FALSE
			)
		)
	)
			
	expect_equal(
		object = plDataScatterDf, 
		expected = data[, c("Mean", "SE", "AVISIT")],
		check.attributes = FALSE
	)
			
})

test_that("Vertical error bars are correctly colored based on a specified variable", {
			
	data <- data.frame(
		AVISIT = factor(
			c("Baseline", "Screening", "Baseline", "Screening"),
			levels = c("Screening", "Baseline")
		),
		Mean = c(25.6, 40, 12, 5),
		SE = c(2, 3, 1, 2),
		TRT = c("A", "A", "B", "B"),
		stringsAsFactors = FALSE
	)
			
	pl <- errorbarClinData(
		data = data,
		xVar = "AVISIT", 
		yVar = "Mean", 
		yErrorVar = "SE",
		colorVar = "TRT"
	)
			
	# extract data from output object
	plData <- plotly_build(pl)$x$data
			
	# only 'scatter' aes
	plData <- plData[sapply(plData, function(x) x$type == "scatter")]
			
	plData <- do.call(rbind,
		lapply(plData, function(x) 
			data.frame(
				# Note: error-bars are jittered by the color variable
				# so x-coordinate is the jittered coordinate
				x = round(x[["x"]], 0),
				y = x[["y"]], 
				yError = x[["error_y"]]$array,
				color = as.character(x[["error_y"]]$color),
				group = x[["name"]],
				stringsAsFactors = FALSE
			)
		)
	)
	
	## check that different colors are set by group
	colors <- with(plData, tapply(color, group, unique))
	expect_type(colors, "character")
	expect_length(colors, 2)
	
	## check if input == output data
			
	plData <- plData[, c("x", "y", "yError", "group")]
	
	dataReference <- data[, c("AVISIT", "Mean", "SE", "TRT")]
	# x-variable is the jittered version of AVISIT
	dataReference$AVISIT <- as.numeric(as.factor(dataReference$AVISIT))
	
	expect_equivalent(
		object = plData[do.call(order, plData), ], 
		expected = dataReference[do.call(order, dataReference), ]
	)
	
	## check that labels are set in correct order in the x-axis
	plXAxis <- plotly_build(pl)$x$layout$xaxis
	# extract tick labels
	plXTickLab <- plXAxis$ticktext
	# and sort them
	plXTickLab <- plXTickLab[order(plXAxis$tickvals, decreasing = FALSE)]
	
	expect_equal(object = unname(plXTickLab), c("Screening", "Baseline"))
			
})

test_that("Horizontal error bars are correctly colored based on a specified variable", {
			
	data <- data.frame(
		AVISIT = factor(
			c("Baseline", "Screening", "Baseline", "Screening"),
			levels = c("Screening", "Baseline")
		),
		Mean = c(25.6, 40, 12, 5),
		SE = c(2, 3, 1, 2),
		TRT = c("A", "A", "B", "B"),
		stringsAsFactors = FALSE
	)
			
	pl <- errorbarClinData(
		data = data,
		yVar = "AVISIT", 
		xVar = "Mean", 
		xErrorVar = "SE",
		colorVar = "TRT"
	)
			
	# extract data from output object
	plData <- plotly_build(pl)$x$data
			
	# only 'scatter' aes
	plData <- plData[sapply(plData, function(x) x$type == "scatter")]
			
	plData <- do.call(rbind,
		lapply(plData, function(x) 
			data.frame(
				# Note: error-bars are jittered by the color variable
				# so y-coordinate is the jittered coordinate
				y = round(x[["y"]], 0),
				x = x[["x"]], 
				xError = x[["error_x"]]$array,
				color = as.character(x[["error_x"]]$color),
				group = x[["name"]],
				stringsAsFactors = FALSE
			)
		)
	)
			
	## check that different colors are set by group
	colors <- with(plData, tapply(color, group, unique))
	expect_type(colors, "character")
	expect_length(colors, 2)
			
	## check if input == output data
	
	plData <- plData[, c("y", "x", "xError", "group")]
			
	dataReference <- data[, c("AVISIT", "Mean", "SE", "TRT")]
	# y-variable is the jittered version of AVISIT
	# in reverse order (first level are on top with highest y)
	dataReference$AVISIT <- as.numeric(as.factor(dataReference$AVISIT))
	dataReference$AVISIT <- max(dataReference$AVISIT)-dataReference$AVISIT+1
			
	expect_equivalent(
		object = plData[do.call(order, plData), ], 
		expected = dataReference[do.call(order, dataReference), ]
	)
			
	## check that labels are set in correct order in the x-axis
	plYAxis <- plotly_build(pl)$x$layout$yaxis
	# extract tick labels
	plYTickLab <- plYAxis$ticktext
	# and sort them from the top (higher y) to the bottom (lowest y)
	plYTickLab <- plYTickLab[order(plYAxis$tickvals, decreasing = TRUE)]
			
	expect_equal(object = unname(plYTickLab), c("Screening", "Baseline"))
			
})

test_that("A color palette is correctly set in the errorbar visualization", {
			
	data <- data.frame(
		AVISIT = c("Baseline", "Week 2", "Baseline", "Week 2"),
		Mean = c(25.6, 40, 12, 5),
		SE = c(2, 3, 1, 2),
		TRT = c("A", "A", "B", "B"),
		stringsAsFactors = FALSE
	)
			
	colorPalette <- c(B = "blue", A = "red")
	pl <- errorbarClinData(
		data = data,
		xVar = "AVISIT", 
		yVar = "Mean", 
		yErrorVar = "SE",
		colorVar = "TRT", colorPalette = colorPalette
	)
	
	plData <- plotly_build(pl)$x$data
	
	# only 'scatter' aes
	plData <- plData[sapply(plData, function(x) x$type == "scatter")]
	
	plColorPalette <- do.call(c,
		lapply(plData, function(x) 
			setNames(
				as.character(x[["error_y"]]$color),
				x[["name"]]
			)
		)
	)
	
	# plotly specifies color in rgba
	colorPaletteRGB <- col2rgb(colorPalette)
	colorPaletteRGBA <- paste0(
		"rgba(", 
		apply(colorPaletteRGB, 2, paste, collapse = ","), 
		",1)" # + alpha
	)
	names(colorPaletteRGBA) <- names(colorPalette)
	
	expect_mapequal(object = plColorPalette, expected = colorPaletteRGBA)
	
})

test_that("An interactive table is correctly included in the errorbar visualization", {
			
	data <- data.frame(
		AVISIT = c("Baseline", "Week 2"),
		Mean = c(25.6, 40),
		SE = c(2, 3),
		stringsAsFactors = FALSE
	)
			
	res <- errorbarClinData(
		data = data,
		xVar = "AVISIT", 
		yVar = "Mean", 
		yErrorVar = "SE",
		table = TRUE
	)
			
	expect_s3_class(res$table, "datatables")
	dataTable <- res$table$x$data[, colnames(data)]
	dataTable$AVISIT <- as.character(dataTable$AVISIT)
	expect_equal(
		object = dataTable,
		expected = data[, colnames(data)]
	)
			
})

test_that("Specified variables for the interactive table are correctly included in the errorbar visualization", {
	
	data <- data.frame(
		AVISIT = c("Baseline", "Week 2"),
		Mean = c(25.6, 40),
		SE = c(2, 3),
		TRT = "A",
		stringsAsFactors = FALSE
	)
			
	tableVars <- c("TRT", "AVISIT", "Mean")
	res <- errorbarClinData(
		data = data,
		xVar = "AVISIT", 
		yVar = "Mean", 
		yErrorVar = "SE",
		table = TRUE,
		tableVars = tableVars
	)
			
	expect_s3_class(res$table, "datatables")
	dataTable <- res$table$x$data[, tableVars]
	dataTable[, c("TRT", "AVISIT")] <- lapply(dataTable[, c("TRT", "AVISIT")], as.character)
	expect_identical(
		object = dataTable,
		expected = data[, tableVars]
	)
			
})

test_that("Labels are correctly extracted for the hover", {
			
	data <- data.frame(
		AVISIT = "Baseline",
		Mean = 25.6,
		SE = 2,
		stringsAsFactors = FALSE
	)
	labelVars <- c(
		AVISIT = "Actual Visit", 
		"Mean" = "Observed Mean",
		SE = "Standard Error"
	)
	pl <- errorbarClinData(
		data = data,
		xVar = "AVISIT", 
		yVar = "Mean", 
		yErrorVar = "SE",
		labelVars = labelVars
	)
	plData <- plotly_build(pl)$x$data
	
	# only 'scatter' aes
	plData <- plData[sapply(plData, function(x) x$type == "scatter")]
	
	plData <- do.call(rbind,
		lapply(plData, function(x) 
			data.frame(
				x = x[["x"]],
				y = x[["y"]], 
				yError = x[["error_y"]]$array,
				hover = x[["hovertemplate"]],
				stringsAsFactors = FALSE
			)
		)
	)
	
	expect_match(
		object = subset(plData, x == "Baseline" & y == 25.6)$hover,
		regexp = "Actual Visit: Baseline.+Observed Mean: 25.6.+Standard Error: 2"
	)
	
})

test_that("Labels for the x-axis are correctly set from variables", {
			
	# example where some levels of the factor x-variable are not used
	dataPlot <- data.frame(
		AVISIT = factor(
			c("Week 2", "Screening"),
			levels = c("Screening", "Baseline", "Week 2")
		),
		Mean = c(12, 15),
		SE = c(1, 2),
		n = c("N = 3", "N = 5"),
		stringsAsFactors = FALSE
	)
	pl <- errorbarClinData(
		data = dataPlot, 
		xVar = "AVISIT", xLabVars = c("AVISIT", "n"),
		yVar = "Mean", yErrorVar = "SE"
	)
	plXAxis <- plotly_build(pl)$x$layout$xaxis
	
	# all visits - factor levels are included (even if no data)
	expect_equal(plXAxis$tickvals, c(1, 2))
	
	# extract tick labels
	plXTickLab <- plXAxis$ticktext
	# and sort them
	plXTickLab <- plXTickLab[order(plXAxis$tickvals, decreasing = FALSE)]
			
	expect_match(object = plXTickLab[1], regexp = "Screening.+N = 5")
	expect_match(object = plXTickLab[2], regexp = "Week 2.+N = 3")
			
})

test_that("Symbols correctly set based on a specified variable", {
			
	data <- data.frame(
		AVISIT = factor(
			c("Baseline", "Screening", "Baseline", "Screening"),
			levels = c("Screening", "Baseline")
		),
		Mean = c(25.6, 40, 12, 5),
		SE = c(2, 3, 1, 2),
		TRT = c("A", "A", "B", "B"),
		stringsAsFactors = FALSE
	)
			
	pl <- errorbarClinData(
		data = data,
		xVar = "AVISIT", 
		yVar = "Mean", 
		yErrorVar = "SE",
		shapeVar = "TRT"
	)
			
	# extract data from output object
	plData <- plotly_build(pl)$x$data
			
	# only 'scatter' aes
	plData <- plData[sapply(plData, function(x) x$type == "scatter")]
			
	plData <- do.call(rbind,
		lapply(plData, function(x) 
			data.frame(
				# Note: error-bars are jittered by the color variable
				# so x-coordinate is the jittered coordinate
				x = x[["x"]],
				y = x[["y"]], 
				yError = x[["error_y"]]$array,
				shape = as.character(x[["marker"]]$symbol),
				group = x[["name"]],
				stringsAsFactors = FALSE
			)
		)
	)
			
	## check that different shapes are set by group
	shapes <- with(plData, tapply(shape, group, unique))
	expect_type(shapes, "character")
	expect_length(shapes, 2)
			
	## check if input == output data
	
	plData <- plData[, c("x", "y", "yError", "group")]
	dataReference <- data[, c("AVISIT", "Mean", "SE", "TRT")]
	expect_equivalent(
		object = plData[do.call(order, plData), ], 
		expected = dataReference[do.call(order, dataReference), ]
	)
			
})

test_that("The points are correctly shaped with a specified palette", {
			
	data <- data.frame(
		AVISIT = c("Baseline", "Week 2", "Baseline", "Week 2"),
		Mean = c(25.6, 40, 12, 5),
		SE = c(2, 3, 1, 2),
		TRT = c("A", "A", "B", "B"),
		stringsAsFactors = FALSE
	)
			
	shapePalette <- c(B = "diamond", A = "cross")
	pl <- errorbarClinData(
		data = data,
		xVar = "AVISIT", 
		yVar = "Mean", 
		yErrorVar = "SE",
		shapeVar = "TRT", shapePalette = shapePalette
	)
			
	plData <- plotly_build(pl)$x$data
			
	# only 'scatter' aes
	plData <- plData[sapply(plData, function(x) x$type == "scatter")]
			
	plShapePalette <- do.call(c,
		lapply(plData, function(x) 
			setNames(
				as.character(x[["marker"]]$symbol),
				x[["name"]]
			)
		)
	)
			
	expect_mapequal(object = plShapePalette, expected = shapePalette)
			
})
