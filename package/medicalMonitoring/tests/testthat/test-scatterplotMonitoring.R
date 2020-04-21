context("Scatterplot monitoring")

# load example data
library(glpgUtilityFct)
data(SDTMDataPelican)
data(labelVarsSDTMPelican)
dataLB <- SDTMDataPelican$LB
labelVars <- labelVarsSDTMPelican

## create input data

# add baseline as extra column:
dataPlot <- subset(dataLB, LBTESTCD == "ALT")
dataPlotBL <- subset(dataPlot, VISIT == "Screening (D-28 to D-1)")
dataPlotBL <- dataPlotBL[with(dataPlotBL, order(USUBJID, -LBDY)), ]
dataPlotBL <- dataPlotBL[!duplicated(dataPlotBL$USUBJID), ]
dataPlot$LBSTRESNBL <- dataPlot[match(dataPlot$USUBJID, dataPlotBL$USUBJID), "LBSTRESN"]

# sort visits:
dataPlot$VISIT <- with(dataPlot, reorder(VISIT, VISITNUM))

library(plotly) # for plotly_build

test_that("plotting function aesthetic testing", {
	
	pl <- scatterplotMonitoring(
		data = dataPlot, 
		xVar = "LBSTRESNBL", xLab = paste(labelVars["LBSTRESN"], "for last screening visit"),
		yVar = "LBSTRESN", yLab = paste(labelVars["LBSTRESN"], "at visit X"),
		aesPointVar = list( 
			color = "USUBJID", fill = "USUBJID",
			group = "USUBJID", stroke = "USUBJID",
			shape = "USUBJID", 
			alpha = "LBSTRESNBL", size = "LBSTRESNBL"
		),
		aesLineVar = list(
			group = "USUBJID", #color = "USUBJID", 
			alpha = "LBSTRESNBL",
			linetype = "USUBJID", 
			size = "LBSTRESNBL"
		),
		idVar = "USUBJID"
	)
	
	## check input == output data
	
	# extract input data
	cols <- c("LBSTRESNBL", "LBSTRESN", "USUBJID")
	dataPointLine <- dataPlot[, cols]
	dataPointLine <- dataPointLine[do.call(order, dataPointLine), ]
	
	# check data points
	plData <- plotly_build(pl)$x$data
	plDataPointsAll <- plData[sapply(plData, function(x) (x$mode == "markers" & !is.null(x$set)))]
	plDataPoints <- lapply(plDataPointsAll, function(x)
		data.frame(
			x = as.numeric(x$x), 
			y = as.numeric(x$y), 
			key = as.character(x$key),
			stringsAsFactors = FALSE
		)
	)
	plDataPoints <- setNames(do.call(rbind, plDataPoints), cols)
	plDataPoints <- plDataPoints[do.call(order, plDataPoints), ]
	
	expect_equivalent(object = plDataPoints, expected = dataPointLine)# all equal, no attributes

	# check data lines
	plDataLinesAll <- plData[sapply(plData, function(x) (x$mode == "lines" & !is.null(x$set)))]
	plDataLines <- lapply(plDataLinesAll,  function(x)
		data.frame(
			x = as.numeric(x$x), 
			y = as.numeric(x$y), 
			legend = sub("^\\(([^,]*).+", "\\1", x$legendgroup),
			stringsAsFactors = FALSE
		)
	)
	plDataLines <- setNames(do.call(rbind, plDataLines), cols)
	plDataLines <- plDataLines[do.call(order, plDataLines), ]
	
	expect_equivalent(object = plDataLines, expected = dataPointLine)# all equal, no attributes
	
	## check if created plot == reference
	expect_doppelganger(title = "aesthetics", fig = pl, writer = write_svg_plotly)
	
})

test_that("plotting function: reference lines", {
			
	xLine <- mean(dataPlot$LBSTRESNBL)
	yLine <- mean(dataPlot$LBSTRESN)
	pl <- scatterplotMonitoring(
		data = dataPlot, 
		xVar = "LBSTRESNBL", yVar = "LBSTRESN",
		aesPointVar = list(color = "USUBJID"),
		aesLineVar = list(group = "USUBJID", color = "USUBJID"),
		facetPars = list(facets = "VISIT"),
		refLinePars = list(
			# fixed values, no labels
			list(slope = 1, intercept = 0, linetype = 1, color = "black", label = FALSE),
			list(xintercept = xLine), 
			list(yintercept = yLine),
			list(slope = 1, intercept = 0, linetype = 1, color = "black", label = FALSE),
			# from aesthetic
			# default label
			list(xintercept = "LBSTNRLO", linetype = 2, color = "orange", alpha = 0.5), 
			# custom label:
			list(yintercept = "LBSTNRHI", linetype = 2, color = "orange", label = "Reference Range Upper Limit"),
			# combined
			list(slope = 1, intercept = "LBSTNRLO")
		)
	)
	
	## check input == output data
	
	# extract input values for reference lines
	dataRefLines <- unique(dataPlot[, c("VISIT", "LBSTNRLO", "LBSTNRHI")])

	# extract reference lines from output object
	plData <- plotly_build(pl)$x$data
	plDataRefLines <- plData[sapply(plData, function(x) (x$mode == "lines" & is.null(x$set)))]
	
	# check if lines are in the plot
	# ideally we should also check if they are in the correct facet
	# but the mapping facet <-> line doesn't seem to be easily extracted from the plotly_build output
	isRefLineXInPlot <- all(c(dataRefLines$LBSTNRLO, xLine) %in% unlist(lapply(plDataRefLines, function(x) x$x)))
	expect_true(isRefLineXInPlot, info = "All specified horizontal lines are plotted.")

	isRefLineYInPlot <- all(c(dataRefLines$LBSTNRHI, yLine) %in% unlist(lapply(plDataRefLines, function(x) x$y)))
	expect_true(isRefLineYInPlot, info = "All specified vertical lines are plotted.")
	
	## check if created plot == reference
	expect_doppelganger(title = "reference lines", fig = pl, writer = write_svg_plotly)
	
})

test_that("plotting function: labels", {
			
	xLab <- paste("Baseline", labelVars["LBSTRESN"])
	title <- "Actual value of lab parameter at each visit vs baseline"
	pl <- scatterplotMonitoring(
		data = dataPlot, 
		xVar = "LBSTRESNBL", yVar = "LBSTRESN",
		xLab = xLab, # custom label
		aesPointVar = list(color = "USUBJID", shape = "LBNRIND"),
		aesLineVar = list(group = "USUBJID", color = "USUBJID"),
		facetPars = list(facets = "VISIT"),
		labelVars = labelVars,
		title = title
	)
	
	## check input == output data
	
	# extract labels from output object
	yLab <- labelVars["LBSTRESN"]
	plLayout <- plotly_build(pl)$x$layout
	plLayoutAnnot <- plLayout$annotations
	
	# title
	expect_identical(plLayout$title$text, title)
	
	# axes labels
	plAxes <- plLayoutAnnot[sapply(plLayoutAnnot, function(x) "annotationType" %in% names(x) &&x$annotationType == "axis")]
	plAxesLabels <- sapply(plAxes, function(x) x$text)
	expect_equivalent(plAxesLabels, c(xLab, yLab))
	
	# facet labels
	plAnnotAll <- sapply(plLayoutAnnot, function(x)x $text)
	expect_true(all(unique(dataPlot$VISIT) %in% plAnnotAll))
	
	## check if created plot == reference
	expect_doppelganger(title = "labels", fig = pl, writer = write_svg_plotly)
	
})

test_that("interactive table is created", {
	res <- scatterplotMonitoring(
		data = dataPlot, 
		xVar = "LBSTRESNBL", yVar = "LBSTRESN",
		table = TRUE
	)
	expect_is(res$table, "datatables")
})

test_that("facetting", {
		
	plNoFacet <- scatterplotMonitoring(
		data = dataPlot, 
		xVar = "LBSTRESNBL", yVar = "LBSTRESN",
		aesPointVar = list(color = "USUBJID"),
		aesLineVar = list(group = "USUBJID", color = "USUBJID")
	)
			
	# facet_wrap: character test high number of facets
	plWrapSt <- scatterplotMonitoring(
		data = dataPlot, 
		xVar = "LBSTRESNBL", yVar = "LBSTRESN",
		aesPointVar = list(color = "USUBJID"),
		aesLineVar = list(group = "USUBJID", color = "USUBJID"),
		facetPars = list(facets = "USUBJID"),
		themePars = list(legend.position = "right")
	)
	
	# facet_wrap: formula
	plWrapFm <- scatterplotMonitoring(
		data = dataPlot, 
		xVar = "LBSTRESNBL", yVar = "LBSTRESN",
		aesPointVar = list(color = "USUBJID"),
		aesLineVar = list(group = "USUBJID", color = "USUBJID"),
		facetPars = list(facets = as.formula(~VISIT + USUBJID))
	)

})

test_that("Specified limits expanded with data limits", {
			
	xLim <- c(min(dataPlot$LBSTRESNBL) + diff(range(dataPlot$LBSTRESNBL))/2, max(dataPlot$LBSTRESNBL))
	yLim <- c(min(dataPlot$LBSTRESN) + diff(range(dataPlot$LBSTRESN))/2, max(dataPlot$LBSTRESN))
	
	plNoExpandLim <- scatterplotMonitoring(
		data = dataPlot, 
		xVar = "LBSTRESNBL", yVar = "LBSTRESN",
		aesPointVar = list(color = "USUBJID"),
		aesLineVar = list(group = "USUBJID", color = "USUBJID"),
		facetPars = list(facets = as.formula(~VISIT)),
		xLim = xLim, yLim = yLim,
		xLimExpandData = FALSE, yLimExpandData = FALSE
	)
	
	
	plExpandLim <- scatterplotMonitoring(
		data = dataPlot, 
		xVar = "LBSTRESNBL", yVar = "LBSTRESN",
		aesPointVar = list(color = "USUBJID"),
		aesLineVar = list(group = "USUBJID", color = "USUBJID"),
		facetPars = list(facets = as.formula(~VISIT)),
		xLim = xLim, yLim = yLim
	)		

	expect_gt(
		object = diff(plotly_build(plExpandLim)$x$layout$xaxis$range),
		expected = diff(plotly_build(plNoExpandLim)$x$layout$xaxis$range),
		label = "limits of x-axis expanded from data greater than specified limits"
	)
	expect_gt(
		object = diff(plotly_build(plExpandLim)$x$layout$yaxis$range),
		expected = diff(plotly_build(plNoExpandLim)$x$layout$yaxis$range),
		label = "limits of y-axis expanded from data greater than specified limits"
	)			
			
})

