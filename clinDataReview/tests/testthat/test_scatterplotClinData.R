context("Visualize clinical data with a scatterplot")

library(plotly)

test_that("A scatterplot is correctly created", {
			
	data <- data.frame(
		USUBJID = as.character(seq.int(5)),
		LBSTRESN = c(39, 93, 10, 31, 13),
		LBSTRESNBL = c(10, 12, 13, 14, 9),
		stringsAsFactors = FALSE
	)
      
	pl <- scatterplotClinData(
		data = data, 
		xVar = "LBSTRESNBL",
		xLab = "Actual value at baseline",
	    yVar = "LBSTRESN",
	  	yLab = "Actual value at visit X",
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
	dataPointLine <- data[, cols]
	dataPointLine <- dataPointLine[do.call(order, dataPointLine), ]
	dataPointLine <- subset(dataPointLine, ! is.na(LBSTRESNBL))
      
	# check data points
	plData <- plotly_build(pl)$x$data
	plDataPointsAll <- plData[sapply(plData, function(x) 
		(x$mode == "markers" & !is.null(x$set)))]
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
      
	expect_equal(
		object = plDataPoints, 
		expected = dataPointLine,
		check.attributes = FALSE
	)# all equal, no attributes
      
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
	expect_equal(
		object = plDataLines, 
		expected = dataPointLine,
		check.attributes = FALSE
	)# all equal, no attributes
            
})




test_that('Point parameters are set correctly in a scatterplot', {

  pl <- scatterplotClinData(
    data = exampleDataScatter(),
    xVar = "time", yVar = "response",
    aesPointVar = list(color = "treat"),
    pointPars = list( color = 'red'),
    idVar = "subj"
  )
  
  plData <- plotly_build(pl)$x$data
  plmarkerData <- plData[[ 
    which(
      sapply(plData, function(x){
        x$mode == 'markers'} )
    )
  ]]
  # test color parameter correclty
  colorRGB <- sub("^rgba\\((\\d{1,},\\d{1,},\\d{1,}),.+", "\\1\\2\\3", plmarkerData$marker$line$color)
  expect_setequal(object = colorRGB, expected = paste(col2rgb('red'), collapse =","))
  
  
})

test_that("Point parameters with 'colour' (UK English spelling) are set correctly in a scatterplot", {
  
  expect_silent(
    pl <- scatterplotClinData(
      data = exampleDataScatter(),
      xVar = "time", yVar = "response",
      pointPars = list(colour = 'red'),
      idVar = "subj"
    )
  )
  
  plData <- plotly_build(pl)$x$data
  plmarkerData <- plData[[which(sapply(plData, function(x){x$mode == 'markers'}))]]
  colorRGB <- sub("^rgba\\((\\d{1,},\\d{1,},\\d{1,}),.+", "\\1\\2\\3", plmarkerData$marker$line$color)
  expect_equal(object = colorRGB, expected = paste(col2rgb('red'), collapse =","))
  
})


test_that('Line parameters are set correctly in a scatterplot', {
 
   pl <- scatterplotClinData(
    data = exampleDataScatter(),
    xVar = "time", yVar = "response",
    aesPointVar = list(color = "treat"),
    aesLineVar = list(group = 'subj'),
    linePars = list(linetype='dotted'),
    idVar = "subj"
  )
   
  plData <- plotly_build(pl)$x$data
  
  pllineData <- plData[[ 
    which(
      sapply(plData, function(x){
        x$mode == 'lines'} )
    )
  ]]
  
  expect_equal(pllineData$line$dash , 'dot')
  
})


test_that("Smoothing variables are set correctly in a scatterplot", {
  
  exampleData <- exampleDataScatter()
  
  plSmoothLayer <- scatterplotClinData(
    data = exampleData,
    xVar = "time", yVar = "response",
    aesSmoothVar = list(group = 'subj'),
    idVar = "subj",
    smoothPars = list(se=FALSE)
  )
  
  plSmoothData <- plotly_build(plSmoothLayer)$x$data
  
  
  plNoSmooth <- scatterplotClinData(
    data = exampleData,
    xVar = "time", yVar = "response",
    idVar = "subj")
    

  plNoSmoothData <- plotly_build(plNoSmooth)$x$data
  
  # test smoothing line added for each subject
  lineData <- plSmoothData[[which(sapply(plSmoothData, `[[`, "mode") == "lines")]]
  lineKeys <-  unique(unlist(lineData$key))
  lineKeys <-  lineKeys[!is.na(lineKeys)]
  expect_equal(
    object = lineKeys,
    expected = unique(exampleData$subj)
  )
})


test_that("Smoothing parameters are set correctly in a scatterplot", {
  
  exampleData <- exampleDataScatter()
  
  plSmoothLayer <- scatterplotClinData(
    data = exampleData,
    xVar = "time", yVar = "response",
    aesPointVar = list(color = "treat"),
    pointPars = list( color = 'red'),
    aesLineVar = list(group = 'subj'),
    linePars = list(linetype='dotted'),
    aesSmoothVar = list(group = 'subj'),
    smoothPars = list(col='green', se=TRUE) ,
    idVar = "subj")
  
  plSmoothData <- plotly_build(plSmoothLayer)$x$data
  
  # no standard error means one plotting layer less
  
  plSmoothNoSE <- scatterplotClinData(
    data = exampleData,
    xVar = "time", yVar = "response",
    aesPointVar = list(color = "treat"),
    pointPars = list( color = 'red'),
    aesLineVar = list(group = 'subj'),
    linePars = list(linetype='dotted'),
    aesSmoothVar = list(group = 'subj'),
    smoothPars = list(col='green', se=FALSE),
    idVar = "subj"
  ) 

  plSmoothNoSEData <-  plotly_build(plSmoothNoSE)$x$data
  
  # test that error band parameters added 
  expect_gt(
    length(plSmoothData),
    length(plSmoothNoSEData)
  )
  
  # test that color is smoothing curves is set correctly
  
  isLine <- sapply(plSmoothNoSEData, `[[`, "mode") == "lines"
  colors <- sapply(plSmoothNoSEData[isLine], function(x) x$line$color)
  colorsRGB <- sub("^rgba\\((\\d{1,},\\d{1,},\\d{1,}),.+", "\\1\\2\\3", colors)[-1]
  
  expect_setequal(object = colorsRGB, expected = paste(col2rgb('green'), collapse =","))
  
}
)

test_that("Smoothing layer can be disabled in scatterplot", {
  
  exampleData <- exampleDataScatter()
  
  plWithSmoothLayer <- scatterplotClinData(
    data = exampleData,
    xVar = "time", yVar = "response",
    aesPointVar = list(color = "treat"),
    pointPars = list( color = 'red'),
    aesLineVar = list(group = 'subj'),
    linePars = list(linetype='dotted'),
    aesSmoothVar = list(group = 'subj'),
    smoothPars = list(col='green', se=TRUE),
    smoothInclude = TRUE,
    idVar = "subj"
  )

  dataWithSmooth <- plotly_build(plWithSmoothLayer)$x$data
  
  plNoSmoothLayer <- scatterplotClinData(
    data = exampleData,
    xVar = "time", yVar = "response",
    aesPointVar = list(color = "treat"),
    pointPars = list( color = 'red'),
    aesLineVar = list(group = 'subj'),
    linePars = list(linetype='dotted'),
    aesSmoothVar = list(group = 'subj'),
    smoothPars = list(col='green', se=TRUE),
    smoothInclude = FALSE,
    idVar = "subj"
    )
  
  dataNoSmooth <- plotly_build(plNoSmoothLayer)$x$data

  
  
  # test that smoothing layer not included when smoothInclude = FALSE
  expect_equal(
    length(dataWithSmooth),
    length(dataNoSmooth) + 2 # one layer for smooth, 1 for se
  )
}
)
  



test_that("Reference lines are correctly implemented in a scatterplot", {
			
	data <- data.frame(
		USUBJID = as.character(seq.int(5)),
		LBSTRESN = c(39, 93, 10, 31, 13),
		LBSTRESNBL = c(10, 12, 13, 14, 9),
		LBSTNRLO = 0,
		LBSTNRHI = 50,
		VISIT = c("Screening 1", "Screening 1", "Screening 1", "Week 1", "Week 1")
	)
      
	xLine <- mean(data$LBSTRESNBL)
	yLine <- mean(data$LBSTRESN)
	
	pl <- scatterplotClinData(
		data = data, 
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
	dataRefLines <- unique(data[, c("VISIT", "LBSTNRLO", "LBSTNRHI")])
      
	# extract reference lines from output object
	plData <- plotly_build(pl)$x$data
	plDataRefLines <- plData[sapply(plData, function(x) 
		(x$mode == "lines" & is.null(x$set)))]
      
	# check if lines are in the plot
	# ideally we should also check if they are in the correct facet
 	# but the mapping facet <-> line doesn't seem to be easily extracted from the plotly_build output
	isRefLineXInPlot <- all(c(dataRefLines$LBSTNRLO, xLine) %in% 
		unlist(lapply(plDataRefLines, function(x) x$x)))
	expect_true(isRefLineXInPlot, info = "All specified horizontal lines are plotted.")
      
	isRefLineYInPlot <- all(c(dataRefLines$LBSTNRHI, yLine) %in% 
		unlist(lapply(plDataRefLines, function(x) x$y)))
	expect_true(isRefLineYInPlot, info = "All specified vertical lines are plotted.")
            
})

test_that("Axis labels and title are correctly set in a scatterplot", {
			
	data <- data.frame(
		USUBJID = as.character(seq.int(5)),
		LBSTRESN = c(39, 93, 10, 31, 13),
		LBSTRESNBL = c(10, 12, 13, 14, 9)
	)
	
	labelVars <- c(
		LBSTRESN = "Actual value",
		LBSTRESNBL = "Actual value at baseline"
	)
      
	xLab <- paste("Baseline", labelVars["LBSTRESN"])
	title <- "Actual value of lab parameter at each visit vs baseline"
	pl <- scatterplotClinData(
	 	data = data, 
		xVar = "LBSTRESNBL", yVar = "LBSTRESN",
		xLab = xLab, # custom label
		labelVars = labelVars,
		title = title
	)
      
	## check input == output data
      
	# extract labels from output object
	yLab <- labelVars["LBSTRESN"]
	plLayout <- plotly_build(pl)$x$layout
	plLayoutAnnot <- plLayout$annotations
      
	# title
	expect_equal(
		object =  plLayout$title$text, 
		expected = title, 
		check.attributes = FALSE
	)
      
	# axes labels
	expect_equal(
		object = pl$x$layout$xaxis$title$text, 
		expected = xLab,
		check.attributes = FALSE
	)
	expect_equal(
		object = pl$x$layout$yaxis$title$text, 
		expected = unname(yLab),
		check.attributes = FALSE
	)
	  
})

test_that("An interactive table is created in addition to the scatterplot", {
      
	data <- data.frame(
		USUBJID = as.character(seq.int(5)),
		LBSTRESN = c(39, 93, 10, 31, 13),
		LBSTRESNBL = c(10, 12, 13, 14, 9)
	)
			
	res <- scatterplotClinData(
		data = data, 
		xVar = "LBSTRESNBL", yVar = "LBSTRESN",
		table = TRUE
	)
	expect_s3_class(res$table, "datatables")
      
})

test_that("The scatterplot is successfully facetted based on a variable", {
			
	data <- data.frame(
		USUBJID = as.character(seq.int(5)),
		LBSTRESN = c(39, 93, 10, 31, 13),
		LBSTRESNBL = c(10, 12, 13, 14, 9)
	)
      
	# facet_wrap: character test high number of facets
	plWrapSt <-	scatterplotClinData(
		data = data, 
		xVar = "LBSTRESNBL", yVar = "LBSTRESN",
		aesPointVar = list(color = "USUBJID"),
		aesLineVar = list(group = "USUBJID", color = "USUBJID"),
		facetPars = list(facets = "USUBJID"),
		themePars = list(legend.position = "right")
	)
	
	expect_s3_class(plWrapSt, "plotly")
	
})

test_that("The scatterplot is successfully facetted based on a formula", {
			
	data <- data.frame(
		USUBJID = as.character(seq.int(5)),
		LBSTRESN = c(39, 93, 10, 31, 13),
		LBSTRESNBL = c(10, 12, 13, 14, 9),
		VISIT = c("Screening 1", "Screening 1", 
			"Screening 1", "Week 1", "Week 1")
	)
			
	# facet_wrap: formula
	plWrapFm <- scatterplotClinData(
		data = data, 
		xVar = "LBSTRESNBL", yVar = "LBSTRESN",
		aesPointVar = list(color = "USUBJID"),
		aesLineVar = list(group = "USUBJID", color = "USUBJID"),
		facetPars = list(facets = as.formula(~VISIT + USUBJID))
	)
	
	expect_s3_class(plWrapFm, "plotly")
      
})

test_that("Specified x-axis and y-axis limits are expanded when data is outside of these limits in a scatterplot", {
      
	data <- data.frame(
		USUBJID = as.character(seq.int(5)),
		LBSTRESN = c(39, 93, 10, 31, 13),
		LBSTRESNBL = c(10, 12, 13, 14, 9)
	)
	xLim <- c(min(data$LBSTRESNBL) + diff(range(data$LBSTRESNBL))/2, max(data$LBSTRESNBL))
	yLim <- c(min(data$LBSTRESN) + diff(range(data$LBSTRESN))/2, max(data$LBSTRESN))
	
	plNoExpandLim <- scatterplotClinData(
		data = data, 
		xVar = "LBSTRESNBL", yVar = "LBSTRESN",
	 	aesPointVar = list(color = "USUBJID"),
		aesLineVar = list(group = "USUBJID", color = "USUBJID"),
		xLim = xLim, yLim = yLim,
		xLimExpandData = FALSE, yLimExpandData = FALSE
	)
      
	plExpandLim <- scatterplotClinData(
		data = data, 
		xVar = "LBSTRESNBL", yVar = "LBSTRESN",
		aesPointVar = list(color = "USUBJID"),
		aesLineVar = list(group = "USUBJID", color = "USUBJID"),
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

test_that("A scatterplot with specified hover variables is successfully created", {
      
	data <- data.frame(
		USUBJID = as.character(seq.int(5)),
		LBSTRESN = c(39, 93, 10, 31, 13),
		LBSTRESNBL = c(10, 12, 13, 14, 9)
	)
			
	plOutput <- scatterplotClinData(
		data = data, 
		xVar = "LBSTRESNBL", yVar = "LBSTRESN",
		hoverVars = c("LBSTRESNBL", "USUBJID")
	)
	expect_s3_class(plOutput, "plotly")
      
})

test_that("A legend is correctly not included in a scatterplot", {
      
	data <- data.frame(
		USUBJID = as.character(seq.int(5)),
		LBSTRESN = c(39, 93, 10, 31, 13),
		LBSTRESNBL = c(10, 12, 13, 14, 9)
	)
			
	plOutput <- scatterplotClinData(
		data = data, 
		xVar = "LBSTRESNBL", yVar = "LBSTRESN",
		themePars = list(legend.position = "none")
	)
	expect_s3_class(plOutput, "plotly")
      
})

test_that("A color palette is correctly set in a scatterplot", {
      
	data <- data.frame(
		DY = c(1, 2, 1, 2),
		AVAL = c(3, 4, 2, 6),
		NRIND = c("Normal", "High", "Low", "Normal"),
		USUBJID = c(1, 1, 2, 2),
		stringsAsFactors = FALSE
	)
	colorPalette <- c(
		Low = "purple", 
		Normal = "green", 
		High = "blue"
	)
	pl <- scatterplotClinData(
		data = data, 
		xVar = "DY", 
		yVar = "AVAL", 
		aesPointVar = list(color = "NRIND"),
 		scalePars = list(
			list(aesthetic = "colour", values = colorPalette)	
		)
	)
	plData <- plotly_build(pl)$x$data
	plData <- lapply(plData, function(x){
		data.frame(
			x = as.numeric(x$x),
			y = as.numeric(x$y),
			NRIND = as.character(x$name),
			color = as.character(x$marker$line$color),
			stringsAsFactors = FALSE
		)
	})
	plData <- do.call(rbind, plData)
	plData <- plData[do.call(order, plData), ]
      
	# plotly specifies color in rgba
	colorPaletteRGB <- col2rgb(colorPalette)
	colorPaletteRGBA <- paste0(
		"rgba(", 
		apply(colorPaletteRGB, 2, paste, collapse = ","), 
		",1)" # + alpha
	)
	names(colorPaletteRGBA) <- colorPalette
      
	# extract color for input data:
	data$color <- colorPaletteRGBA[colorPalette[data$NRIND]]
	data <- data[, c("DY", "AVAL", "NRIND", "color")] 
	data <- data[do.call(order, data), ]
      
	expect_equal(
		object = plData,
		expected = data, 
		check.attributes = FALSE
	)
      
})
	
test_that("The x-axis labels are correctly set from variables in the scatterplot", {
		
	data <- data.frame(
		AVISIT = factor(
			c("Day 4", "Screening"), 
			levels = c("Screening", "Day 4")
		),
		AVAL = c(1, 2),
		USUBJID = c("1", "2"),
		n = c("N = 4", "N = 3"),
		stringsAsFactors = FALSE
	)
	pl <- scatterplotClinData(
		data = data, 
		xVar = "AVISIT", xLabVars = c("AVISIT", "n"),
		yVar = "AVAL"
	)
	plXAxis <- plotly_build(pl)$x$layout$xaxis
	# extract tick labels
	plXTickLab <- plXAxis$ticktext
	# and sort them
	plXTickLab <- plXTickLab[order(plXAxis$tickvals, decreasing = FALSE)]
	
	expect_match(object = plXTickLab[1], regexp = "Screening.+N = 3")
	expect_match(object = plXTickLab[2], regexp = "Day 4.+N = 4")
			
})

test_that("Variables for x-axis labels are correctly combined and ordered if multiple in a scatterplot", {
			
	data <- data.frame(
		AVISIT = factor(
			c("Day 4", "Screening", "Day 4", "Screening"), 
			levels = c("Screening", "Day 4")
		),
		AVAL = c(1, 2, 3, 4),
		USUBJID = c("1", "1", "2", "2"),
		TREAT = factor(
			c("Compound", "Compound", "Placebo", "Placebo"),
			levels = c("Placebo", "Compound")
		),
		stringsAsFactors = FALSE
	)
	pl <- scatterplotClinData(
		data = data, 
		xVar = "AVISIT", xLabVars = c("AVISIT", "TREAT"),
		yVar = "AVAL"
	)
	plXAxis <- plotly_build(pl)$x$layout$xaxis
	
	# extract tick labels
	plXTickLab <- plXAxis$ticktext
	# and sort them
	plXTickLab <- plXTickLab[order(plXAxis$tickvals, decreasing = FALSE)]
			
	expect_match(object = plXTickLab[1], regexp = "Screening.+Placebo.+Compound")
	expect_match(object = plXTickLab[2], regexp = "Day 4.+Placebo.+Compound")
	
})
	

test_that("A subtitle is correctly set in a scatterplot", {
			
	data <- data.frame(
		DY = c(1, 2, 1, 2),
		AVAL = c(3, 4, 2, 6),
		USUBJID = c(1, 1, 2, 2),
		stringsAsFactors = FALSE
	)	
	
	subtitle <- paste(sample(LETTERS, 100, replace = TRUE), collapse = "")
	subtitle <- paste(rep(subtitle, 10), collapse = "\n")
	plSubtitle <- scatterplotClinData(
		data = data, 
		xVar = "DY", 
		yVar = "AVAL", 
		subtitle = subtitle
	)
	
	# extract annotation
	plAnnot <- plotly_build(plSubtitle)$x$layout$annotations
	plSubtitleAnnot <- lapply(plAnnot, function(xEl)
		if(hasName(xEl, "text"))
			xEl[["text"]]
	)
	plSubtitleAnnot <- unlist(plSubtitleAnnot, recursive = TRUE, use.names = FALSE)
	expect_match(plSubtitleAnnot, gsub("\n", ".*", subtitle))
	
	# check that top margin is increased
	plNoSubtitle <- scatterplotClinData(
		data = data, 
		xVar = "DY", 
		yVar = "AVAL"
	)
	expect_gt(
		object = plotly_build(plSubtitle)$x$layout$margin$t,
		expected = plotly_build(plNoSubtitle)$x$layout$margin$t
	)
			
})

test_that("Extra margin is correctly set for the subtitle in a facetted plot", {
			
	data <- data.frame(
		DY = c(1, 2, 1, 2),
		AVAL = c(3, 4, 2, 6),
		USUBJID = c(1, 1, 2, 2),
		TRT = c("A", "A", "B", "B"),
		stringsAsFactors = FALSE
	)
			
	subtitle <- paste(sample(LETTERS, 100, replace = TRUE), collapse = "")
	subtitle <- paste(rep(subtitle, 10), collapse = "\n")
	plSubtitleFacet <- scatterplotClinData(
		data = data, 
		xVar = "DY", 
		yVar = "AVAL", 
		subtitle = subtitle,
		facetPars = list(facets = "TRT")
	)
	
	# check that top margin is increased
	plSubtitleNoFacet <- scatterplotClinData(
		data = data, 
		xVar = "DY", 
		yVar = "AVAL",
		subtitle = subtitle
	)
	expect_gt(
		object = plotly_build(plSubtitleFacet)$x$layout$margin$t,
		expected = plotly_build(plSubtitleNoFacet)$x$layout$margin$t
	)

})

test_that("A caption is correctly set in a scatterplot", {
			
	data <- data.frame(
		DY = c(1, 2, 1, 2),
		AVAL = c(3, 4, 2, 6),
		USUBJID = c(1, 1, 2, 2),
		stringsAsFactors = FALSE
	)	
			
	caption <- paste(sample(LETTERS, 100, replace = TRUE), collapse = "")
	caption <- paste(rep(caption, 10), collapse = "\n")
	plCaption <- scatterplotClinData(
		data = data, 
		xVar = "DY", xLab = "TEST",
		yVar = "AVAL", 
		caption = caption
	)
			
	# extract annotation
	plAnnot <- plotly_build(plCaption)$x$layout$annotations
	plCaptionAnnot <- lapply(plAnnot, function(xEl)
		if(hasName(xEl, "text"))
			xEl[["text"]]
	)
	plCaptionAnnot <- unlist(plCaptionAnnot, recursive = TRUE, use.names = FALSE)
	expect_match(plCaptionAnnot, gsub("\n", ".*", caption))
	
	# check that bottom margin is increased
	plNoCaption <- scatterplotClinData(
		data = data, 
		xVar = "DY", 
		yVar = "AVAL"
	)
	expect_gt(
		object = plotly_build(plCaption)$x$layout$margin$b,
		expected = plotly_build(plNoCaption)$x$layout$margin$b
	)			
})

test_that("A selection variable is correctly included in a scatterplot", {
  
  data <- data.frame(
    group = factor(c("A", "A", "A", "A", "B"), levels = c("B", "A")),
    DY = c(1, 2, 1, 2, 3),
    AVAL = c(3, 4, 2, 6, 5),
    USUBJID = c(1, 1, 2, 2, 3),
    stringsAsFactors = FALSE
  )	
  
  res <- scatterplotClinData(
    data = data, 
    xVar = "DY",
    yVar = "AVAL",
    aesLineVar = list(group = "USUBJID"),
    selectVars = "group"
  )
  
  # check the output:
  expect_s3_class(res, "clinDataReview")
  expect_named(res, expected = c("buttons", "plot"))
  expect_s3_class(res$plot, "plotly")
  
  expect_length(res$buttons, 1)
  
  # check button values
  btnScriptTag <- htmltools::tagQuery(res$buttons)$find("script")$selectedTags()
  buttonData <- jsonlite::fromJSON(txt = as.character(btnScriptTag[[1]]$children))
  expect_equal(object = buttonData$items$value, expected = levels(data$group))
  
})

test_that("A box to highlight the elements of the ID variable is correctly included in a scatterplot", {
  
  data <- data.frame(
    DY = c(1, 2, 1, 2),
    AVAL = c(3, 4, 2, 6),
    USUBJID = c(1, 1, 2, 2),
    stringsAsFactors = FALSE
  )	
  
  pl <- scatterplotClinData(
    data = data, 
    xVar = "DY",
    yVar = "AVAL",
    idHighlightBox = TRUE
  )
  
  # check the output:
  expect_s3_class(pl, "plotly")
  expect_true(pl$x$highlight$selectize)
  
})