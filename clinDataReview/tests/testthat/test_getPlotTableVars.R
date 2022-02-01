context("Get variables for the plot table")

# 'getPlotTableVars' is also tested through the other plot functionalities
# if a table is extracted for a specific plot
# So other tests are skipped

test_that("An error is generated if the plotting function is not available", {
			
	data <- data.frame(A = c(1, 2), B = c(3, 4))		
	plotArgs <- list(
		data = data,
		xVar = "A", yVar = "B"
	)
	expect_error(
		getPlotTableVars("plot", plotArgs),
		"Extraction of table parameters for function: .* is not yet implemented."
	)
      
})

test_that("Variable and labels are correctly extracted from the table variables", {
      
	data <- data.frame(A = c(1, 2), B = c(3, 4), C = c(3, 5))
	
	# check extraction labels from tableLab, labelVars or tableVars
	plotArgs <- list(
		data = data,
		xVar = "B", yVar = "A", colorVar = "C",
		valueVar = "C",
		tableVars = c("C", "A", "B"),
		tableLab = c(A = "firstName"),
		labelVars = c(B = "secondName")
	)
	
	plotFctNames <- c(
		"scatterplotClinData", "barplotClinData",
		"timeProfileIntervalPlot",
		"plotCountClinData", "treemapClinData", "sunburstClinData"
	)
	
	for(fctName in plotFctNames){
		
		expect_equal(
			object = getPlotTableVars(!!fctName, plotArgs), 
			expected = c("C", "A", "B"), 
			check.attributes = FALSE
		)
		expect_mapequal(
			object = attr(getPlotTableVars(!!fctName, plotArgs), "tableLab"), 
			expected = c(C = "C", B  = "secondName", A = "firstName")
		)
		
	}
      
})

test_that("Variable and labels are correctly extracted from the parameters of the scatterplot", {
			
	data <- data.frame(
		A = c(1, 2), B = c(3, 4), 
		C = c(3, 5), D = c("a", "b"),
		ID = c("1", "2")
	)
			
	plotArgs <- list(
		data = data,
		xVar = "B", yVar = "A",
		aesPointVar = list(shape = "D"),
		aesLineVar = list(linetype = "C"),
		aesLab = c(C = "variable C"),
		xLab = "variable B",
		yLab = "variable A",
		labelVars = c(D = "variable D")
	)
	res <- getPlotTableVars(
		plotFunction = "scatterplotClinData", 
		plotArgs = plotArgs
	)
	expect_equal(
		object = res, 
		expected = c("USUBJID", "B", "A", "D", "C"),
		check.attributes = FALSE
	)
	expect_mapequal(
		object = attr(res, "tableLab"), 
		expected = c(
			A = "variable A", B = "variable B", C = "variable C",
			D = "variable D",
			USUBJID = "USUBJID"
		)
	)
			
})

test_that("Variable and labels are correctly extracted from the parameters of the barplot", {
			
	data <- data.frame(
		A = c(1, 2), B = c(3, 4), 
		C = c(3, 5)
	)
			
	plotArgs <- list(
		data = data,
		xVar = "B", yVar = "A",
		colorVar = "C",
		yLab = "variable A",
		colorLab = "variable C",
		labelVars = c(B = "variable B")
	)

	res <- getPlotTableVars(
		plotFunction = "barplotClinData", 
		plotArgs = plotArgs
	)
	expect_equal(
		object = res, 
		expected = c("B", "C", "A"),
		check.attributes = FALSE
	)
	expect_mapequal(
		object = attr(res, "tableLab"), 
		expected = c(A = "variable A", B = "variable B", C = "variable C")
	)
	
})

test_that("Variable and labels are correctly extracted from the parameters of the count plots", {
			
	data <- data.frame(
		A = c(1, 2), B = c(3, 4), 
		C = c(3, 5), D = c(5, 6),
		E = c(2, 4)
	)
			
	plotArgs <- list(
		data = data,
		vars = c("C",  "B", "E"),
		valueVar = "A",
		colorVar = "D",
		varsLab = c(C = "variable C"),
		valueLab = "variable A",
		colorLab = "variable D",
		labelVars = c(E = "variable E")
	)
			
	plotFctNames <- c(
		"plotCountClinData", "treemapClinData", "sunburstClinData"
	)

	for(fctName in plotFctNames){
		
		expect_equal(
			object = getPlotTableVars(plotFunction = !!fctName, plotArgs = plotArgs), 
			expected = c("C",  "B", "E", "A", "D"),
			check.attributes = FALSE
		)
		expect_mapequal(
			object = {
				res <- getPlotTableVars(plotFunction = !!fctName, plotArgs = plotArgs)
				attr(res, "tableLab")
			},
			expected = c(
				A = "variable A", B = "B",
				C = "variable C",
				D = "variable D", E = "variable E"
			)
		)
		
	}
	
})

test_that("Variable and labels are correctly extracted from the parameters of the time interval plot", {
			
	data <- data.frame(
		CAT = c("A", "A"), TERM = c("a1", "a2"), 
		ASTDY = c(NA_real_, 2), AENDY = c(2, 3),
		ASTFLG = c("Missing", "Complete"),
		AENFLG = c("Complete", "Complete"),
		TRT = c("I", "II")
	)
			
	plotArgs <- list(
		data = data,
		paramGroupVar = "CAT", 
		paramVar = "TERM", 
		timeStartVar = "ASTDY",
		timeEndVar = "AENDY",
		timeStartShapeVar = "ASTFLG",
		timeEndShapeVar = "AENFLG",
		colorVar = "TRT"
	)
			
	# labels extracted from variable lab
	plotArgsParLab <- list(
		paramLab = "term of interest", 
		timeStartLab = "start relative day",
		timeEndLab = "end relative day",
		timeStartShapeLab = "start status",
		timeEndShapeLab = "end status",
		colorLab = "treatment"
	)
	res <- getPlotTableVars(
		plotFunction = "timeProfileIntervalPlot", 
		plotArgs = c(plotArgs, plotArgsParLab)
	)	
	expect_equal(
		object = res, 
		expected = c("CAT", "TERM", "ASTDY", "AENDY", 
			"TRT", "ASTFLG", "AENFLG"), 
		check.attributes = FALSE
	)
	
	tableLabRef <- setNames(
		unlist(plotArgsParLab), 
		unlist(plotArgs[sub("Lab$", "Var", names(plotArgsParLab))])
	)
	# no param yet for label of param group
	tableLabRef <- c(tableLabRef, CAT = "CAT")
	expect_mapequal(attr(res, "tableLab"), tableLabRef)
			
	# labels extracted from general var lab or var name
	plotArgsParLab <- c(plotArgs,
		list(labelVars = c(TRT = "treatment", TERM = "term of interest"))
	)
	res <- getPlotTableVars(
		plotFunction = "timeProfileIntervalPlot", 
		plotArgs = c(plotArgs, plotArgsParLab)
	)
	expect_equal(
		object = res, 
		expected = c("CAT", "TERM", "ASTDY", "AENDY", 
			"TRT", "ASTFLG", "AENFLG"), 
		check.attributes = FALSE
	)
	
	tableLabRef <- c(
		TRT = "treatment", TERM = "term of interest",
		CAT = "CAT", ASTDY = "ASTDY", AENDY = "AENDY", 
		ASTFLG = "ASTFLG", AENFLG = "AENFLG"
	)
	expect_mapequal(
		object = attr(res, "tableLab"), 
		expected = tableLabRef
	)

})