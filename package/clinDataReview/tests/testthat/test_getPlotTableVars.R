context("Test extraction of table variables")

# 'getPlotTableVars' is also tested through the other plot functionalities
# if a table is extracted for a specific plot
# So other tests are skipped

test_that("error is triggered if plotting function not available", {
			
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

test_that("variable and labels are extracted from table variables", {
      
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
			getPlotTableVars(!!fctName, plotArgs), 
			c("C", "A", "B"), 
			check.attributes = FALSE
		)
		expect_mapequal(
			attr(getPlotTableVars(!!fctName, plotArgs), "tableLab"), 
			c(C = "C", B  = "secondName", A = "firstName")
		)
		
	}
      
})

test_that("value variable displayed as a bar in table for count plot(s)", {
			
	plotArgs <- list(
		data = data,
		vars = "A", 
		colorVar = "B", valueVar = "C",
		tableVars = c("A", "B"),
		labelVars = NULL
	)
	
	plotFctNames <- c(
		"plotCountClinData", "treemapClinData", "sunburstClinData"
	)
	
	for(fctName in plotFctNames){
		
		expect_identical({
			res <- getPlotTableVars(!!fctName, plotArgs)
			attr(res, "tablePars")
			}, 
			expected = list(nonVisibleVar = "C", barVar = "C")
		)
		
	}
			
})

test_that("variable/labels are extracted from scatterplot parameters", {
			
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
	expect_silent(
		res <- getPlotTableVars(
			"scatterplotClinData", 
			plotArgs
		)
	)
	expect_equal(
		res, 
		c("USUBJID", "B", "A", "D", "C"), 
		check.attributes = FALSE
	)
	expect_mapequal(
		attr(res, "tableLab"), 
		c(A = "variable A", B = "variable B", C = "variable C",
			D = "variable D",
			USUBJID = "USUBJID"
		)
	)
			
})

test_that("variable/labels are extracted from barplot parameters", {
			
	data <- data.frame(
		A = c(1, 2), B = c(3, 4), 
		C = c(3, 5)
	)
			
	plotArgs <- list(
		data = data,
		xVar = "B", yVar = "A",
		colorVar = "C"
	)
	
	# labels extracted from variable lab
	plotArgsParLab <- c(plotArgs, list(
		xLab = "variable B",
		yLab = "variable A",
		colorLab = "variable C"
	))	
	expect_silent(
		res <- getPlotTableVars(
			"barplotClinData", 
			plotArgsParLab
		)
	)	
	expect_equal(res, c("B", "C", "A"), check.attributes = FALSE)
	expect_mapequal(
		attr(res, "tableLab"), 
		c(A = "variable A", B = "variable B", C = "variable C")
	)
	
	# labels extracted from general var lab or var name
	plotArgsLab <- c(plotArgs,
		list(labelVars = c(C = "variable C"))
	)
	expect_silent(
		res <- getPlotTableVars(
			"barplotClinData", 
			plotArgsLab
		)
	)
	expect_equal(res, c("B", "C", "A"), check.attributes = FALSE)
	expect_mapequal(
		attr(res, "tableLab"), 
		c(A = "A", B = "B", C = "variable C")
	)
			
})

test_that("variable/labels are extracted from count plot(s) parameters", {
			
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
			getPlotTableVars(!!fctName, plotArgs), 
			c("C",  "B", "E", "A", "D"), 
			check.attributes = FALSE
		)
		expect_mapequal({
			res <- getPlotTableVars(!!fctName, plotArgs)
			attr(res, "tableLab")
			},
			expected = c(A = "variable A", B = "B",
				C = "variable C",
				D = "variable D", E = "variable E")
		)
		
	}
	
})

test_that("variable/labels are extracted from time profile interval plot parameters", {
			
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
	expect_silent(
		res <- getPlotTableVars(
			"timeProfileIntervalPlot", 
			c(plotArgs, plotArgsParLab)
		)
	)	
	expect_equal(
		res, 
		c("CAT", "TERM", "ASTDY", "AENDY", 
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
	expect_silent(
		res <- getPlotTableVars(
			"timeProfileIntervalPlot", 
			c(plotArgs, plotArgsParLab)
		)
	)
	expect_equal(
		res, 
		c("CAT", "TERM", "ASTDY", "AENDY", 
			"TRT", "ASTFLG", "AENFLG"), 
		check.attributes = FALSE
	)
	
	tableLabRef <- c(
		TRT = "treatment", TERM = "term of interest",
		CAT = "CAT", ASTDY = "ASTDY", AENDY = "AENDY", 
		ASTFLG = "ASTFLG", AENFLG = "AENFLG"
	)
	expect_mapequal(attr(res, "tableLab"), tableLabRef)

})
