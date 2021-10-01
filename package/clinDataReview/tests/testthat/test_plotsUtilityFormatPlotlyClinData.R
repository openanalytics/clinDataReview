context("Format a plotly clinical data object")

library(plotly)

test_that("A plotly object is correctly formatted with a path variable", {
			
	data <- data.frame(
		USUBJID = seq.int(2),
		pathVar = sprintf("<a href=\"./path-to-report-%d\">label</a>", seq.int(2)),
		stringsAsFactors = FALSE
	)
	plotlyOut <- formatPlotlyClinData(
		pl = plot_ly(),
		data = data,
		pathVar = "pathVar"
	)
	expect_s3_class(plotlyOut, "plotly")
	
	# JS function included
	expect_type(plotlyOut$jsHooks$render, "list")
	expect_equal(
		object = plotlyOut$jsHooks$render[[1]]$data,
		expected = setNames(data, c("key", "path"))
	)
	
	# JS dependencies are included
	expect_type(plotlyOut$prepend, "list")
      
})

test_that("A plotly object is correctly formatted with a path variable and multiple identifier variables", {

	data <- data.frame(
		USUBJID = seq.int(2),
		IDVAR2 = c("i1", "i2"),
		pathVar = sprintf("<a href=\"./path-to-report-%d\">label</a>", seq.int(2)),
		stringsAsFactors = FALSE
	)	
	plotlyOut <- formatPlotlyClinData(
		pl = plot_ly(),
		data = data,
		idVar = c("USUBJID", "IDVAR2"),
		pathVar = "pathVar"
	)
	expect_s3_class(plotlyOut, "plotly")
	
	# JS function included
	expect_type(plotlyOut$jsHooks$render, "list")
	expect_equal(
		object = plotlyOut$jsHooks$render[[1]]$data,
		expected = data.frame(
			key = factor(c("1.i1", "2.i2")),
			path = data[["pathVar"]],
			stringsAsFactors = FALSE
		),
		check.attributes = FALSE
	)
	# JS dependencies are included
	expect_type(plotlyOut$prepend, "list")
      
})

test_that("An error is generated when records with the same ID have different paths when formatting a plotly object", {
      
	data <- data.frame(
		USUBJID = c("ID3", "ID3"),
		IDVAR2 = c("i3", "i3"),
		pathVar = c(
			"<a href=\"./path-to-report\">label</a>",
			"<a href=\"./path-to-report-other\">labelOther</a>"
		),
		stringsAsFactors = FALSE
	)
      
	expect_error(
		formatPlotlyClinData(
			pl = plot_ly(),
 			data = data,
			idVar = c("USUBJID", "IDVAR2"),
			pathVar = "pathVar"
		),
		"Different .* available for specific .*"
	)
      
})