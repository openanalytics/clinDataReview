context("Visualize clinical data with a boxplot")

# Suppress the following warning as this is a bug in plotly: 
# See https://github.com/ropensci/plotly/issues/994
ignoreBoxmodeWarning <- function(expr){
	withCallingHandlers(
		expr,
		warning = function(w){
			if(grepl("^'layout' objects don't have these attributes: 'boxmode'",w$message)){
				invokeRestart("muffleWarning")
			}
		}
	)		
}

library(plotly)

test_that("Boxplots are correctly generated", {
			
	data <- data.frame(
		USUBJID = c("1", "1", "2", "2", "2"),
		AVISIT = c("Week 1", "Week 1", "Baseline", "Week 1", "Week 1"),
		ATPT = c("PREDOSE", "1H", "PREDOSE", "PREDOSE", "PREDOSE"),
		TRTA = c("A", "A", "B", "B", "B"),
		AVAL = c(34, 29, 70, 13, 45),
		patientProfileLink = sprintf("<a href=\"./path-to-report-%d\">label</a>", c(1, 1, 2, 2, 2)),
		stringsAsFactors = FALSE
	)

	# create plot
	pl <- ignoreBoxmodeWarning({
		boxplotClinData(
			data = data,
			xVar = "AVISIT", 
			yVar = "AVAL",
			colorVar = "TRTA",
			facetVar = "ATPT",
			title = "Diastolic Blood Pressure distribution by actual visit and analysis timepoint",
			yLab = "Actual value of the Diastolic Blood Pressure parameter (mmHg)",
			pathVar = "patientProfileLink"
		)
	})

	## check if input == output data
		
	# extract data from output object
	plData <- ignoreBoxmodeWarning(plotly_build(pl)$x$data)
	
	# only box aes
	plDataBox <- plData[sapply(plData, function(x) x$type == "box")]
		
	plDataBoxDf <- do.call(rbind,
		lapply(plDataBox, function(x) 
			data.frame(
				AVISIT = as.character(x[["x"]]), 
				AVAL = as.numeric(x$y),
				TRTA = x$legendgroup,
				# To get facetVar look at the key definition. This extraction relies on the
				# column facetVar position in the key-column definition. 
				# Ideally it should be made more general in the future. 
				ATPT = strsplit(as.character(x$key),"\\.")[[1]][[3]], 
				stringsAsFactors = FALSE
			)
		)
	)
		
	data <- data[c("AVISIT","AVAL","TRTA","ATPT")]
		
	# order data to compare with each other
	data <- data[with(data,order(ATPT, TRTA, AVISIT, AVAL)),]
	plDataBoxDf <- plDataBoxDf[with(plDataBoxDf,order(ATPT, TRTA, AVISIT, AVAL)),]
	expect_equivalent(object = data, expected = plDataBoxDf)
		
})

test_that("An interactive table is created in addition to the boxplot", {
			
	data <- data.frame(
		USUBJID = c("1", "1", "2", "2", "2"),
		AVISIT = c("Week 1", "Week 1", "Baseline", "Week 1", "Week 1"),
		ATPT = c("PREDOSE", "1H", "PREDOSE", "PREDOSE", "PREDOSE"),
		TRTA = c("A", "A", "B", "B", "B"),
		AVAL = c(34, 29, 70, 13, 45),
		patientProfileLink = sprintf("<a href=\"./path-to-report-%d\">label</a>", c(1, 1, 2, 2, 2))
	)
		
	# create plot
	res <- ignoreBoxmodeWarning({
		boxplotClinData(
			data = data,
			xVar = "AVISIT", yVar = "AVAL", colorVar = "TRTA", facetVar = "ATPT",
			table = TRUE,
			pathVar = "patientProfileLink", pathLab = "Subject variable"
		)
	})
		
	expect_s3_class(res$table, "datatables")
		
})

test_that("A boxplot with selected hover variables is created", {
			
	data <- data.frame(
		USUBJID = c("1", "1", "2", "2", "2"),
		AVISIT = c("Week 1", "Week 1", "Baseline", "Week 1", "Week 1"),
		AVAL = c(34, 29, 70, 13, 45)
	)
    
	# create plot
	res <- ignoreBoxmodeWarning({
		boxplotClinData(
			data = data,
			xVar = "AVISIT", yVar = "AVAL", 
			hoverVars = c("USUBJID", "AVISIT", "AVAL")
		)
	})
		
    expect_s3_class(res, "plotly")
    
})

test_that("A boxplot is successfully created with a color variable but without facets", {
			
	data <- data.frame(
		USUBJID = c("1", "1", "2", "2", "2"),
		AVISIT = c("Week 1", "Week 1", "Baseline", "Week 1", "Week 1"),
		AVAL = c(34, 29, 70, 13, 45),
		TRTA = c("A", "A", "B", "B", "B")
	)
		
	# create plot
	res <- ignoreBoxmodeWarning({
		boxplotClinData(
			data = data,
			xVar = "AVISIT", yVar = "AVAL", 
			colorVar = "TRTA", facetVar = NULL, 
		)
	})
		
    expect_s3_class(res, "plotly")
	
})

test_that("A boxplot is successfully created with facets but without a color variable", {
			
	data <- data.frame(
		USUBJID = c("1", "1", "2", "2", "2"),
		AVISIT = c("Week 1", "Week 1", "Baseline", "Week 1", "Week 1"),
		AVAL = c(34, 29, 70, 13, 45),
		ATPT = c("PREDOSE", "1H", "PREDOSE", "PREDOSE", "PREDOSE")
	)
			
	# create plot
	res <- ignoreBoxmodeWarning({
		boxplotClinData(
			data = data,
			xVar = "AVISIT", yVar = "AVAL", 
			colorVar = NULL, facetVar = "ATPT", 
		)
	})
		
    expect_s3_class(res, "plotly")
	
})

test_that("A boxplot is successfully created without a color variable and facets", {
			
	data <- data.frame(
		USUBJID = c("1", "1", "2", "2", "2"),
		AVISIT = c("Week 1", "Week 1", "Baseline", "Week 1", "Week 1"),
		AVAL = c(34, 29, 70, 13, 45)
	)	
	
	res <- ignoreBoxmodeWarning({
		boxplotClinData(
			data = data,
			xVar = "AVISIT", yVar = "AVAL", 
			colorVar = NULL, facetVar = NULL, 
		)
	})
		
    expect_s3_class(res, "plotly")

})