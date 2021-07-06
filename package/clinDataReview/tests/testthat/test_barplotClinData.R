context("Barplot for clinical data")

library(clinUtils)
library(inTextSummaryTable)
library(plotly)

# load example data
data(dataADaMCDISCP01)
labelVars <- attr(dataADaMCDISCP01, "labelVars")

dataAE <- dataADaMCDISCP01$ADAE
dataDM <- dataADaMCDISCP01$ADSL

# sunburst takes as input table with counts

# total counts: Safety Analysis Set (patients with start date for the first treatment)
dataTotal <- subset(dataDM, RFSTDTC != "")

## patient profiles report

# add path in data
dataAE$patientProfilePath <- paste0(
	"patientProfiles/subjectProfile-", 
	sub("/", "-", dataAE$USUBJID), ".pdf"
)
# add link in data (for attached table)
dataAE$patientProfileLink <- with(dataAE,
	paste0(
		'<a href="', patientProfilePath, 
		'" target="_blank">', USUBJID, '</a>'
	)
)

# combine all paths across patients
statsExtraPP <- list(
		statPatientProfilePath = function(data)	
			toString(sort(unique(data$patientProfilePath))),
		statPatientProfileLink = function(data)
			toString(sort(unique(data$patientProfileLink)))
)

# get default counts + stats with subjects profiles path
statsPP <- c(
	getStats(type = "count-default"),
	list(
			patientProfilePath = quote(statPatientProfilePath),
			patientProfileLink = quote(statPatientProfileLink)
	))

# compute adverse event table
tableAE <- getSummaryStatisticsTable(
		
	data = dataAE,
	rowVar = c("AESOC", "AEDECOD"),
	dataTotal = dataTotal,
	rowOrder = "total",
	labelVars = labelVars,
	
	# plotly treemap requires records (rows) for each group
	rowVarTotalInclude = "AEDECOD",
	
	## DT-output specific:
	outputType = "data.frame-base",
	# statistics of interest
	# for DT output, include columns with patients
	stats = statsPP, 
	# add extra 'statistic': concatenate subject IDs
	statsExtra = statsExtraPP

)

dataPlot <- subset(tableAE, AEDECOD != "Total")

dataPlot$n <- as.numeric(dataPlot$n)

test_that("plotting function runs properly", {
			
	# create plot
	pl <- barplotClinData(
		data = dataPlot,
		xVar = "AEDECOD", colorVar = "AESOC",
		yVar = "n", yLab = "Number of patients with adverse events",
		labelVars = labelVars,
		pathVar = "patientProfileLink", pathLab = getLabelVar(var = "USUBJID", labelVars = labelVars)
	)
	
	## check if input == output data
	
	# extract data from output object
	plData <- plotly_build(pl)$x$data
	
	# only bar aes
	plDataBar <- plData[sapply(plData, function(x) x$type == "bar")]
	
	plDataBarDf <- do.call(rbind,
		lapply(plDataBar, function(x) 
			data.frame(
				AEDECOD = as.character(x[["x"]]), 
				n = x$y, 
				stringsAsFactors = TRUE
			)
		)
	)
	
	dataPlot <- dataPlot[, c("AEDECOD", "n")]
	dataPlot <- dataPlot[match(plDataBarDf$AEDECOD, dataPlot$AEDECOD), ]
	expect_equivalent(object = plDataBarDf, expected = dataPlot)
	
})

test_that("interactive table is created", {
			
	res <- barplotClinData(
		data = dataPlot,
		xVar = "AEDECOD", yVar = "n",
		table = TRUE
	)
	
	expect_is(res$table, "datatables")
	
})

test_that("Barplot with hoverVars without label", {
      
      plOutput <- barplotClinData(
          data = dataPlot, 
          xVar = "AEDECOD", yVar = "n",
          hoverVars = c("AEDECOD", "n")
      )
      expect_is(plOutput, "plotly")
      
    })

test_that("x-variable non nested in color variable is created with success for a stack barplot", {
		
	data <- data.frame(
		ANRIND = c("Low", "Normal", "High", "Normal"),
		AEDECOD = c("a", "a", "b", "b"),
		n = c(2, 3, 4, 1)
	)
	# create plot
	expect_warning(
		pl <- barplotClinData(
			data = data,
			xVar = "AEDECOD", colorVar = "ANRIND",
			yVar = "n",
			barmode = "stack"
		),
		"ordering of the x-variable"
	)
	
})

test_that("A text variable is correctly displayed in the barplot", {
			
	# create plot
	pl <- barplotClinData(
		data = dataPlot,
		xVar = "AEDECOD",
		yVar = "n", 
		textVar = "%"
	)
	
	# extract data from output object
	plData <- plotly_build(pl)$x$data
	
	# only bar aes
	plDataBar <- plData[sapply(plData, function(x) x$type == "bar")]
	
	# format plot data
	plDataBar <- lapply(plDataBar, `[`, c("x", "y", "text"))
	plDataBar <- lapply(plDataBar, cbind.data.frame)
	plDataBar <- do.call(rbind, plDataBar)
	plDataBar <- plDataBar[do.call(order, plDataBar), ]
	# text is converted to factor by plotly:
	plDataBar[["text"]] <- as.character(plDataBar[["text"]])
	
	dataRef <- dataPlot[, c("AEDECOD" ,"n", "%")]
	dataRef <- dataRef[do.call(order, dataRef), ]
	
	expect_equal(
		object = plDataBar, 
		expected = dataRef, 
		checkNames = FALSE,
		check.attributes = FALSE # ignore row.names
	)
	
})

