context("Sunburst monitoring")

# load example data
library(glpgUtilityFct)
data(SDTMDataPelican)
data(labelVarsSDTMPelican)
dataDM <- SDTMDataPelican$DM
dataAE <- SDTMDataPelican$AE
labelVars <- labelVarsSDTMPelican

# sunburst takes as input table with counts
library(inTextSummaryTable)

# total counts: Safety Analysis Set (patients with start date for the first treatment)
dataTotal <- subset(dataDM, RFXSTDTC != "")

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

dataSunburst <- subset(tableAE, !isTotal)

dataSunburst$n <- as.numeric(dataSunburst$n)

test_that("plotting function runs properly", {
			
	pl <- sunburstMonitoring(
		data = dataSunburst,
		vars = c("AESOC", "AEDECOD"),
		valueVar = "n", valueLab = "Number of patients with adverse events"
	)
	
	## check if created plot == reference
	expect_doppelganger(title = "basic", fig = pl, writer = write_svg_plotly)

})

test_that("interactive table is created", {
			
	res <- sunburstMonitoring(
		data = dataSunburst,
		vars = c("AESOC", "AEDECOD"),
		valueVar = "n", valueLab = "Number of patients with adverse events",
		table = TRUE
	)
	
	expect_is(res$table, "datatables")
	
})

test_that("plotting function - total", {
			
	# returns a warning: valueType: 'total' -> 'relative'
	expect_warning(
		pl <- sunburstMonitoring(
			data = dataSunburst,
			vars = c("AESOC", "AEDECOD"), valueVar = "n", 
			valueType = "total"
		),
		regexp = "'valueType' is set to 'relative' (instead of 'total')",
		fixed = TRUE
	)
	
	# successful example of the 'total'
	tableDM <- getSummaryStatisticsTable(
		data = dataDM,
		rowVar = c("COUNTRY", "SITEID"),
		labelVars = labelVars,
		# plotly treemap requires records (rows) for each group
		rowVarTotalInclude = "SITEID",
		rowTotalInclude = TRUE,
		outputType = "data.frame-base"
	)
	tableDM <- subset(tableDM, !isTotal)
	
	tableDM$statN <- as.numeric(tableDM$statN)
	expect_silent({
		sunburstMonitoring(
			data = tableDM,
			vars = c("COUNTRY", "SITEID"), valueVar = "statN", 
			valueType = "total"
		)
	})
	
})
			
