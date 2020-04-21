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
	outputType = "data.frame",
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
	pl <- barplotMonitoring(
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
			data.frame(AEDECOD = as.character(x[["x"]]), n = x$y)
		)
	)
	
	dataPlot <- dataPlot[, c("AEDECOD", "n")]
	expect_equivalent(object = plDataBarDf, expected = dataPlot)
	
	## check if created plot == reference
	expect_doppelganger(title = "basic", fig = pl, writer = write_svg_plotly)

})

test_that("interactive table is created", {
			
	res <- barplotMonitoring(
		data = dataPlot,
		xVar = "AEDECOD", yVar = "n",
		table = TRUE
	)
	
	expect_is(res$table, "datatables")
	
})
