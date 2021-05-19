library(clinUtils)

data(dataADaMCDISCP01)
labelVars <- attr(dataADaMCDISCP01, "labelVars")

dataAE <- dataADaMCDISCP01$ADAE
dataDM <- dataADaMCDISCP01$ADSL

## example of basic barplot:

# treemap takes as input table with counts
library(inTextSummaryTable)

# total counts: Safety Analysis Set (patients with start date for the first treatment)
dataTotal <- subset(dataDM, RFSTDTC != "")

# compute adverse event table
tableAE <- computeSummaryStatisticsTable(
	data = dataAE,
	rowVar = c("AEBODSYS", "AEDECOD"),
	dataTotal = dataTotal,
	labelVars = labelVars,
	stats = getStats("count")
)

dataPlot <- subset(tableAE, AEDECOD != "Total")

dataPlot$n <- as.numeric(dataPlot$n)

# create plot
barplotClinData(
	data = dataPlot,
	xVar = "AEDECOD", 
	yVar = "n", yLab = "Number of patients with adverse events",
	labelVars = labelVars
)

# display percentage of events per severity
dataPlot <- computeSummaryStatisticsTable(
	data = dataAE,
	rowVar = c("AEDECOD", "AESEV"),
	dataTotal = dataTotal,
	labelVars = labelVars,
	statsPerc = "statm",
	stats = getStats("%m"),
	dataTotalPerc = dataAE,
	rowVarTotalPerc = "AEDECOD"
)
barplotClinData(
	data = dataPlot,
	xVar = "AEDECOD", 
	yVar = "statPercm", yLab = "Percentage of adverse events",
	labelVars = labelVars,
	colorVar = "AESEV", barmode = "stack",
	hoverVar = c("AEDECOD", "AESEV", "statN", "statm", "statPercm"),
	hoverLab = c(
		labelVars["AEDECOD"],
		labelVars["AESEV"],
		statN = "Number of patients",
		statm = "Number of events",
		statPercm = "Percentage of events"
	)
)