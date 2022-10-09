library(clinUtils)

data(dataADaMCDISCP01)
labelVars <- attr(dataADaMCDISCP01, "labelVars")

dataAE <- dataADaMCDISCP01$ADAE
dataDM <- dataADaMCDISCP01$ADSL

## example of basic barplot:

# treemap takes as input table with counts

if (requireNamespace("inTextSummaryTable", quietly = TRUE)) {

# total counts: Safety Analysis Set (patients with start date for the first treatment)
dataTotal <- subset(dataDM, RFSTDTC != "")

# compute adverse event table

tableAE <- inTextSummaryTable::computeSummaryStatisticsTable(
	data = dataAE,
	rowVar = c("AEBODSYS", "AEDECOD"),
	rowOrder = "total",
	dataTotal = dataTotal,
	labelVars = labelVars,
	stats = inTextSummaryTable::getStats("count")
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

# add number on top of the bars
barplotClinData(
	data = dataPlot,
	xVar = "AEDECOD", 
	yVar = "n", yLab = "Number of patients with adverse events",
	textVar = "n",
	labelVars = labelVars
)

# add a selection box
if(interactive()){
  barplotClinData(
    data = dataPlot,
    xVar = "AEDECOD", 
    yVar = "n", yLab = "Number of patients with adverse events",
    labelVars = labelVars,
    selectVars = "AEBODSYS"
  )
}

\dontrun{

# display percentage of events per severity
tableAEBySeverity <- inTextSummaryTable::computeSummaryStatisticsTable(
	data = dataAE,
	rowVar = c("AEDECOD", "AESEV"),
	dataTotal = dataTotal,
	labelVars = labelVars,
	statsPerc = "statm",
	stats = inTextSummaryTable::getStats("%m"),
	dataTotalPerc = dataAE,
	rowVarTotalPerc = "AEDECOD"
)
barplotClinData(
	data = tableAEBySeverity,
	xVar = "AEDECOD", xLab = "Adverse event term",
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
	),
	textVar = "%m",
	# add subtitle
	subtitle = "Group: severity"
)

}

}