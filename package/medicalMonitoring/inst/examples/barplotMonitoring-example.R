library(glpgUtilityFct)

data(SDTMDataPelican)
data(labelVarsSDTMPelican)

dataAE <- SDTMDataPelican$AE
dataDM <- SDTMDataPelican$DM
labelVars <- labelVarsSDTMPelican

## example of basic barplot:

# treemap takes as input table with counts
library(inTextSummaryTable)

# total counts: Safety Analysis Set (patients with start date for the first treatment)
dataTotal <- subset(dataDM, RFXSTDTC != "")

# compute adverse event table
tableAE <- getSummaryStatisticsTable(
	data = dataAE,
	rowVar = c("AEBODSYS", "AEDECOD"),
	dataTotal = dataTotal,
	labelVars = labelVars,
	stats = getStats("count"),
	outputType = "data.frame-base"
)

dataPlot <- subset(tableAE, AEDECOD != "Total")

dataPlot$n <- as.numeric(dataPlot$n)

# create plot
barplotMonitoring(
	data = dataPlot,
	xVar = "AEDECOD", 
	yVar = "n", yLab = "Number of patients with adverse events",
	labelVars = labelVars
)

# stacked barplot
barplotMonitoring(
	data = dataPlot,
	xVar = "AEBODSYS", 
	yVar = "n", yLab = "Number of patients with adverse events",
	labelVars = labelVars,
	colorVar = "AEDECOD", barmode = "stack"
)