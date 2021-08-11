library(clinUtils)

data(dataADaMCDISCP01)
labelVars <- attr(dataADaMCDISCP01, "labelVars")

## Summary plot with vertical error bars

dataVSDIABP <- subset(dataADaMCDISCP01$ADVS, 
	PARAMCD == "DIABP" & ANL01FL == "Y" &
	AVISIT %in% c("Baseline", "Week 2", "Week 4", "Week 6", "Week 8")
)

# compute summary statistics by visit
library(inTextSummaryTable)
summaryTableVSDIABP <- computeSummaryStatisticsTable(
	data = dataVSDIABP,
	rowVar = c("AVISIT", "ATPT"),
	var = "AVAL",
	stats = getStats(c("n", "Mean", "SE")),
	labelVars = labelVars
)
dataPlot <- subset(summaryTableVSDIABP, !isTotal)

errorbarClinData(
	data = dataPlot,
	xVar = "AVISIT",
	colorVar = "ATPT",
	# use non-rounded statistics for the plot
	yVar = "statMean", yErrorVar = "statSE", 
	yLab = "Mean", yErrorLab = "Standard Error",
	# include lines connecting the error bars
	mode = "markers+lines",
	labelVars = labelVars
)

# add number of subjects in labels
dataPlot$nSubj <- with(dataPlot, paste0("N=", n))
errorbarClinData(
	data = dataPlot,
	xVar = "AVISIT", 
	xLabVars = c("AVISIT", "nSubj"),
	colorVar = "ATPT",
	yVar = "statMean", yLab = "Mean",
	yErrorVar = "statSE", yErrorLab = "Standard error",
	mode = "markers+lines",
	title = "Diastolic Blood Pressure summary profile by actual visit and and analysis timepoint",
	labelVars = labelVars
)

## Summary plot with horizontal error bars

# Data of interest: ratio from baseline at week 16
dataLBW8 <- subset(dataADaMCDISCP01$ADLBC, grepl("Week 8", AVISIT))
dataLBW8$R2BASE <- with(dataLBW8, AVAL/BASE) # compute ratio
dataLBW8 <- subset(dataLBW8, !is.na(R2BASE))

# compute summary statistics of the ratio per baseline per parameter
library(inTextSummaryTable)
summaryTableLBW8 <- computeSummaryStatisticsTable(
	data = dataLBW8,
	var = "R2BASE",
	rowVar = "PARAM",
	colVar = "TRTA",
	stats = getStats(x = dataLBW8$R2BASE, type = c("n", "Mean", "SD"))
)
dataPlot <- subset(summaryTableLBW8, !isTotal)
errorbarClinData(
	data = dataPlot,
	xVar = "statMedian", xErrorVar = "statSD",
	xLab = "Median", xErrorLab = "Standard deviation",
	xAxisLab = "Ratio from baseline (Mean +- SD)",
	yVar = "PARAM",
	colorVar = "TRTA",
	labelVars = labelVars,
	title = "Summary ratio from baseline at week 8 by treatment"
)

