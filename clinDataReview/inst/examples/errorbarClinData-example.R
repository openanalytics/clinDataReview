library(clinUtils)

data(dataADaMCDISCP01)
labelVars <- attr(dataADaMCDISCP01, "labelVars")

## Summary plot with vertical error bars

dataVSDIABP <- subset(dataADaMCDISCP01$ADVS, 
	PARAMCD == "DIABP" & ANL01FL == "Y" &
	AVISIT %in% c("Baseline", "Week 2", "Week 4", "Week 6", "Week 8")
)

# compute summary statistics by visit
if (requireNamespace("inTextSummaryTable", quietly = TRUE)) {
	
summaryTableVSDIABP <- inTextSummaryTable::computeSummaryStatisticsTable(
	data = dataVSDIABP,
	rowVar = c("AVISIT", "ATPT"),
	var = "AVAL",
	stats = inTextSummaryTable::getStats(c("n", "Mean", "SE")),
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
	title = paste("Diastolic Blood Pressure summary profile by actual visit",
		"and analysis timepoint"),
	labelVars = labelVars
)

## Add a selection box
if(interactive()){
  summaryTable <- inTextSummaryTable::computeSummaryStatisticsTable(
    data = subset(dataADaMCDISCP01$ADVS, 
      ANL01FL == "Y" &
      AVISIT %in% c("Baseline", "Week 2", "Week 4", "Week 6", "Week 8")
    ),
    rowVar = c("PARAM", "AVISIT", "ATPT"),
    var = "AVAL",
    stats = inTextSummaryTable::getStats(c("Mean", "SE")),
    labelVars = labelVars
  )
  dataPlot <- subset(summaryTable, !isTotal)
  errorbarClinData(
    data = dataPlot,
    xVar = "AVISIT", 
    colorVar = "ATPT",
    yVar = "statMean", yLab = "Mean",
    yErrorVar = "statSE", yErrorLab = "Standard error",
    mode = "markers+lines",
    title = paste("Lab parameters summary profile by actual visit",
      "and analysis timepoint"),
    labelVars = labelVars,
    selectVars = "PARAM"
  )
}

## Summary plot with horizontal error bars

# Data of interest: ratio from baseline at week 16
dataLBW8 <- subset(dataADaMCDISCP01$ADLBC, grepl("Week 8", AVISIT))
# compute ratio from baseline
dataLBW8$R2BASE <- with(dataLBW8, AVAL/BASE) 
dataLBW8 <- subset(dataLBW8, !is.na(R2BASE))
# Order actual treatments
dataLBW8$TRTA <- with(dataLBW8, reorder(TRTA, TRTAN))

# compute summary statistics of the ratio per baseline per parameter
summaryTableLBW8 <- inTextSummaryTable::computeSummaryStatisticsTable(
	data = dataLBW8,
	var = "R2BASE",
	rowVar = "PARAM",
	colVar = "TRTA",
	stats = inTextSummaryTable::getStats(x = dataLBW8$R2BASE, type = c("n", "Median", "SD"))
)
dataPlot <- subset(summaryTableLBW8, !isTotal)
# extract direction of ratio
dataPlot$dir <- factor(
	ifelse(dataPlot$statMedian >= 1, "Increase", "Decrease"),
	levels = c("Decrease", "Increase")
)
# compute relative ratio (percentage above 1)
dataPlot$statMedianRelative <- with(dataPlot,
	ifelse(statMedian < 1, 1/statMedian, statMedian)
)
# order based on mean relative ratio across treatment arms
params <- names(sort(with(dataPlot, tapply(statMedianRelative, PARAM, mean))))
dataPlot$PARAM <- factor(dataPlot$PARAM, levels = params)
errorbarClinData(
	data = dataPlot,
	xVar = "statMedianRelative", xErrorVar = "statSD",
	xLab = "Median", xErrorLab = "Standard deviation",
	xAxisLab = "Relative ratio from baseline (Median +- SD)",
	yVar = "PARAM",
	colorVar = "TRTA",
	shapeVar = "dir", shapeLab = "Direction of ratio",
	shapePalette = c(`Decrease` = 25, `Increase` = 24),
	size = 10,
	labelVars = labelVars,
	title = "Summary ratio from baseline at week 8 by treatment"
)

}

