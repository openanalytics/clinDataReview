library(glpgUtilityFct)

data(SDTMDataPelican)
data(labelVarsSDTMPelican)

dataAE <- SDTMDataPelican$AE
dataDM <- SDTMDataPelican$DM
labelVars <- labelVarsSDTMPelican

library(plyr)

## basic treemap:

# treemap takes as input table with counts
library(inTextSummaryTable)

# total counts: Safety Analysis Set (patients with start date for the first treatment)
dataTotal <- subset(dataDM, RFXSTDTC != "")

# compute adverse event table
tableAE <- getSummaryStatisticsTable(
		
	data = dataAE,
	rowVar = c("AESOC", "AEDECOD"),
	dataTotal = dataTotal,
	rowOrder = "total",
	labelVars = labelVars,
	stats = getStats("count"),
	
	# plotly treemap requires records (rows) for each group
	rowVarTotalInclude = "AEDECOD",
	outputType = "data.frame"

)

dataPlot <- tableAE

dataPlot$n <- as.numeric(dataPlot$n)

# create plot
treemapMonitoring(
	data = dataPlot,
	vars = c("AESOC", "AEDECOD"),
	valueVar = "n", valueLab = "Number of patients with adverse events"
)

## treemap with coloring

# extract worst-case scenario
dataAE$AESEVN <- as.numeric(factor(dataAE$AESEV, levels = c("MILD", "MODERATE")))
if(any(is.na(dataAEWC$AESEVN)))
	stop("Severity should be filled for all subjects.")

dataAEWC <- ddply(dataAE, c("AESOC", "AEDECOD", "USUBJID"), function(x){
	x[which.max(x$AESEVN), ]
})
dataTotalRow <- list(AEDECOD = 
	ddply(dataAEWC, c("AESOC", "USUBJID"), function(x){
		x[which.max(x$AESEVN), ]
	})
)


# compute adverse event table
tableAE <- getSummaryStatisticsTable(
		
	data = dataAEWC,
	rowVar = c("AESOC", "AEDECOD"),
	var = "AESEVN",
	dataTotal = dataTotal,
	rowOrder = "total",
	labelVars = labelVars,
		
	# plotly treemap requires records (rows) for each group
	rowVarTotalInclude = "AEDECOD",
	dataTotalRow = dataTotalRow,
	outputType = "data.frame"

)

dataPlot <- tableAE

dataPlot$statN <- as.numeric(dataPlot$statN)
dataPlot$statMean <- as.numeric(dataPlot$statMean)

# create plot
treemapMonitoring(
	data = dataPlot,
	vars = c("AESOC", "AEDECOD"),
	valueVar = "statN", valueLab = "Number of patients with adverse events",
	colorVar = "statMean", colorLab = "Mean severity"
)