library(glpgUtilityFct)

data(SDTMDataPelican)
data(labelVarsSDTMPelican)

dataAE <- SDTMDataPelican$AE
dataDM <- SDTMDataPelican$DM
labelVars <- labelVarsSDTMPelican

## example of basic sunburst:

# sunburst takes as input table with counts
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
	outputType = "data.frame-base"

)

dataSunburst <- tableAE

dataSunburst$n <- as.numeric(dataSunburst$n)

# create plot
sunburstMonitoring(
	data = dataSunburst,
	vars = c("AESOC", "AEDECOD"),
	valueVar = "n", valueLab = "Number of patients with adverse events"
)

## example where sum(counts) of child = counts of parent

# counts of patients per country/site
tableDM <- getSummaryStatisticsTable(
	data = dataDM,
	rowVar = c("COUNTRY", "SITEID"),
	labelVars = labelVars,
	# plotly treemap requires records (rows) for each group
	rowVarTotalInclude = "SITEID",
	rowTotalInclude = TRUE,
	outputType = "data.frame-base"
)
tableDM$statN <- as.numeric(tableDM$statN)

# create the plot
sunburstMonitoring(
	data = tableDM,
	vars = c("COUNTRY", "SITEID"),
	valueVar = "statN", valueLab = "Counts of patients",
	valueType = "total"
)