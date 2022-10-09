library(clinUtils)

data(dataADaMCDISCP01)
labelVars <- attr(dataADaMCDISCP01, "labelVars")

dataAE <- dataADaMCDISCP01$ADAE
dataDM <- dataADaMCDISCP01$ADSL

## example of basic sunburst:

# sunburst takes as input table with counts
if (requireNamespace("inTextSummaryTable", quietly = TRUE)) {

# total counts: Safety Analysis Set (patients with start date for the first treatment)
dataTotal <- subset(dataDM, RFSTDTC != "")

# compute adverse event table
tableAE <- inTextSummaryTable::getSummaryStatisticsTable(
		
	data = dataAE,
	rowVar = c("AESOC", "AEDECOD"),
	dataTotal = dataTotal,
	rowOrder = "total",
	labelVars = labelVars,
	stats = inTextSummaryTable::getStats("count"),
	
	# plotly treemap requires records (rows) for each group
	rowVarTotalInclude = "AEDECOD",
	outputType = "data.frame-base"

)

dataSunburst <- tableAE

dataSunburst$n <- as.numeric(dataSunburst$n)

# create plot
sunburstClinData(
	data = dataSunburst,
	vars = c("AESOC", "AEDECOD"),
	valueVar = "n",
    valueLab = "Number of patients with adverse events"
)

## example where sum(counts) of child = counts of parent

# counts of patients per arm/site
tableDM <- inTextSummaryTable::getSummaryStatisticsTable(
	data = dataDM,
	rowVar = c("ARM", "SITEID"),
	labelVars = labelVars,
	# plotly treemap requires records (rows) for each group
	rowVarTotalInclude = "SITEID",
	rowTotalInclude = TRUE,
	outputType = "data.frame-base"
)
tableDM$statN <- as.numeric(tableDM$statN)

# create the plot
sunburstClinData(
	data = tableDM,
	vars = c("ARM", "SITEID"),
	valueVar = "statN", valueLab = "Counts of patients",
	valueType = "total",
	caption = "The sectors are colored by category.",
	subtitle = "Group: treatment and site"
)

}