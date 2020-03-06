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
	outputType = "data.frame"

)

dataSunburst <- tableAE

dataSunburst$n <- as.numeric(dataSunburst$n)

# reformat summary statistics data.frame to link child <-> parent node
dataSunburst['parent'] = with(dataSunburst, 
	ifelse(AEDECOD == "Total", 'Adverse events', as.character(AESOC))
)
dataSunburst['child'] = with(dataSunburst, 
	ifelse(AEDECOD == "Total", as.character(AESOC), as.character(AEDECOD))
)

# create plot
sunburstMonitoring(
	data = dataSunburst,
	parentVar = "parent", parentLab = getLabelVar(var = "AESOC", labelVars = labelVars),
	childVar = "child", childLab = getLabelVar(var = "AEDECOD", labelVars = labelVars),
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
	outputType = "data.frame"
)

# reformat summary statistics data.frame to link child <-> parent node
tableDM['parent'] = with(tableDM, 
	ifelse(SITEID == "Total", 'All', as.character(COUNTRY))
)
tableDM['child'] = with(tableDM, 
	ifelse(SITEID == "Total", as.character(COUNTRY), as.character(SITEID))
)
tableDM$statN <- as.numeric(tableDM$statN)

# create the plot
sunburstMonitoring(
	data = tableDM,
	parentVar = "parent", parentLab = "country",
	childVar = "child", childLab = "side identifier",
	valueVar = "statN", valueLab = "Counts of patients",
	valueType = "total",
)