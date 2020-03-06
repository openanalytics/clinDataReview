library(glpgUtilityFct)

data(SDTMDataPelican)
data(labelVarsSDTMPelican)

dataAE <- SDTMDataPelican$AE
dataDM <- SDTMDataPelican$DM
labelVars <- labelVarsSDTMPelican

## example of basic treemap:

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

# reformat summary statistics data.frame to link child <-> parent node
dataPlot['parent'] = with(dataPlot, 
	ifelse(AEDECOD == "Total", 'Adverse events', as.character(AESOC))
)
dataPlot['child'] = with(dataPlot, 
	ifelse(AEDECOD == "Total", as.character(AESOC), as.character(AEDECOD))
)

# create plot
treemapMonitoring(
	data = dataPlot,
	parentVar = "parent", parentLab = getLabelVar(var = "AESOC", labelVars = labelVars),
	childVar = "child", childLab = getLabelVar(var = "AEDECOD", labelVars = labelVars),
	valueVar = "n", valueLab = "Number of patients with adverse events"
)