library(glpgUtilityFct)

data(SDTMDataPelican)
data(labelVarsSDTMPelican)

dataLB <- SDTMDataPelican$LB
data(SDTMDataPelican)
labelVars <- labelVarsSDTMPelican

## pairwise comparison plot of two parameters of interest:

# format data long -> wide format (one column per lab param)
dataPlot <- subset(dataLB, LBTESTCD %in% c("ALT", "ALB"))
library(reshape2)
dataPlotWide <- dcast(
	data = dataPlot,
	formula = USUBJID + VISIT + VISITNUM ~ LBTESTCD, 
	value.var = "LBSTRESN",
	fun.aggregate = mean
)

# scatterplot per visit
scatterplotMonitoring(
	data = dataPlotWide, 
	xVar = "ALT", yVar = "ALB",
	aesPointVar = list(color = "USUBJID"),
	themePars = list(legend.position = "none"),
	facetPars = list(facets = ~ VISIT),
	labelVars = labelVars, hoverVar = NULL
)

# scatterplot with all visits, link subjects
scatterplotMonitoring(
	data = dataPlotWide, 
	xVar = "ALT", yVar = "ALB",
	xLab = getLabelParamcd(paramcd = "ALT", data = dataLB, paramcdVar = "LBTESTCD", paramVar = "LBTEST"),
	yLab = getLabelParamcd(paramcd = "ALB", data = dataLB, paramcdVar = "LBTESTCD", paramVar = "LBTEST"),
	aesPointVar = list(color = "VISIT", shape = "VISIT"),
	aesLineVar = list(group = "USUBJID"),
	labelVars = labelVars
)

