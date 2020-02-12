library(glpgUtilityFct)

data(SDTMDataPelican)
data(labelVarsSDTMPelican)

dataLB <- SDTMDataPelican$LB
dataDM <- SDTMDataPelican$DM
dataLB <- annotateData(dataLB, annotations = list(data = dataDM))
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

# patient profiles:

dataPlot <- subset(dataLB, LBTESTCD == "ALT")
visitLab <- with(dataPlot, tapply(LBDY, VISIT, median))
names(visitLab) <- sub("-", "\n", names(visitLab))

scatterplotMonitoring(
	data = dataPlot, 
	xVar = "LBDY", yVar = "LBSTRESN",
	aesPointVar = list(color = "ACTARM", shape = "USUBJID"),
	aesLineVar = list(group = "USUBJID", color = "ACTARM"),
	hoverVar = c("USUBJID", "VISIT", "LBDY", "LBSTRESN", "COUNTRY", "ACTARM"),
	labelVars = labelVars,
	xPars = list(breaks = visitLab, labels = names(visitLab)),
	themePars = list(legend.position = "none"),
	title = paste("Actual value of", 
		getLabelParamcd(paramcd = "ALT", data = dataLB, paramcdVar = "LBTESTCD", paramVar = "LBTEST")
	)
)

