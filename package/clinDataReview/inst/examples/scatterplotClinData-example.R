library(clinUtils)

data(dataADaMCDISCP01)
labelVars <- attr(dataADaMCDISCP01, "labelVars")

dataLB <- dataADaMCDISCP01$ADLBC
dataDM <- dataADaMCDISCP01$ADSL
dataLB <- annotateData(dataLB, annotations = list(data = dataDM))

## pairwise comparison plot of two parameters of interest:

# format data long -> wide format (one column per lab param)
dataPlot <- subset(dataLB, PARAMCD %in% c("ALT", "AST"))
library(reshape2)
dataPlotWide <- dcast(
	data = dataPlot,
	formula = USUBJID + VISIT + VISITNUM ~ PARAMCD, 
	value.var = "LBSTRESN",
	fun.aggregate = mean
)

# scatterplot per visit
scatterplotClinData(
	data = dataPlotWide, 
	xVar = "ALT", yVar = "AST",
	aesPointVar = list(color = "USUBJID"),
	themePars = list(legend.position = "none"),
	facetPars = list(facets = "VISIT"),
	labelVars = labelVars
)

# scatterplot with all visits, link subjects
xLab <- getLabelParamcd(paramcd = "ALT", data = dataLB, 
	paramcdVar = "PARAMCD", paramVar = "PARAM")
yLab <- getLabelParamcd(paramcd = "AST", data = dataLB, 
	paramcdVar = "PARAMCD", paramVar = "PARAM")
scatterplotClinData(
	data = dataPlotWide, 
	xVar = "ALT", yVar = "AST",
	xLab = xLab,
	yLab = yLab,
	aesPointVar = list(color = "VISIT", shape = "VISIT"),
	aesLineVar = list(group = "USUBJID", linetype = "VISIT"),
	labelVars = labelVars
)

# scatterplot of different visits versus baseline

# add baseline as extra column:
dataPlot <- subset(dataLB, PARAMCD == "ALT")
dataPlotBL <- subset(dataPlot, VISIT == "SCREENING 1")
dataPlotBL <- dataPlotBL[with(dataPlotBL, order(USUBJID, -ADY)), ]
dataPlotBL <- dataPlotBL[!duplicated(dataPlotBL$USUBJID), ]
dataPlot$LBSTRESNBL <- dataPlot[match(dataPlot$USUBJID, dataPlotBL$USUBJID), "LBSTRESN"]

# sort visits:
dataPlot$VISIT <- with(dataPlot, reorder(VISIT, VISITNUM))

xLab <- paste(labelVars["LBSTRESN"], "for last screening visit")
yLab <- paste(labelVars["LBSTRESN"], "at visit X")
paramLab <- getLabelParamcd(paramcd = "ALT", data = dataLB, 
	paramcdVar = "PARAMCD", paramVar = "PARAM")
scatterplotClinData(
	data = dataPlot, 
	xVar = "LBSTRESNBL", xLab = xLab,
	yVar = "LBSTRESN", yLab = yLab,
	aesPointVar = list(color = "USUBJID"),
	aesLineVar = list(group = "USUBJID", color = "USUBJID"),
	hoverVars = c("USUBJID", "VISIT", "ADY", "LBSTRESN"),
	labelVars = labelVars,
	facetPars = list(facets = "VISIT"),
	themePars = list(legend.position = "none"),
	title = paste("Comparison of actual value of", 
		paramLab,
		"at each visit versus baseline"
	),
	refLinePars = list(
		list(slope = 1, intercept = 0, linetype = 1, color = "black", 
			label = FALSE),
		list(xintercept = "A1LO", linetype = 2, color = "orange"),
		list(yintercept = "A1LO", linetype = 2, color = "orange"),
		list(xintercept = "A1HI", linetype = 2, color = "orange"),
		list(yintercept = "A1HI", linetype = 2, color = "orange", 
			label = "Reference Range Upper Limit")
	)
)

