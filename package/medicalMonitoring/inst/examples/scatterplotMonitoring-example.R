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
	facetPars = list(facets = "VISIT"),
	labelVars = labelVars
)

# scatterplot with all visits, link subjects
xLab <- getLabelParamcd(paramcd = "ALT", data = dataLB, 
	paramcdVar = "LBTESTCD", paramVar = "LBTEST")
yLab <- getLabelParamcd(paramcd = "ALB", data = dataLB, 
	paramcdVar = "LBTESTCD", paramVar = "LBTEST")
scatterplotMonitoring(
	data = dataPlotWide, 
	xVar = "ALT", yVar = "ALB",
	xLab = xLab,
	yLab = yLab,
	aesPointVar = list(color = "VISIT", shape = "VISIT"),
	aesLineVar = list(group = "USUBJID", linetype = "VISIT"),
	labelVars = labelVars
)

# scatterplot of different visits versus baseline

# add baseline as extra column:
dataPlot <- subset(dataLB, LBTESTCD == "ALT")
dataPlotBL <- subset(dataPlot, VISIT == "Screening (D-28 to D-1)")
dataPlotBL <- dataPlotBL[with(dataPlotBL, order(USUBJID, -LBDY)), ]
dataPlotBL <- dataPlotBL[!duplicated(dataPlotBL$USUBJID), ]
dataPlot$LBSTRESNBL <- dataPlot[match(dataPlot$USUBJID, dataPlotBL$USUBJID), "LBSTRESN"]

# sort visits:
dataPlot$VISIT <- with(dataPlot, reorder(VISIT, VISITNUM))

xLab <- paste(labelVars["LBSTRESN"], "for last screening visit")
yLab <- paste(labelVars["LBSTRESN"], "at visit X")
paramLab <- getLabelParamcd(paramcd = "ALT", data = dataLB, 
	paramcdVar = "LBTESTCD", paramVar = "LBTEST")
scatterplotMonitoring(
	data = dataPlot, 
	xVar = "LBSTRESNBL", xLab = xLab,
	yVar = "LBSTRESN", yLab = yLab,
	aesPointVar = list(color = "USUBJID"),
	aesLineVar = list(group = "USUBJID", color = "USUBJID"),
	hoverVars = c("USUBJID", "VISIT", "LBDY", "LBSTRESN", "COUNTRY", "ACTARM"),
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
		list(xintercept = "LBSTNRLO", linetype = 2, color = "orange"),
		list(yintercept = "LBSTNRLO", linetype = 2, color = "orange"),
		list(xintercept = "LBSTNRHI", linetype = 2, color = "orange"),
		list(yintercept = "LBSTNRHI", linetype = 2, color = "orange", 
			label = "Reference Range Upper Limit")
	)
)

