library(clinUtils)

data(dataADaMCDISCP01)
labelVars <- attr(dataADaMCDISCP01, "labelVars")

dataLB <- dataADaMCDISCP01$ADLBC
dataDM <- dataADaMCDISCP01$ADSL
dataLB <- annotateData(dataLB, annotations = list(data = dataDM))
# subset of the data for the example
dataLB <- subset(dataLB, VISIT %in% c("SCREENING 1", "WEEK 2", "WEEK 8"))

## time profile

dataPlot <- subset(dataLB, PARAMCD == "ALT")

# with relative day
scatterplotClinData(
	data = dataPlot, 
	xVar = "ADY",
	yVar = "LBSTRESN",
	aesPointVar = list(color = "TRTP", fill = "TRTP"),
	aesLineVar = list(group = "USUBJID", color = "TRTP"),
	labelVars = labelVars
)

# with actual visit
dataPlot$AVISIT <- with(dataPlot, reorder(trimws(AVISIT), AVISITN))
scatterplotClinData(
	data = dataPlot, 
	xVar = "AVISIT",
	yVar = "LBSTRESN",
	aesPointVar = list(color = "TRTP", fill = "TRTP"),
	aesLineVar = list(group = "USUBJID", color = "TRTP"),
	labelVars = labelVars
)



\dontrun{

# add number of subjects below each visit
	
if (requireNamespace("inTextSummaryTable", quietly = TRUE)) {

# compute number of subjects by visit
summaryTable <- inTextSummaryTable::computeSummaryStatisticsTable(
	dataPlot,
	rowVar = "AVISIT",
	stats = "n"
)
# add it in the data
dataPlot <- merge(dataPlot, summaryTable[, c("AVISIT", "n")], all.x = TRUE)
dataPlot$n <- paste0("N=", dataPlot$n)

scatterplotClinData(
	data = dataPlot, 
	xVar = "AVISIT", xLabVars = c("AVISIT", "n"),
	yVar = "LBSTRESN",
	aesPointVar = list(color = "TRTP", fill = "TRTP"),
	aesLineVar = list(group = "USUBJID", color = "TRTP"),
	labelVars = labelVars
)

}

}

## pairwise comparison plot of two parameters of interest:

# format data long -> wide format (one column per lab param)
dataPlot <- subset(dataLB, PARAMCD %in% c("ALT", "AST"))
dataPlot <- stats::aggregate(
	LBSTRESN ~ USUBJID + VISIT + VISITNUM + PARAMCD, 
	data = dataPlot,
	FUN = mean
)
dataPlotWide <- stats::reshape(
	data = dataPlot,
	timevar = "PARAMCD", idvar = c("USUBJID", "VISIT", "VISITNUM"),
	direction = "wide"
)
colnames(dataPlotWide) <- sub("^LBSTRESN.", "", colnames(dataPlotWide))
# scatterplot per visit
scatterplotClinData(
	data = dataPlotWide, 
	xVar = "ALT", yVar = "AST",
	aesPointVar = list(color = "USUBJID", fill = "USUBJID"),
	themePars = list(legend.position = "none"),
	facetPars = list(facets = "VISIT"),
	labelVars = labelVars,
	subtitle = "Visualization is split by visit",
	caption = "Points are colored by subject ID"
)

\dontrun{

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
	aesPointVar = list(color = "VISIT", fill = "VISIT"),
	aesLineVar = list(group = "USUBJID"),
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
	aesPointVar = list(color = "USUBJID", fill = "USUBJID"),
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
		list(yintercept = "A1LO", linetype = 2, color = "blue"),
		list(yintercept = "A1HI", linetype = 2, color = "purple", 
			label = "Reference Range Upper Limit")
	)
)


## scatterplot with smoothing layer

data <- data.frame(
  subj = c(rep('subj1', 20), rep('subj2', 20)),
  time = rep( 1:20 , 2 ),
  response =  c(1:20, 50:31) + runif(min =-3, max = +3, 40),
  treat =  rep(c('trA', 'trB'), 20),
  stringsAsFactors = FALSE
)

# smoothing per subject
smoothPlot <- scatterplotClinData(
  data = data,
  xVar = "time", yVar = "response",
  aesPointVar = list(color = "treat"),
  aesLineVar = list(group = 'subj'),
  linePars = list(linetype='dotted'),
  aesSmoothVar = list(color='subj', group='subj'), 
  smoothPars =  list(alpha=0.5, size=0.3 , se=TRUE, color = 'black')
)
smoothPlot


# plot smoothing over subjects
smoothPlot <- scatterplotClinData(
  data = data,
  xVar = "time", yVar = "response",
  aesPointVar = list(color = "treat"),
  aesLineVar = list(group = 'subj'),
  linePars = list(linetype='dotted'),
  aesSmoothVar = list(), 
  smoothPars =  list(alpha=0.5, size=0.3 , se=TRUE, color = 'black')
)
smoothPlot

}

# add a selection box
if(interactive()){
  dataPlot <- subset(dataLB, PARAMCD == "ALT")
  dataPlot$TRTA <- with(dataPlot, reorder(TRTA, TRTAN))
  scatterplotClinData(
    data = dataPlot, 
    xVar = "ADY",
    yVar = "LBSTRESN",
    aesPointVar = list(fill = "TRTA", color = "TRTA"),
    aesLineVar = list(group = "USUBJID", color = "TRTA"),
    selectVars = "TRTA",
    labelVars = labelVars
  )
}
