library(clinUtils)

data(dataADaMCDISCP01)
labelVars <- attr(dataADaMCDISCP01, "labelVars")

## example of basic barplot:

data <- subset(dataADaMCDISCP01$ADVS, 
	PARAMCD == "DIABP" & ANL01FL == "Y" &
	AVISIT %in% c("Baseline", "Week 2", "Week 4", "Week 6", "Week 8")
)

## example of basic boxplot:

# With color var and facet:
boxplotClinData(
	data = data,
	xVar = "AVISIT", 
	yVar = "AVAL",
	colorVar = "TRTA",
	facetVar = "ATPT",
	title = "Diastolic Blood Pressure distribution by actual visit and analysis timepoint",
	yLab = "Actual value of the Diastolic Blood Pressure parameter (mmHg)",
	labelVars = labelVars
)

# Control number of facet columns:
boxplotClinData(
	data = data,
	xVar = "AVISIT", 
	yVar = "AVAL",
	colorVar = "TRTA",
	facetVar = "ATPT",
	ncol = 2,
	title = "Diastolic Blood Pressure distribution by actual visit and analysis timepoint",
	yLab = "Actual value of the Diastolic Blood Pressure parameter (mmHg)",
	labelVars = labelVars
)

# Facet or color is optional:
boxplotClinData(
	data = data,
	xVar = "AVISIT", 
	yVar = "AVAL",
	colorVar = "TRTA",
	title = "Diastolic Blood Pressure distribution by actual visit and analysis timepoint",
	yLab = "Actual value of the Diastolic Blood Pressure parameter (mmHg)",
	labelVars = labelVars
)

boxplotClinData(
	data = data,
	xVar = "AVISIT", 
	yVar = "AVAL",
	facetVar = "ATPT",
	ncol = 2,
	title = "Diastolic Blood Pressure distribution by actual visit and analysis timepoint",
	yLab = "Actual value of the Diastolic Blood Pressure parameter (mmHg)",
	labelVars = labelVars
)