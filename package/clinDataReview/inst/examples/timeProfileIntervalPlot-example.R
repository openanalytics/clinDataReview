library(clinUtils)

data(dataADaMCDISCP01)
labelVars <- attr(dataADaMCDISCP01, "labelVars")

dataAE <- dataADaMCDISCP01$ADAE

# basic plot
timeProfileIntervalPlot(
	data = dataAE,
	paramVar = "USUBJID",
	# time-variables
	timeStartVar = "ASTDY",
	timeEndVar = "ASTDY",
	# colored by severity
	colorVar = "AESEV",
	labelVars = labelVars
)

# add caption & subtitle
timeProfileIntervalPlot(
	data = dataAE,
	paramVar = "USUBJID",
	timeStartVar = "ASTDY",
	timeEndVar = "ASTDY",
	colorVar = "AESEV",
	labelVars = labelVars,
	title = "Adverse events",
	subtitle = "Time intervals",
	caption = "Day is relative to the study baseline"
)