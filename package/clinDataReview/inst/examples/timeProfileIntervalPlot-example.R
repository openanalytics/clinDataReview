library(clinUtils)

data(dataADaMCDISCP01)
labelVars <- attr(dataADaMCDISCP01, "labelVars")

dataAE <- dataADaMCDISCP01$ADAE

timeProfileIntervalPlot(
	data = dataAE,
	paramVar = "USUBJID",
	# time-variables
	timeStartVar = "ASTDY",
	timeEndVar = "ASTDY",
	colorVar = "AESEV",
	labelVars = labelVars
)