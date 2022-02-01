library(clinUtils)

data(dataADaMCDISCP01)

dataLB <- dataADaMCDISCP01$ADLBC
dataDM <- dataADaMCDISCP01$ADSL
dataAE <- dataADaMCDISCP01$ADAE

labelVars <- attr(dataADaMCDISCP01, "labelVars")

# standard annotations:
# path to dataset should be specified via: 'pathData'
\dontrun{
annotateData(dataLB, annotations = "demographics", pathData = ...)
}

# add all variables in annotation data (if not already available)
head(annotateData(dataLB, annotations = list(data = dataDM)), 1)

# only variables of interest
head(annotateData(dataLB, annotations = list(data = dataDM, vars = c("ARM", "ETHNIC"))), 1)

# filter annotation dataset
dataAnnotated <- annotateData(dataLB, 
	annotations = list(
		data = dataDM, 
		vars = c("ARM", "ETHNIC"), 
		filters = list(var = "ARM", value = "Placebo")
	)
)
head(subset(dataAnnotated, ARM == "Placebo"), 1)
head(subset(dataAnnotated, is.na(ARM)), 1)

# worst-case scenario: add a new variable based on filtering condition
dataAE$AESEV <- factor(dataAE$AESEV, levels = c('MILD', "MODERATE", "SEVERE"))
dataAEWC <- annotateData(
	data = dataAE,
	annotations = list(
		vars = "WORSTINT", 
		# create new variable: 'WORSTINT' 
		# with TRUE if maximum toxicity grade per subject/test 
		# (if multiple, they are all retained)
		filters = list(
			var = "AESEV", 
			# max will take latest level in a factor 
			# (so 'MODERATE' if 'MILD'/'MODERATE' are available)
			valueFct = function(x) x[which.max(as.numeric(x))],
			varsBy = c("USUBJID", "AEDECOD"),
			keepNA = FALSE,
			varNew = "WORSTINT", 
			labelNew = "worst-case"
		)
	),
	labelVars = labelVars,
	verbose = TRUE
)
attr(dataAEWC, "labelVars")["WORSTINT"]

# add a new variable based on a combination of variables:
dataLB <- annotateData(dataLB, 
	annotations = list(vars = "HILORATIO", varFct = "A1HI / A1LO")
)

# add a new variable based on extraction of a existing variable
# Note: slash should be doubled when the function is specified as text
dataLB <- annotateData(dataLB, 
	annotations = list(vars = "PERIOD", varFct = "sub('.* Week (.+)', 'Week \\\\1', AVISIT)")
)

# multiple annotations:
dataAnnotated <- annotateData(dataLB, 
	annotations = list(
		list(data = dataDM, vars = c("ARM", "ETHNIC")),
		list(data = dataAE, vars = c("AESEV"))
	)
)
head(dataAnnotated, 1)
