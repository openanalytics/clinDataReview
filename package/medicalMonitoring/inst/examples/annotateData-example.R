library(glpgUtilityFct)

data(SDTMDataPelican)

dataLB <- SDTMDataPelican$LB
dataDM <- SDTMDataPelican$DM
dataEX <- SDTMDataPelican$EX
dataAE <- SDTMDataPelican$AE

data(labelVarsSDTMPelican)
labelVars <- labelVarsSDTMPelican

# standard annotations:
# path to dataset should be specified via: 'pathData'
\dontrun{
annotateData(dataLB, annotations = "demographics", pathData = ...)
}

# add all variables in annotation data (if not already available)
head(annotateData(dataLB, annotations = list(data = dataDM)), 1)

# only variables of interest
head(annotateData(dataLB, annotations = list(data = dataDM, vars = c("ARMCD", "COUNTRY"))), 1)

# filter annotation dataset
dataAnnotated <- annotateData(dataLB, 
	annotations = list(
		data = dataDM, 
		vars = c("ARMCD", "COUNTRY", "SEX"), 
		filters = list(var = "SEX", value = "F")
	)
)
head(subset(dataAnnotated, SEX == "F"), 1)
head(subset(dataAnnotated, is.na(SEX)), 1)

# worst-case scenario: add a new variable based on filtering condition
dataAE$AESEV <- factor(dataAE$AESEV, levels = c('MILD', "MODERATE"))
dataAEWC <- annotateData(
	data = dataAE,
	annotations = list(
		vars = "WORSTINT", 
		# create new variable: 'WORSTINT' 
		# with TRUE if maximum toxicity grade per subject/test 
		# (if multiple, they are all retained)
		filters = list(
			var = "AESEV", 
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
	annotations = list(vars = "ULN_ratio", varFct = "LBSTRESN / LBSTNRHI")
)

# add a new variable based on extraction of a existing variable
# Note: slash should be doubled when the function is specified as text
dataLB <- annotateData(dataLB, 
	annotations = list(vars = "PERIOD", varFct = "sub('Day .* - (.+)', '\\\\1', VISIT)")
)
print(unique(dataLB[, c("VISIT", "PERIOD")]))

# multiple annotations:
dataAnnotated <- annotateData(dataLB, 
	annotations = list(
		list(data = dataDM, vars = c("ARMCD", "COUNTRY", "SEX")),
		list(data = dataEX)
	)
)
head(dataAnnotated, 1)
head(subset(dataAnnotated, !is.na(EXSEQ)), 1)
