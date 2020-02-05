library(glpgUtilityFct)

data(SDTMDataPelican)

dataLB <- SDTMDataPelican$LB
dataDM <- SDTMDataPelican$DM
dataEX <- SDTMDataPelican$EX

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
		filters = list(var = "SEX", value = "F"))
	)
head(subset(dataAnnotated, SEX == "M"), 1)
head(subset(dataAnnotated, is.na(SEX)), 1)

# multiple annotations:
dataAnnotated <- annotateData(dataLB, 
	annotations = list(
		list(data = dataDM, vars = c("ARMCD", "COUNTRY", "SEX")),
		list(data = dataEX)
	)
)
head(dataAnnotated, 1)
head(subset(dataAnnotated, !is.na(EXSEQ)), 1)