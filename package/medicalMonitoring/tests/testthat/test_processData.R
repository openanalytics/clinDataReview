context("data annotation")

library(glpgUtilityFct)

data(SDTMDataPelican)

dataLB <- SDTMDataPelican$LB
dataDM <- SDTMDataPelican$DM
dataEX <- SDTMDataPelican$EX
labelVars <- attr(SDTMDataPelican, "labelVars")

test_that("Process data identical to calling pre-processing function separately", {
		
	# each pre-processing function separately
	dataLBAnnot <- annotateData(
		data = dataLB, 
		annotations = list(data = dataDM, vars = c("SEX", "RACE", "COUNTRY", "ACTARM")), 
		labelVars = labelVars
	)
	labelVarsAnnot <- attr(dataLBAnnot, "labelVars")
	dataLBAnnotTreatment <- filterData(
		data = dataLBAnnot, 
		filters = list(var = "ACTARM", value = "Screen Failure", rev = TRUE), 
		labelVars = labelVarsAnnot
	)
		
	# all at once
	dataLBAnnotTreatment2 <- processData(
		data = dataLB,
		processing = list(
			list(annotate = list(data = dataDM, vars = c("SEX", "RACE", "COUNTRY", "ACTARM"))),
			list(filter = list(var = "ACTARM", value = "Screen Failure", rev = TRUE))
		),
		labelVars = labelVars
	)
			
	expect_identical(dataLBAnnotTreatment, dataLBAnnotTreatment2)
			
})

test_that("Process data fails is input is not a list", {
	
	# in case user forgets the '-' in the config file
	expect_error(
		res <- processData(
			data = dataLB,
			processing = list(
				annotate = list(data = dataDM, vars = c("SEX", "RACE", "COUNTRY", "ACTARM")),
				filter = list(var = "ACTARM", value = "Screen Failure", rev = TRUE)
			),
			labelVars = labelVars
		)
	)
			
})
