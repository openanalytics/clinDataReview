context("data annotation")

library(glpgUtilityFct)

data(SDTMDataPelican)

dataLB <- SDTMDataPelican$LB
dataDM <- SDTMDataPelican$DM
dataEX <- SDTMDataPelican$EX

test_that("Correct extraction of custom annotation", {
			
	dataAnnot <- annotateData(dataLB, annotations = list(data = dataDM))
	
	newCols <- setdiff(colnames(dataAnnot), colnames(dataLB))
	expect_equivalent(
		object = dataDM[match(dataLB$USUBJID, dataDM$USUBJID), newCols],
		expected = dataAnnot[, newCols]
	)	
			
})

test_that("Correct computation of new variable based on combination of multiple variables", {

	dataLB <- annotateData(dataLB, annotations = list(vars = "ULN_ratio", varFct = "LBSTRESN / LBSTNRHI"))
	expect_equivalent(
		object = dataLB$`ULN_ratio`,
		expected = with(dataLB, LBSTRESN / LBSTNRHI)
	)
		
})

test_that("Annotation based on text extraction with 'sub'", {
			
	varFct <- "sub('Day .* - (.+)', '\\\\1', VISIT)"
	dataLB <- annotateData(dataLB, 
		annotations = list(
			vars = "PERIOD", 
			varFct = varFct
		)
	)

	expect_equivalent(
		object = dataLB$PERIOD,
		expected = eval(expr = parse(text = varFct), envir = dataLB)
	)
	
})