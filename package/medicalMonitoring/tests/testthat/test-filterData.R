context("Filter data")

# load example data
library(glpgUtilityFct)
data(SDTMDataPelican)
dataDM <- SDTMDataPelican$DM

test_that("simple condition with inclusion criteria", {

	dataFilt <- filterData(dataDM, filters = list(var = "SEX", value = "M"), verbose = TRUE)
	expect_equal(structure(dataFilt, msg = NULL), subset(dataDM, SEX == "M"))
	
})

test_that("simple condition with non inclusion criteria", {
			
	dataFilt <- filterData(dataDM, filters = list(var = "SEX", value = "M", rev = TRUE), verbose = TRUE)
	expect_equal(structure(dataFilt, msg = NULL), subset(dataDM, SEX != "M"))
			
})

test_that("simple condition with specified operator", {
			
	dataFilt <- filterData(dataDM,  list(var = "AGE", value = 20, op = "<="), verbose = TRUE)
	expect_equal(structure(dataFilt, msg = NULL), subset(dataDM, AGE <= 20))
			
})

test_that("missing values are included by default", {
			
	dataDMNA <- dataDM
	dataDMNA[1:2, "AGE"] <- NA
	dataFilt <- filterData(dataDMNA, list(var = "AGE", value = 20, op = "<="), verbose = TRUE)
	expect_equal(structure(dataFilt, msg = NULL), subset(dataDMNA, AGE <= 20 | is.na(AGE)))
			
})

test_that("missing values are retained (keepNA)", {
			
	dataDMNA <- dataDM
	dataDMNA[1:2, "AGE"] <- NA
	dataFilt <- filterData(dataDMNA, list(var = "AGE", value = 20, op = "<=", keepNA = FALSE), verbose = TRUE)
	expect_equal(structure(dataFilt, msg = NULL), subset(dataDMNA, AGE <= 20))
	
})

test_that("multiple condition with specification operator and without", {
			
	filters <- list(
		list(var = "AGE", value = 20, op = "<="),
		"|",
		list(var = "SEX", value = "M"),
		list(var = "ARMCD", value = "SCRNFAIL")
	)
	dataFilt <- filterData(data = dataDM, filters = filters, verbose = TRUE)
	
	expect_equal(structure(dataFilt, msg = NULL), subset(dataDM, (AGE <= 20 | SEX == "M") & ARMCD == "SCRNFAIL"))

})
