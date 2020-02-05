library(glpgUtilityFct)

data(SDTMDataPelican)

dataDM <- SDTMDataPelican$DM

# filter with inclusion criteria:
filterData(dataDM, filters = list(var = "SEX", value = "M"), verbose = TRUE)

# filter with non-inclusion criteria
filterData(dataDM, filters = list(var = "SEX", value = "M", rev = TRUE), verbose = TRUE)

# filter based on inequality operator
filterData(dataDM, filters = list(var = "AGE", value = 20, op = "<="), verbose = TRUE)

# multiple filters
filterData(
	data = dataDM, 
	filters = list(
		list(var = "AGE", value = 20, op = "<="),
		list(var = "SEX", value = "M")
	), 
	verbose = TRUE
)
