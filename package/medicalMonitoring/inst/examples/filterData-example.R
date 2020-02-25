library(glpgUtilityFct)

data(SDTMDataPelican)

dataDM <- SDTMDataPelican$DM

## single filter

# filter with inclusion criteria:
filterData(dataDM, filters = list(var = "SEX", value = "M"), verbose = TRUE)

# filter with non-inclusion criteria
filterData(dataDM, filters = list(var = "SEX", value = "M", rev = TRUE), verbose = TRUE)

# filter based on inequality operator
filterData(dataDM, filters = list(var = "AGE", value = 20, op = "<="), verbose = TRUE)

# missing values are retained by default!
dataDMNA <- dataDM
dataDMNA[1:2, "AGE"] <- NA
filterData(dataDMNA, filters = list(var = "AGE", value = 20, op = "<="), verbose = TRUE)

# filter missing values on variable
filterData(dataDMNA, filters = list(var = "AGE", value = 20, op = "<=", keepNA = FALSE), verbose = TRUE)

# retain only missing values
filterData(dataDMNA, filters = list(var = "AGE", value = NA, keepNA = FALSE), verbose = TRUE)

## multiple filters

# by default the records fulfilling all conditions are retained ('AND')
filterData(
	data = dataDM, 
	filters = list(
		list(var = "AGE", value = 20, op = "<="),
		list(var = "SEX", value = "M")
	), 
	verbose = TRUE
)

# custom operator:
filterData(
	data = dataDM, 
	filters = list(
		list(var = "AGE", value = 20, op = "<="),
		"|",
		list(var = "SEX", value = "M")
	), 
	verbose = TRUE
)
