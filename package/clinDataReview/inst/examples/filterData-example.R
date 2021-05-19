library(clinUtils)

data(dataADaMCDISCP01)
labelVars <- attr(dataADaMCDISCP01, "labelVars")

dataDM <- dataADaMCDISCP01$ADSL

## single filter

# filter with inclusion criteria:
filterData(
	data = dataDM, 
	filters = list(var = "SEX", value = "M"), 
	verbose = TRUE
)

# filter with non-inclusion criteria
filterData(
	data = dataDM, 
	filters = list(var = "SEX", value = "M", rev = TRUE), 
	verbose = TRUE
)

# filter based on inequality operator
filterData(
	data = dataDM, 
	filters = list(var = "AGE", value = 75, op = "<="), 
	verbose = TRUE
)

# missing values are retained by default!
dataDMNA <- dataDM
dataDMNA[1 : 2, "AGE"] <- NA
filterData(
	data = dataDMNA, 
	filters = list(var = "AGE", value = 75, op = "<="), 
	verbose = TRUE
)

# filter missing values on variable
filterData(
	data = dataDMNA, 
	filters = list(var = "AGE", value = 75, op = "<=", keepNA = FALSE), 
	verbose = TRUE
)

# retain only missing values
filterData(
	data = dataDMNA, 
	filters = list(var = "AGE", value = NA, keepNA = TRUE), 
	verbose = TRUE
)

# filter missing values
filterData(
	data = dataDMNA, 
	filters = list(var = "AGE", keepNA = FALSE), 
	verbose = TRUE
)


## multiple filters

# by default the records fulfilling all conditions are retained ('AND')
filterData(
	data = dataDM, 
	filters = list(
		list(var = "AGE", value = 75, op = "<="),
		list(var = "SEX", value = "M")
	), 
	verbose = TRUE
)

# custom operator:
filterData(
	data = dataDM, 
	filters = list(
		list(var = "AGE", value = 75, op = "<="),
		"|",
		list(var = "SEX", value = "M")
	), 
	verbose = TRUE
)
