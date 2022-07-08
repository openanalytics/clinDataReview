library(clinUtils)

data(dataADaMCDISCP01)
labelVars <- attr(dataADaMCDISCP01, "labelVars")

dataDM <- dataADaMCDISCP01$ADSL

## single filter

# filter with inclusion criteria:
filterData(
	data = dataDM, 
	filters = list(var = "SEX", value = "M"),
	# optional
	labelVars = labelVars, verbose = TRUE
)

# filter with non-inclusion criteria
filterData(
	data = dataDM, 
	filters = list(var = "SEX", value = "M", rev = TRUE), 
	# optional
	labelVars = labelVars, verbose = TRUE
)

# filter based on inequality operator
filterData(
	data = dataDM, 
	filters = list(var = "AGE", value = 75, op = "<="), 
	# optional
	labelVars = labelVars, verbose = TRUE
)

# missing values are retained by default!
dataDMNA <- dataDM
dataDMNA[1 : 2, "AGE"] <- NA
filterData(
	data = dataDMNA, 
	filters = list(var = "AGE", value = 75, op = "<="), 
	# optional
	labelVars = labelVars, verbose = TRUE
)

# filter missing values on variable
filterData(
	data = dataDMNA, 
	filters = list(var = "AGE", value = 75, op = "<=", keepNA = FALSE), 
	# optional
	labelVars = labelVars, verbose = TRUE
)

# retain only missing values
filterData(
	data = dataDMNA, 
	filters = list(var = "AGE", value = NA, keepNA = TRUE), 
	# optional
	labelVars = labelVars, verbose = TRUE
)

# filter missing values
filterData(
	data = dataDMNA, 
	filters = list(var = "AGE", keepNA = FALSE), 
	# optional
	labelVars = labelVars, verbose = TRUE
)


## multiple filters

# by default the records fulfilling all conditions are retained ('AND')
filterData(
	data = dataDM, 
	filters = list(
		list(var = "AGE", value = 75, op = "<="),
		list(var = "SEX", value = "M")
	), 
	# optional
	labelVars = labelVars, verbose = TRUE
)

# custom operator:
filterData(
	data = dataDM, 
	filters = list(
		list(var = "AGE", value = 75, op = "<="),
		"|",
		list(var = "SEX", value = "M")
	), 
	# optional
	labelVars = labelVars, verbose = TRUE
)

# filter by group

# only retain adverse event records with worst-case severity
dataAE <- dataADaMCDISCP01$ADAE
dataAE$AESEV <- factor(dataAE$AESEV, levels = c("MILD", "MODERATE", "SEVERE"))
dataAE$AESEVN <- as.numeric(dataAE$AESEV)
nrow(dataAE)
dataAEWorst <- filterData(
	data = dataAE,
	filters = list(
		var = "AESEVN",		
		valueFct = max,
		varsBy = c("USUBJID", "AEDECOD"),
		keepNA = FALSE
	),
	# optional
	labelVars = labelVars, verbose = TRUE
)
nrow(dataAEWorst)

# post-processing function
# keep subjects with at least one severe AE:
dataSubjectWithSevereAE <- filterData(
  data = dataAE,
  filters = list(
    var = "AESEV",		
    value = "SEVERE",
    varsBy = "USUBJID",
    postFct = any
  ),
  # optional
  labelVars = labelVars, verbose = TRUE
)

# for each laboratory parameter: keep only subjects which have at least one
# measurement classified as low or high
dataLB <- subset(dataADaMCDISCP01$ADLBC, !grepl("change", PARAM))
dataLBFiltered <- filterData(
  data = dataLB,
  filters = list(
    var = "LBNRIND",		
    value = c("LOW", "HIGH"),
    varsBy = c("PARAMCD", "USUBJID"),
    postFct = any
  ),
  # optional
  labelVars = labelVars, verbose = TRUE
)
