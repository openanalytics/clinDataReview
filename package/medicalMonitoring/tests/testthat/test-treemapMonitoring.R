context("Sunburst monitoring")

# load example data
library(glpgUtilityFct)
data(SDTMDataPelican)
data(labelVarsSDTMPelican)
dataDM <- SDTMDataPelican$DM
dataAE <- SDTMDataPelican$AE
labelVars <- labelVarsSDTMPelican

# sunburst takes as input table with counts
library(inTextSummaryTable)

# total counts: Safety Analysis Set (patients with start date for the first treatment)
dataTotal <- subset(dataDM, RFXSTDTC != "")

## patient profiles report

# add path in data
dataAE$patientProfilePath <- paste0(
	"patientProfiles/subjectProfile-", 
	sub("/", "-", dataAE$USUBJID), ".pdf"
)
# add link in data (for attached table)
dataAE$patientProfileLink <- with(dataAE,
	paste0(
		'<a href="', patientProfilePath, 
		'" target="_blank">', USUBJID, '</a>'
	)
)

# combine all paths across patients
statsExtraPP <- list(
		statPatientProfilePath = function(data)	
			toString(sort(unique(data$patientProfilePath))),
		statPatientProfileLink = function(data)
			toString(sort(unique(data$patientProfileLink)))
)

# get default counts + stats with subjects profiles path
statsPP <- c(
	getStats(type = "count-default"),
	list(
			patientProfilePath = quote(statPatientProfilePath),
			patientProfileLink = quote(statPatientProfileLink)
	))

# compute adverse event table
tableAE <- getSummaryStatisticsTable(
		
	data = dataAE,
	rowVar = c("AESOC", "AEDECOD"),
	dataTotal = dataTotal,
	rowOrder = "total",
	labelVars = labelVars,
	
	# plotly treemap requires records (rows) for each group
	rowVarTotalInclude = "AEDECOD",
	
	## DT-output specific:
	outputType = "data.frame",
	# statistics of interest
	# for DT output, include columns with patients
	stats = statsPP, 
	# add extra 'statistic': concatenate subject IDs
	statsExtra = statsExtraPP

)

dataPlot <- tableAE

dataPlot$n <- as.numeric(dataPlot$n)

test_that("plotting function runs properly", {
			
	pl <- treemapMonitoring(
		data = dataPlot,
		vars = c("AESOC", "AEDECOD"),
		valueVar = "n", valueLab = "Number of patients with adverse events"
	)
	
	## check if created plot == reference
	expect_doppelganger(title = "basic", fig = pl, writer = write_svg_plotly)

})

test_that("interactive table is created", {
			
	res <- sunburstMonitoring(
		data = dataPlot,
		vars = c("AESOC", "AEDECOD"),
		valueVar = "n", valueLab = "Number of patients with adverse events",
		table = TRUE
	)
	
	expect_is(res$table, "datatables")
	
})

test_that("plotting function", {
	
	# successful example of the 'total'
	tableDM <- getSummaryStatisticsTable(
		data = dataDM,
		rowVar = c("COUNTRY", "SITEID"),
		labelVars = labelVars,
		# plotly treemap requires records (rows) for each group
		rowVarTotalInclude = "SITEID",
		rowTotalInclude = TRUE,
		outputType = "data.frame"
	)
	tableDM$statN <- as.numeric(tableDM$statN)
	expect_silent({
		treemapMonitoring(
			data = tableDM,
			vars = c("COUNTRY", "SITEID"), valueVar = "statN"
		)
	})
	
})

test_that("repeated labels in child and parent variables", {
			
	set.seed(123)
	# counts for child
	parents <- sample(LETTERS[1:4], length(letters), replace = TRUE)
	childs <- letters
	counts <- sample(10, length(parents), replace = TRUE)
	dataChild <- data.frame(parent = parents, child = childs, count = counts, stringsAsFactors = FALSE)
	idxDuplLabels <- sample(which(with(dataChild, parent != child)), 3)
	dataChild[1, "child"] <- "B"
	# counts for parent
	dataParent <- data.frame(parent = unique(parents), child = "Total", count = 100)
	dataPlot <- rbind(dataChild, dataParent)
	expect_silent(
		pl <- treemapMonitoring(
			data = dataPlot,
			vars = c("parent", "child"), valueVar = "count"
		)
	)
	# Note: Plotly failed in Js: 'Failed to build treemap hierarchy. Error: ambiguous'
#	htmlwidgets::saveWidget(pl, "test.html")
#	plotly::orca(pl, "test.png")
			
})
			
