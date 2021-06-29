context("Treemap for clinical data")

# load example data
library(clinUtils)
library(inTextSummaryTable)
library(plyr)
library(plotly)

data(dataADaMCDISCP01)
labelVars <- attr(dataADaMCDISCP01, "labelVars")

dataAE <- dataADaMCDISCP01$ADAE
dataDM <- dataADaMCDISCP01$ADSL

# sunburst takes as input table with counts
# total counts: Safety Analysis Set (patients with start date for the first treatment)
dataTotal <- subset(dataDM, RFSTDTC != "")

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
	outputType = "data.frame-base",
	# statistics of interest
	# for DT output, include columns with patients
	stats = statsPP, 
	# add extra 'statistic': concatenate subject IDs
	statsExtra = statsExtraPP

)

dataPlot <- subset(tableAE, !isTotal)

dataPlot$n <- as.numeric(dataPlot$n)

test_that("plotting function runs properly", {
			
	pl <- treemapClinData(
		data = dataPlot,
		vars = c("AESOC", "AEDECOD"),
		valueVar = "n", valueLab = "Number of patients with adverse events"
	)
    expect_is(pl, "plotly")
    
})

test_that("interactive table is created", {
			
	res <- sunburstClinData(
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
		rowVar = c("ARM", "SITEID"),
		labelVars = labelVars,
		# plotly treemap requires records (rows) for each group
		rowVarTotalInclude = "SITEID",
		rowTotalInclude = TRUE,
		outputType = "data.frame-base"
	)
	tableDM$statN <- as.numeric(tableDM$statN)
	tableDM <- subset(tableDM, !isTotal)
	expect_silent({
		treemapClinData(
			data = tableDM,
			vars = c("ARM", "SITEID"), 
			valueVar = "statN"
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
		pl <- treemapClinData(
			data = dataPlot,
			vars = c("parent", "child"), valueVar = "count"
		)
	)
	# Note: Plotly failed in Js: 'Failed to build treemap hierarchy. Error: ambiguous'
#	htmlwidgets::saveWidget(pl, "test.html")
#	plotly::orca(pl, "test.png")
			
})

test_that("treemap with color variable", {
			
	# extract worst-case scenario
	dataAE$AESEVN <- as.numeric(factor(dataAE$AESEV, levels = c("MILD", "MODERATE", "SEVERE")))
	if(any(is.na(dataAE$AESEVN)))
		stop("Severity should be filled for all subjects.")
	
	dataAEWC <- ddply(dataAE, c("AESOC", "AEDECOD", "USUBJID"), function(x){
		x[which.max(x$AESEVN), ]
	})
	dataTotalRow <- list(AEDECOD = 
		ddply(dataAEWC, c("AESOC", "USUBJID"), function(x){
			x[which.max(x$AESEVN), ]
		})
	)
				
	# compute adverse event table
	tableAE <- getSummaryStatisticsTable(
					
		data = dataAEWC,
		rowVar = c("AESOC", "AEDECOD"),
		var = "AESEVN",
		dataTotal = dataTotal,
		rowOrder = "total",
		labelVars = labelVars,
		
		# plotly treemap requires records (rows) for each group
		rowVarTotalInclude = "AEDECOD",
		dataTotalRow = dataTotalRow,
		outputType = "data.frame-base"
	
	)
	
	dataPlot <- tableAE
	
	dataPlot$statN <- as.numeric(dataPlot$statN)
	dataPlot$statMean <- as.numeric(dataPlot$statMean)
			
	# create plot
	pl <- treemapClinData(
		data = dataPlot,
		vars = c("AESOC", "AEDECOD"),
		valueVar = "statN", valueLab = "Number of patients with adverse events",
		colorVar = "statMean", colorLab = "Mean severity"
	)
	dataPlot$ids <- with(dataPlot, paste(AESOC, AEDECOD, sep = "-"))
	dataPlot$ids <- sub("-Total", "", dataPlot$ids)
	
	plData <- plotly_build(pl)$x$data[[1]]
	colors <- plData$marker$colors
	ids <- plData$ids
	
	colorsData <- dataPlot[match(ids, dataPlot$ids), "statMean"]
	statPerColor <- tapply(colorsData, colors, function(x) range(x))
	statPerColorRank <- statPerColor[order(sapply(statPerColor, min), decreasing = FALSE)]
	isColorGroupOfStat <- all(diff(unlist(statPerColorRank)) >= 0)
	expect_true(
		object = isColorGroupOfStat, 
		label = "color var doesn't represent groups of specified summary statistic"
	)
				
})
			
