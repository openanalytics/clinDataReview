params <-
list(createPatientProfiles = TRUE)

## ----options, echo = FALSE, message = FALSE-----------------------------------------------------------------------------------------------------------------------------
	
	library(knitr)
	library(plotly)
	opts_chunk$set(
		echo = TRUE, 
#		results = 'asis', 
		warning = FALSE, 
		error = FALSE, message = FALSE, cache = FALSE,
		fig.width = 8, fig.height = 7,
		fig.path = "./figures_vignette/",
		#out.width = '0.7\\textwidth', 
		fig.align = 'center')#, out.height = 0.5\\textwidth', fig.path = 'graphs/') 
	options(width = 170)#, stringsAsFactors = FALSE
	options(warn = 1)#instead of warn = 0 by default -> to have place where warnings occur in the call to Sweave function
	
	heightLineIn  <- 0.2
	

## ----loadPackages, message = FALSE--------------------------------------------------------------------------------------------------------------------------------------

	library(medicalMonitoring)
	library(pander)


## ----loadData-----------------------------------------------------------------------------------------------------------------------------------------------------------
	
	library(glpgUtilityFct)
	
	data(SDTMDataPelican)
	data(labelVarsSDTMPelican)
	
	dataLB <- SDTMDataPelican$LB
	dataDM <- SDTMDataPelican$DM
	dataAE <- SDTMDataPelican$AE
	labelVars <- labelVarsSDTMPelican


## ----annotateData, message = TRUE---------------------------------------------------------------------------------------------------------------------------------------
dataLB <- annotateData(dataLB, annotations = list(data = dataDM), verbose = TRUE)
pander(
	head(dataLB), 
	caption = paste("Laboratory parameters annotated with",
		"demographics information with the `annotatedData` function"
	)
)

## ----transformData, message = TRUE--------------------------------------------------------------------------------------------------------------------------------------

	eDishData <- transformData(
		data = subset(dataLB, LBTESTCD %in% c("ALT", "BILI")),
		transformations = list(
			type = "pivot_wider",
			varsID = c("USUBJID", "VISIT"), 
			varsValue = c("LBSTRESN", "LBNRIND"),
			varPivot = "LBTESTCD"
		),
		verbose = TRUE,
		labelVars = labelVars
	)
	pander(head(eDishData))


## ----createPatientProfilesReport, eval = params$createPatientProfiles, results = "hide"---------------------------------------------------------------------------------------------------------------

	library(patientProfilesVis)
	
	# export example data
	dataPath <- "data";dir.create(dataPath)
	library(haven)
	dataPathFiles <- sapply(names(SDTMDataPelican), function(dm){
		# issue for empty col
		idxCol <- !sapply(SDTMDataPelican[[dm]], function(x) all(x == ""))
		dataPathDM <- file.path(dataPath, paste0(dm, ".sas7bdat"))
		write_sas(
			data = SDTMDataPelican[[dm]][, idxCol], 
			path = dataPathDM
		)
		dataPathDM
	})
#	sapply(dataPathFiles, read_sas)

	# create patient profile report
	runPatientProfileTemplateReport(
		dataPath = dataPath, 
		outputFile = "patientProfiles/subjectProfile.pdf",
		study = "Pelican",
		batch = "20180606",
		author = "Laure Cougnaud",
		overwrite = TRUE
	)


## ----timeProfiles-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

labParam <- "ALT"
dataPlot <- subset(dataLB, LBTESTCD == labParam)
visitLab <- with(dataPlot, tapply(LBDY, VISIT, median))
names(visitLab) <- sub("-", "\n", names(visitLab))

# link to patient profiles
dataPlot$patientProfilePath <- paste0(
	"patientProfiles/subjectProfile-", 
	sub("/", "-", dataPlot$USUBJID), ".pdf"
)

scatterplotMonitoring(
	data = dataPlot, 
	xVar = "LBDY", yVar = "LBSTRESN",
	aesPointVar = list(color = "ACTARM"),
	aesLineVar = list(group = "USUBJID", color = "ACTARM"),
	hoverVars = c("USUBJID", "VISIT", "LBDY", "LBSTRESN", "COUNTRY", "ACTARM"),
	labelVars = labelVars,
	xPars = list(breaks = visitLab, labels = names(visitLab)),
#	themePars = list(legend.position = "none"),
	title = paste("Actual value of", 
		getLabelParamcd(paramcd = labParam, data = dataLB, paramcdVar = "LBTESTCD", paramVar = "LBTEST")
	),
	# include link to patient profiles:
	pathVar = "patientProfilePath",
	table = TRUE, id = paste("subjectProfile", labParam, sep = "-"),
	verbose = TRUE
)


## ----scatterplot--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# format data long -> wide format (one column per lab param)
dataPlot <- subset(dataLB, LBTESTCD %in% c("ALT", "ALB"))
library(reshape2)
dataPlotWide <- dcast(
	data = dataPlot,
	formula = USUBJID + VISIT + VISITNUM ~ LBTESTCD, 
	value.var = "LBSTRESN",
	fun.aggregate = mean
)

# link to patient profiles
dataPlotWide$patientProfilePath <- paste0(
	"patientProfiles/subjectProfile-", 
	sub("/", "-", dataPlotWide$USUBJID), ".pdf"
)

# scatterplot per visit
scatterplotMonitoring(
	data = dataPlotWide, 
	xVar = "ALT", yVar = "ALB",
	xLab = getLabelParamcd(paramcd = "ALT", data = dataLB, paramcdVar = "LBTESTCD", paramVar = "LBTEST"),
	yLab = getLabelParamcd(paramcd = "ALB", data = dataLB, paramcdVar = "LBTESTCD", paramVar = "LBTEST"),
	aesPointVar = list(color = "USUBJID"),
#	themePars = list(legend.position = "none"),
	facetPars = list(facets = ~ VISIT),
	labelVars = labelVars,
	pathVar = "patientProfilePath",
	table = TRUE
)



## ----eDishPlot----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

	dataALT <- subset(dataLB, LBTESTCD == "ALT")
	dataBILI <- subset(dataLB, LBTESTCD == "BILI")
	byVar <- c("USUBJID", "VISIT")
	dataPlot <- merge(
		x = dataALT, y = dataBILI[, c(byVar, "LBSTRESN")], 
		by = c("USUBJID", "VISIT"), 
		suffixes = c(".ALT", ".BILI"),
		all = TRUE
	)
	labelVars[paste0("LBSTRESN.", c("ALT", "BILI"))] <-
		paste(
			"Actual value of", 
			getLabelParamcd(paramcd = c("ALT", "BILI"), data = dataLB, paramcdVar = "LBTESTCD", paramVar = "LBTEST")
		)
	
	# link to patient profiles
	dataPlot$patientProfilePath <- paste0(
		"patientProfiles/subjectProfile-", 
		sub("/", "-", dataPlot$USUBJID), ".pdf"
	)

	# scatterplot per visit
	scatterplotMonitoring(
		data = dataPlot, 
		xVar = "LBSTRESN.ALT", yVar = "LBSTRESN.BILI",
		xLab = getLabelParamcd(paramcd = "ALT", data = dataLB, paramcdVar = "LBTESTCD", paramVar = "LBTEST"),
		yLab = getLabelParamcd(paramcd = "BILI", data = dataLB, paramcdVar = "LBTESTCD", paramVar = "LBTEST"),
		aesPointVar = list(color = "VISIT", shape = "ACTARM"),
		xTrans = "log10", yTrans = "log10",
		hoverVars = c("USUBJID"),
		themePars = list(legend.position = "bottom"),
		labelVars = labelVars,
		table = TRUE, id = "eDish",
		pathVar = "patientProfilePath"
	)


## ----createCountTableAE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

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
	# the paths should be collapsed with: ', '
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
		)
	)
	
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
	pander(head(tableAE),
		caption = paste("Extract of the Adverse Event summary table",
			"used for the sunburst and barplot visualization"
		)
	)


## ----sunburst-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

	dataSunburst <- tableAE
	
	dataSunburst$n <- as.numeric(dataSunburst$n)

	sunburstMonitoring(
		data = dataSunburst,
		vars = c("AESOC", "AEDECOD"),
		valueVar = "n", valueLab = "Number of patients with adverse events",
		pathVar = "patientProfileLink", pathLab = getLabelVar(var = "USUBJID", labelVars = labelVars),
		table = TRUE,
		verbose = TRUE
	)
	

## ----treemap------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

	dataTreemap <- tableAE
	
	dataTreemap$n <- as.numeric(dataTreemap$n)
	
	treemapMonitoring(
		data = dataTreemap,
		vars = c("AESOC", "AEDECOD"),
		valueVar = "n", valueLab = "Number of patients with adverse events",
		pathVar = "patientProfileLink", pathLab = getLabelVar(var = "USUBJID", labelVars = labelVars),
		table = TRUE,
		verbose = TRUE
	)
	

## ----barplot------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

	dataPlot <- subset(tableAE, AEDECOD != "Total")
	
	dataPlot$n <- as.numeric(dataPlot$n)
	
	# create plot
	barplotMonitoring(
		data = dataPlot,
		xVar = "AEDECOD", colorVar = "AESOC",
		yVar = "n", yLab = "Number of patients with adverse events",
		labelVars = labelVars,
		pathVar = "patientProfileLink", pathLab = getLabelVar(var = "USUBJID", labelVars = labelVars),
		table = TRUE,
		verbose = TRUE
	)
	

## ----'lab-profile-loop1', results = 'asis', echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------
cat("\n", paste(rep("#", titleLevel), collapse = ""), " Sweat Volume, Left\n", sep = "")
xList[[1]]

## ----'lab-profile-loop2', results = 'asis', echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------
cat("\n", paste(rep("#", titleLevel), collapse = ""), " Sweat Volume, Right\n", sep = "")
xList[[2]]

## ----'lab-profile-loop3', results = 'asis', echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------
cat("\n", paste(rep("#", titleLevel), collapse = ""), " Sweat Chloride, Left\n", sep = "")
xList[[3]]

## ----'lab-profile-loop4', results = 'asis', echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------
cat("\n", paste(rep("#", titleLevel), collapse = ""), " Sweat Chloride, Right\n", sep = "")
xList[[4]]

## ----lab-profile-loop, results = "asis"---------------------------------------------------------------------------------------------------------------------------------------------------------------

	# consider only restricted set of lab parameters
	dataPlot <- subset(dataLB, LBCAT == "SPECIAL CHEMISTRY")

	# link to patient profiles
	dataPlot$patientProfilePath <- paste0(
		"patientProfiles/subjectProfile-", 
		sub("/", "-", dataPlot$USUBJID), ".pdf"
	)
	
	# 1) create plot+table for each laboratory parameter:
	library(plyr) # for ddply
	plotsLab <- dlply(dataPlot, "LBTESTCD", function(dataLBParam){
				
		paramcd <- unique(dataLBParam$LBTESTCD)
			
		scatterplotMonitoring(
			data = dataLBParam, 
			xVar = "LBDY", yVar = "LBSTRESN",
			aesPointVar = list(color = "ACTARM"),
			aesLineVar = list(group = "USUBJID", color = "ACTARM"),
			labelVars = labelVars,
			title = paste("Actual value of", 
				getLabelParamcd(paramcd = paramcd, data = dataLBParam, paramcdVar = "LBTESTCD", paramVar = "LBTEST")
			),
			# include link to patient profiles:
			pathVar = "patientProfilePath",
			table = TRUE, 
			# important: each plot should have an unique ID!
			# for unique relationship of interactivity between plot <-> table
			id = paste("labProfileLoop", paramcd, sep = "-"),
			verbose = TRUE
		)
			
	})

	# include this output in the report:
	library(glpgUtilityFct)
	listLabels <- getLabelParamcd(paramcd = names(plotsLab), data = dataLB, paramcdVar = "LBTESTCD", paramVar = "LBTEST")
	knitPrintListObjects(
		xList = plotsLab, 
		generalLabel = "lab-profile-loop",
		titles = listLabels, titleLevel = 4
	)


## ----includeSessionInfo, echo = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------

	library(pander)
	pander(sessionInfo())


