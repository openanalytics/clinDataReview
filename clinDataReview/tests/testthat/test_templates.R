context("Create report based on a template")

library(yaml)
library(rmarkdown)
library(haven)

# fix for pandoc warning: 'This document format requires a nonempty <title> element.'
outputOpts <- list(pandoc_args = 
	rmarkdown::pandoc_metadata_arg(name = "pagetitle", value = "test report")
)

test_that("The division template is successfully rendered", {
			
	skip_if_not(
		condition = rmarkdown::pandoc_available(), 
		message = "pandoc is not available"
	)
	
	dir <- tempfile("division")
	dir.create(dir)
      
	# get params from division config from available one in tests folder
	testPathFiles <- normalizePath(path = "../files")
	testPathConfig <- file.path(testPathFiles, "config")
	configFile <- list.files(
		path = testPathConfig, 
		pattern = "config-adverseEvents-division.yml",
		full.names = TRUE
	)
	params <- read_yaml(configFile)
     
	# run report
	templateName <- "divisionTemplate.Rmd"
	pathTemplate <- system.file("template", templateName, 
		package = "clinDataReview")
	expect_error(
		outputFile <- rmarkdown::render(
			input = pathTemplate,
			output_dir = dir,
			intermediates_dir = dir,
			quiet = TRUE,
			output_options = outputOpts
		),
		NA
	)
	expect_true(file.exists(outputFile))
	detach(params);rm(params)
      
})

test_that("The listing template is successfully rendered", {
			
	skip_on_cran()
      
	dir <- tempfile("listing")
	dir.create(dir)
	
	templateName <- "listingTemplate.Rmd"
	
	# create example data
	dataEX <- data.frame(
		"USUBJID" = c(1, 1, 2, 3, 4),
		"EXDOSE" = "100",
		stringsAsFactors = FALSE
	)
	write_xpt(dataEX, file.path(dir, "adex.xpt"))
      
	# set parameters
	params <- list(
		pathDataFolder = dir,
		template = templateName,
		templatePackage = "clinDataReview",
		reportTitle = gsub("(.+)Template[.].+", "\\1 template", templateName),
		reportTitleLevel = 2,
		dataFileName = list.files(dir),
		tableParams = list(tableVars = c("USUBJID", "EXDOSE"))
	)
      
	# run report
	pathTemplate <- system.file("template", templateName, 
		package = "clinDataReview")      
	expect_error(
		outputFile <- rmarkdown::render(
			input = pathTemplate,
			output_dir = dir,
			intermediates_dir = dir,
			quiet = TRUE,
			output_options = outputOpts
		),
		NA
  	)
	expect_true(file.exists(outputFile))
	
	detach(params);rm(params)
      
})

test_that("The listing template with multiple input data files is successfully rendered", {
  
  skip_on_cran()
  
  dir <- tempfile("listing")
  dir.create(dir)
  
  templateName <- "listingTemplate.Rmd"
  
  # create example datasets
  
  # vital signs
  dataADVS <- data.frame(
    PARAM = "Heart beat",
    USUBJID = c(1, 2),
    VISIT = c("Baseline", "Week 1"),
    AVAL = c(2, 3),
    stringsAsFactors = FALSE
  )
  write_xpt(dataADVS, file.path(dir, "advs.xpt"))
  
  # lab datasets
  dataADLB <- data.frame(
    PARAM = "GP",
    USUBJID = c(1, 2),
    VISIT = c("Baseline", "Week 1"),
    AVAL = c(3, 6),
    stringsAsFactors = FALSE
  )
  write_xpt(dataADLB, file.path(dir, "adlb.xpt"))
  
  # set parameters
  params <- list(
    pathDataFolder = dir,
    template = templateName,
    templatePackage = "clinDataReview",
    reportTitle = "Listing of the lab and vital signs",
    reportTitleLevel = 2,
    dataFileName = c("advs.xpt", "adlb.xpt"),
    tableParams = list(tableVars = c("PARAM", "USUBJID", "VISIT", "AVAL"))
  )
  
  # run report
  pathTemplate <- system.file("template", templateName, 
    package = "clinDataReview")
  
  expect_error(
    outputFile <- rmarkdown::render(
      input = pathTemplate,
      output_dir = dir,
      intermediates_dir = dir,
      quiet = TRUE,
	  output_options = outputOpts
    ),
    NA
  )
  expect_true(file.exists(outputFile))
  
  detach(params);rm(params)
  
})

test_that("The count visualization template is successfully rendered", {
			
	skip_on_cran()
	
	dir <- tempfile("countsVisualization")
	dir.create(dir)
      
	templateName <- "countsVisualizationTemplate.Rmd"
	
	# create example data
	dataEX <- data.frame(
		"USUBJID" = c(1, 1, 2, 3, 4),
		"EXDOSE" = "100",
		stringsAsFactors = FALSE
	)
	write_xpt(dataEX, file.path(dir, "adex.xpt"))
      
	# set parameters
	params <- list(
		pathDataFolder = dir,
		template = templateName,
		templatePackage = "clinDataReview",
		reportTitle = gsub("(.+)Template[.].+", "\\1 template", templateName),
		dataFileName =  list.files(dir, pattern = "xpt"),
		countVar = "EXDOSE"
	)      
      
	# run report
	pathTemplate <- system.file("template", templateName, 
		package = "clinDataReview")      
	expect_error(
		outputFile <- rmarkdown::render(
			input = pathTemplate,
			output_dir = dir,
			intermediates_dir = dir,
			quiet = TRUE,
			output_options = outputOpts
		),
		NA
	)
	expect_true(file.exists(outputFile))
	
	detach(params);rm(params)
      
})

test_that("The plot template is successfully rendered", {
			
	skip_on_cran()
	
	dir <- tempfile("plot")
	dir.create(dir)
	
	templateName <- "plotTemplate.Rmd"
	
	# create example data
	dataEX <- data.frame(
		"USUBJID" = c(1, 1, 2, 3, 4),
		"EXDOSE" = "100",
		stringsAsFactors = FALSE
	)
	write_xpt(dataEX, file.path(dir, "adex.xpt"))
      
	# set parameters
	params <- list(
		pathDataFolder = dir,
		template = templateName,
		templatePackage = "clinDataReview",
		reportTitle = gsub("(.+)Template[.].+", "\\1 template", templateName),
		dataFileName =  list.files(dir, pattern = "xpt"),
		plotFunction = "scatterplotClinData",
		plotParams = list(xVar = "USUBJID", yVar = "EXDOSE")
	)
     
	# run report
	pathTemplate <- system.file("template", templateName, 
		package = "clinDataReview")      
	expect_error(
		outputFile <- rmarkdown::render(
			input = pathTemplate,
			output_dir = dir,
			intermediates_dir = dir,
			quiet = TRUE,
			output_options = outputOpts
		),
		NA
  	)
	expect_true(file.exists(outputFile))
	
	detach(params);rm(params)
      
})

test_that("The summary plot template is successfully rendered", {
			
	skip_on_cran()
	
	dir <- tempfile("summaryPlot")
	dir.create(dir)
      
	templateName <- "summaryPlotTemplate.Rmd"
	  
	# create example data
	dataEX <- data.frame(
		"USUBJID" = c(1, 1, 2, 3, 4),
		"EXDOSE" = "100",
		stringsAsFactors = FALSE
	)
	write_xpt(dataEX, file.path(dir, "adex.xpt"))
	
	# set parameters
	params <- list(
		pathDataFolder = dir,
		template = templateName,
		templatePackage = "clinDataReview",
		reportTitle = gsub("(.+)Template[.].+", "\\1 template", templateName),
		dataFileName =  list.files(dir, pattern = "xpt"),
		tableParams = list(
  			var = "EXDOSE",
  			stats = "getStats(type = 'n')"
  		),
		plotFunction = "barplotClinData",
		plotParams = list(xVar = "variableGroup", yVar = "statN")
	)
	
	# run report
	pathTemplate <- system.file("template", templateName, 
		package = "clinDataReview")      
	expect_error(
		outputFile <- rmarkdown::render(
			input = pathTemplate,
			output_dir = dir,
			intermediates_dir = dir,
			quiet = TRUE,
			output_options = outputOpts
		),
		NA
	)
	expect_true(file.exists(outputFile))
	
	detach(params);rm(params)
      
})

test_that("A plot parameter is correctly extracted from the data in a summary plot template", {
  
  skip_on_cran()
  
  dir <- tempfile("summaryPlot");dir.create(dir)
  
  templateName <- "summaryPlotTemplate.Rmd"
  
  # create example data
  dataEX <- data.frame(
    "USUBJID" = c(1, 1, 2, 3, 4),
    "EXDOSE" = structure(rep("100", 5), label = "Dose"),
    stringsAsFactors = FALSE
  )
  write_xpt(dataEX, file.path(dir, "adex.xpt"))
  
  # set parameters
  params <- list(
    pathDataFolder = dir,
    template = templateName,
    templatePackage = "clinDataReview",
    reportTitle = gsub("(.+)Template[.].+", "\\1 template", templateName),
    dataFileName =  list.files(dir, pattern = "xpt"),
    tableParams = list(
      rowVar = "EXDOSE",
      stats = "getStats(type = 'n')"
    ),
    plotFunction = "barplotClinData",
    plotParams = list(
      xVar = "EXDOSE", 
      yVar = "statN", 
      title = structure(
        "paste0(labelVars['EXDOSE'], ': ', unique(summaryTableI$EXDOSE))", 
        class = "r-lazy"
      )
    )
  )
  
  # run report
  pathTemplate <- system.file("template", templateName, 
      package = "clinDataReview")      
  expect_error(
    outputFile <- rmarkdown::render(
      input = pathTemplate,
      output_dir = dir,
      intermediates_dir = dir,
      quiet = TRUE,
      output_options = outputOpts
    ),
    NA
  )
  expect_true(file.exists(outputFile))
  
  pl <- listPlots[[1]]$plot
  expect_equal(
    object = plotly::plotly_build(pl)$x$layout$title$text,
    expected = "Dose: 100"
  )
  
  detach(params);rm(params)
  
})

test_that("The summary table template is successfully rendered", {
			
	skip_on_cran()
	
	dir <- tempfile("summaryTable")
	dir.create(dir)
      
	templateName <- "summaryTableTemplate.Rmd"
	  
	# create example data
	dataEX <- data.frame(
		"USUBJID" = c(1, 1, 2, 3, 4),
		"EXDOSE" = "100",
		stringsAsFactors = FALSE
	)
	write_xpt(dataEX, file.path(dir, "adex.xpt"))
      
	# set parameters
	params <- list(
		pathDataFolder = dir,
		template = templateName,
		templatePackage = "clinDataReview",
		reportTitle = gsub("(.+)Template[.].+", "\\1 template", templateName),
		dataFileName =  list.files(dir, pattern = "xpt"),
		tableParams = list(
			var = c("USUBJID", "EXDOSE"),
			stats = "setNames(getStats(type = 'n'), nm = 'n')"
		)
	)
	
	# run report
	pathTemplate <- system.file("template", templateName, 
		package = "clinDataReview")      
	expect_error(
		outputFile <- rmarkdown::render(
			input = pathTemplate,
			output_dir = dir,
			intermediates_dir = dir,
			quiet = TRUE,
			output_options = outputOpts
		),
		NA
	)
	expect_true(file.exists(outputFile))
	
	detach(params);rm(params)
      
})

test_that("The patient profile template is successfully rendered", {
			
	skip_on_cran()
	
	dir <- tempfile("patientProfiles")
	dir.create(dir)
	
	templateName <- "patientProfilesTemplate.Rmd"
	
	# create example data
	dataEX <- data.frame(
		"USUBJID" = c(1, 1, 2, 3, 4),
		"EXDOSE" = "100",
		stringsAsFactors = FALSE
	)
	write_xpt(dataEX, file.path(dir, "adex.xpt"))
	
	# set parameters
	patientProfilePath <- file.path(dir, "patientProfiles")
	params <- list(
		pathDataFolder = dir,
		patientProfilePath = patientProfilePath,
		template = templateName,
		templatePackage = "clinDataReview",
		reportTitle = gsub("(.+)Template[.].+", "\\1 template", templateName),
		patientProfilesParams = list(
			list(
				typePlot = "text",
				dataFileName =  list.files(dir, pattern = "xpt"),
 				plotParams = list(
   					paramValueVar = c("USUBJID", "EXDOSE")
				)
			)
		)
	)
      
	# run report
	pathTemplate <- system.file("template", templateName, 
		package = "clinDataReview")      
	expect_error(
		outputFile <- rmarkdown::render(
			input = pathTemplate,
			output_dir = dir,
			intermediates_dir = dir,
			quiet = TRUE,
			output_options = outputOpts
		),
		NA
	)
	expect_true(file.exists(outputFile))
	expect_true(file.exists(patientProfilePath))
	expect_true(all(grepl("^subjectProfile-", list.files(patientProfilePath))))   
	
	detach(params);rm(params)
      
})