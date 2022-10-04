context("Render a clinical data review report")

library(yaml)
library(parallel)
library(xml2)

test_that("Report titles are extracted for each config file", {
      
	# get example config files
	testPathConfig <- normalizePath(file.path("../files", "config"))
	configFiles <- list.files(testPathConfig)
	otherConfigs <- configFiles[!grepl("config.yml", configFiles)]
			
	reportTitles <- clinDataReview:::checkReportTitles(configFiles, configDir = testPathConfig)
	expect_type(reportTitles, "character")
	expect_length(reportTitles, length(otherConfigs))
	expect_named(reportTitles, otherConfigs)
      
})

test_that("An error is generated if the multiple reports have the same titles", {
      
	# get example config files
	testPathConfig <- normalizePath(file.path("../files", "config"))
	configFiles <- list.files(testPathConfig)
			
	configFilesError <- c(configFiles[1], configFiles[1])
	expect_error(
	  clinDataReview:::checkReportTitles(configFilesError, configDir = testPathConfig),
		"The title .+ is duplicated."
	)
	
})


test_that("A warning is generated if a config is generated if a config file doesn't exist", {
	
	# get example config files
	testPathConfig <- normalizePath(file.path("../files", "config"))
	configFiles <- list.files(testPathConfig)
			
	configFilesWarning <- c(configFiles, "config-ciao")
	expect_warning(
	  clinDataReview:::checkReportTitles(configFilesWarning, configDir = testPathConfig),
		"Please check the spelling is correct"
	)
      
})

test_that("Paths of markdown files are extracted correctly from the config files", {
	
	# get example config files			
	testPathBase <- normalizePath(path = "../files")
	testPathConfig <- file.path(testPathBase, "config")
	configFiles <- list.files(testPathConfig)
	testPathInterim <- file.path(testPathBase, "interim")
	otherConfigs <- configFiles[!grepl("config.yml", configFiles)]
      
	mdFiles <- clinDataReview:::getMdFromConfig(configFiles, intermediateDir = testPathInterim)
	expect_type(mdFiles, "character")
	expect_length(mdFiles, length(configFiles))
      
	mdNames <- file.path(
		testPathInterim,
		c(
			gsub("config-(.+).yml", "\\1.md", otherConfigs),
			"index.md"
		))
	expect_identical(object = mdNames, expected = mdFiles)
      
})

test_that("Names of markdown files are extracted correctly from the config files when a custom index file is specified", {
    
	# get example config files			
	testPathBase <- normalizePath(path = "../files")
	testPathConfig <- file.path(testPathBase, "config")
	configFiles <- list.files(testPathConfig)
	otherConfigs <- configFiles[!grepl("config.yml", configFiles)]
	testPathInterim <- file.path(testPathBase, "interim")
			
	# Different name for indexPath
	mdFilesIndex <- clinDataReview:::getMdFromConfig(
		configFiles,
		indexPath = "myIndex.Rmd",
		intermediateDir = testPathInterim
	)
	expect_type(mdFilesIndex, "character")
	expect_length(object = mdFilesIndex, n = length(configFiles))
	mdNames <- file.path(
		testPathInterim,
		c(
			gsub("config-(.+).yml", "\\1.md", otherConfigs),
			"myIndex.md"
		)
	)
	expect_identical(object = mdNames, expected = mdFilesIndex)
      
})

test_that("Names of markdown files are extracted correctly from the config files when a custom intermediate directory is specified", {
		
	# get example config files			
	testPathBase <- normalizePath(path = "../files")
	testPathConfig <- file.path(testPathBase, "config")
	configFiles <- list.files(testPathConfig)
	otherConfigs <- configFiles[!grepl("config.yml", configFiles)]
			
	mdFilesInterim <- clinDataReview:::getMdFromConfig(configFiles, intermediateDir = "myDir")
	expect_type(mdFilesInterim, "character")
	expect_length(mdFilesInterim, length(configFiles))
	mdNames <- file.path("myDir", c(
		gsub("config-(.+).yml", "\\1.md", otherConfigs),
		"index.md"
	))
	expect_identical(object = mdNames, expected = mdFilesInterim)
      
})


test_that("Parameters are extracted correctly from a general config file", {

	# get example config files			
	testPathBase <- normalizePath(path = "../files")
	testPathConfig <- file.path(testPathBase, "config")
	listConfig <- getParamsFromConfig("config.yml", configDir = testPathConfig)
	filePathGeneralConfig <- file.path(testPathConfig, "config.yml")
	
	configFromYaml <- read_yaml(filePathGeneralConfig)
	expect_type(listConfig, "list")
	expect_length(configFromYaml, length(listConfig))
	expect_identical(
		object = listConfig,
		expected = configFromYaml
	)
      
})

test_that("Parameters are extracted correctly from a chapter-specific config file", {
			
	# get example config files			
	testPathBase <- normalizePath(path = "../files")
	testPathConfig <- file.path(testPathBase, "config")
      
	listConfig <- getParamsFromConfig("config-adverseEvents-division.yml", configDir = testPathConfig)

	configFromYamlGeneral <- read_yaml(file.path(testPathConfig, "config.yml"))
	configFromYaml <- read_yaml(file.path(testPathConfig, "config-adverseEvents-division.yml"))
	expect_type(listConfig, "list")
	expect_length(
		object = listConfig, 
		n = length(configFromYamlGeneral) + length(configFromYaml)
	)
	expect_mapequal(
		object = listConfig,
		expected = c(configFromYamlGeneral, configFromYaml)
	)
      
})

test_that("An error is generated if the config directory is not available", {
      
	configDir <- tempfile(pattern = "config")
	expect_error(
		getParamsFromConfig(configFile = "config.yml", configDir = configDir),
		"Config directory: .+ doesn't exist."
	)
	
})

test_that("An error is generated if a config file is not available", {
	
	configDir <- tempfile(pattern = "config")
	dir.create(configDir)
			
	# get example general config file
	testPathConfig <- normalizePath(file.path("../files", "config"))
	file.copy(from = file.path(testPathConfig, "config.yml"), to = configDir)
	
	expect_error(
		getParamsFromConfig(configFile = "config-test.yml", configDir = configDir),
		"File .+ cannot be found."
	)
      
})

test_that("A warning is generated if the general config file is not available", {
      
	configFileTemp <- tempfile(pattern = "config-", fileext = ".yml")
	write_yaml(list(), configFileTemp)
      
	expect_warning(
		output <- getParamsFromConfig(
			configFile = basename(configFileTemp), 
			configDir = dirname(configFileTemp)
		),
		"General config file: .+ not available"
	)
	expect_type(output, "list")
      
})

test_that("The creation of html file from the specified markdown files is successful", {
			
	# fix for: 'Using anchor_sections requires Pandoc 2.0+'
	skip_if_not(
		condition = rmarkdown::pandoc_available(version = "2.0"), 
		message = "pandoc 2.0 is not available"
	)
			
	# get example config files
	testPathBase <- normalizePath(path = "../files")
	testPathConfig <- file.path(testPathBase, "config")
	testPathInterim <- file.path(testPathBase, "interim")
	
	# copy md file to temp directory
	testDir <- tempfile("mdConversion")
	dir.create(testDir)
	tmp <- file.copy(from = testPathInterim, to = testDir, recursive = TRUE)
	interimDir <- file.path(testDir, basename(testPathInterim))

	# Md files
	filePathMd <- file.path(interimDir, "adverseEvents-division.md")
      
	expect_message(
		htmlOutput <- clinDataReview:::convertMdToHtml(
			mdFile = filePathMd, 
			indexPath = file.path(testPathBase, "index.Rmd"), 
			intermediateDir = interimDir,
			outputDir = interimDir,
			quiet = TRUE, # suppress printing of pandoc cmd line
		),
		"Convert the Markdown file.+to html.+"
	)
	expect_type(htmlOutput, "character")
	expect_true(file.exists(htmlOutput))
	expect_equal(normalizePath(dirname(htmlOutput)), normalizePath(interimDir))
      
})

test_that("The template name is successfully checked from correct config files", {
			
	# get example config files
	testPathBase <- normalizePath(path = "../files")
	testPathConfig <- file.path(testPathBase, "config")
	configFiles <- list.files(testPathConfig)
      
	expect_silent(
		checkedConfig <- clinDataReview:::checkTemplatesName(
			configFiles = configFiles, configDir = testPathConfig
		)
	)
	expect_type(checkedConfig, "character")
	expect_identical(object = checkedConfig, expected = configFiles)
      
})

test_that("A warning is generated if a config file has no template name", {
      
	testDir <- tempfile("config-templateName")
	dir.create(testDir)
	
	# create a general config file
	configFileGeneral <- file.path(testDir, "config.yml")
	write_yaml(x = list(), file = configFileGeneral)
      
	configFileTemplate <- tempfile(
		pattern = "config-test", fileext = ".yml", 
		tmpdir = testDir
	)
	write_yaml(
		x = list(reportTitle = "Title", reportTitleLevel = 2),
		file = configFileTemplate 
	)
      
	expect_warning(
		clinDataReview:::checkTemplatesName(
			configFiles = basename(configFileTemplate), 
			configDir = testDir
		),
		"Import of parameters from config file .+ failed with error:"
	)
      
})

test_that("A warning is generated if config files with the same template name in different packages are used", {
      
	testDir <- tempfile("config-sameTemplateName")
	dir.create(testDir)
	
	# create a general config file
	configFileGeneral <- file.path(testDir, "config.yml")
	write_yaml(x = list(), file = configFileGeneral)
			
	# create a first config file
	configFileTemplate <- tempfile(pattern = "config-", fileext = ".yml", tmpdir = testDir)
	write_yaml(
		x = list(reportTitle = "Title", reportTitleLevel = 2,
			template = "divisionTemplate", templatePackage = "custom"
		),
		file = configFileTemplate 
	)
	configFileTemplate <- basename(configFileTemplate)  
      
	# create a second config file
	configFileTemplateBis <- tempfile(pattern = "config-", fileext = ".yml", tmpdir = testDir)
	write_yaml(
		x = list(reportTitle = "Title", reportTitleLevel = 2,
		template = "divisionTemplate", templatePackage = "clinDataReview"
		),
		file = configFileTemplateBis 
	)
	configFileTemplateBis <- basename(configFileTemplateBis)  
      
	# check templates
	configFileTemplates <- basename(c(configFileTemplate, configFileTemplateBis))
	expect_warning(
		output <- clinDataReview:::checkTemplatesName(
			configFiles = configFileTemplates, 
			configDir = testDir
		),
		"The following config file.* are ignored, because the same template name is used"
	)
	expect_length(output, 0)
      
})

test_that("Session informations are merged successfully", {
      
	sessionInfos <- list(sessionInfo(), sessionInfo())
	sessionInfo <- do.call(clinDataReview:::merge.sessionInfo, sessionInfos)
	expect_s3_class(sessionInfo, "sessionInfo")
      
})

test_that("Session informations with different elements are merged successfully", {
			
	sessionInfos <- list(sessionInfo(), sessionInfo())
	sessionInfos[[1]]$loadedOnly <- NULL
	expect_silent(
		sessionInfo <- do.call(clinDataReview:::merge.sessionInfo, sessionInfos)
	)
	expect_s3_class(sessionInfo, "sessionInfo")
			
})

test_that("The export of empty session infos to Markdown returns an empty output", {
      
	expect_null(clinDataReview:::exportSessionInfoToMd(sessionInfos = NULL))
	
})

test_that("Session infos are successfully exported to Markdown", {
			
	interimDir <- tempfile("interim")			
	sessionInfos <- list(sessionInfo(), sessionInfo())
      
	mdFile <- clinDataReview:::exportSessionInfoToMd(
		sessionInfos = sessionInfos, 
		intermediateDir = interimDir,
		quiet = TRUE # suppress printing of pandoc cmd line
	)
	expect_type(mdFile, "character")
	expect_true(file.exists(mdFile))
	expect_identical(mdFile, file.path(interimDir, "sessionInfo.md"))
      
})

test_that("A clinical data report is created successfully via the specification of config files", {
	
	skip_on_cran()
	
	# fix for: 'Using anchor_sections requires Pandoc 2.0+'
	skip_if_not(
		condition = rmarkdown::pandoc_available(version = "2.0"), 
		message = "pandoc 2.0 is not available"
	)
			
	# get example config files
	testPathBase <- normalizePath(path = "../files")
	testPathConfig <- file.path(testPathBase, "config")
	configFiles <- list.files(testPathConfig)
	indexPath <- file.path(testPathBase, "index.Rmd")
			
	testDir <- tempfile("report")
	dir.create(testDir)
	outputDir <- file.path(testDir, "report")
	
	# copy index file
	file.copy(from = indexPath, to = testDir)
	# copy configuration files
	file.copy(from = testPathConfig, to = testDir, recursive = TRUE)
      
	output <- render_clinDataReviewReport(
		configFiles = configFiles,
		configDir = testPathConfig,
		inputDir = testDir,
		intermediateDir = file.path(testDir, "interim"),
		outputDir = outputDir,
		quiet = TRUE, # suppress printing of pandoc cmd line
		verbose = FALSE
	)
	expect_type(output, "character")
	expect_match(output, "introduction")
	htmlFiles <- list.files(pattern = "html", outputDir)
	expect_true(
		any(grepl("1-introduction", htmlFiles))
	)
      
})

test_that("A clinical data report is created successfully with a log file", {
			
	skip_on_cran()
	
	# fix for: 'Using anchor_sections requires Pandoc 2.0+'
	skip_if_not(
		condition = rmarkdown::pandoc_available(version = "2.0"), 
		message = "pandoc 2.0 is not available"
	)
	
	# get example config files
	testPathBase <- normalizePath(path = "../files")
	testPathConfig <- file.path(testPathBase, "config")
	indexPath <- file.path(testPathBase, "index.Rmd")
			
	testDir <- tempfile("report")
	dir.create(testDir)
	
	# copy index file
	file.copy(from = indexPath, to = testDir)
	# copy configuration files
	file.copy(from = testPathConfig, to = testDir, recursive = TRUE)
      
	logPath <- file.path(testDir, "log.txt")
      
	output <- render_clinDataReviewReport(
		configDir = testPathConfig,
		inputDir = testDir,
		intermediateDir = file.path(testDir, "interim"),
		outputDir = file.path(testDir, "report"),
		logFile = logPath
	)
	expect_type(output, "character")
	expect_match(output, "introduction")
	expect_true(file.exists(logPath))

})

test_that("A clinical data report is created successfully via the specification of a config directory", {

	skip_on_cran()
	
	# fix for: 'Using anchor_sections requires Pandoc 2.0+'
	skip_if_not(
		condition = rmarkdown::pandoc_available(version = "2.0"), 
		message = "pandoc 2.0 is not available"
	)
	
	# get example config files
	testPathBase <- normalizePath(path = "../files")
	testPathConfig <- file.path(testPathBase, "config")
	configFiles <- list.files(testPathConfig)
	indexPath <- file.path(testPathBase, "index.Rmd")
			
	testDir <- tempfile("report")
	dir.create(testDir)
	outputDir <- file.path(testDir, "report")
	
	# copy index file
	file.copy(from = indexPath, to = testDir)
	# copy configuration files
	file.copy(from = testPathConfig, to = testDir, recursive = TRUE)
	
	output <- render_clinDataReviewReport(
		configFiles = NULL,
		configDir = file.path(testDir, basename(testPathConfig)),
		inputDir = testDir,
		intermediateDir = file.path(testDir, "interim"),
		outputDir = outputDir,
		quiet = TRUE, # suppress printing of pandoc cmd line
		verbose = FALSE
	)
	expect_type(output, "character")
	expect_match(output, "introduction")
	htmlFiles <- list.files(pattern = "html", path = outputDir)
	sectionName <- clinDataReview:::checkReportTitles(configFiles, configDir = testPathConfig)
	sectionName <- gsub(" ", "-", sectionName)
	expect_true(
		any(grepl(sectionName, htmlFiles, ignore.case = TRUE))
	)
      
})

test_that("A warning is generated if a specified config file doesn't exist", {
      
	skip_on_cran()
	
	# fix for: 'Using anchor_sections requires Pandoc 2.0+'
	skip_if_not(
		condition = rmarkdown::pandoc_available(version = "2.0"), 
		message = "pandoc 2.0 is not available"
	)
	
	# get example config files
	testPathBase <- normalizePath(path = "../files")
	indexPath <- file.path(testPathBase, "index.Rmd")
			
	testDir <- tempfile("report")
	dir.create(testDir)
	  
	# copy index file
	file.copy(from = indexPath, to = testDir)
      
	# file 1
	configFile1 <- file.path(testDir, "configFile1.yml")
	write_yaml(
		x = list(
			template = "divisionTemplate.Rmd",
			templatePackage = "clinDataReview",
			reportTitle = "Adverse events",
			reportTitleLevel = 1          
		),
		file = configFile1 
	)
      
	# general config file
	configFileGeneral <- file.path(testDir, "config.yml") 
	write_yaml(
		x = list(
			study = "Study name",
			pathDataFolder = "path/to/data",
			config = list(
				basename(configFile1),
				"configFile2.yml"
			)
		),
		file = configFileGeneral
	)
	configFiles <- c(configFileGeneral, configFile1, "configFile2.yml")
	configFiles <- basename(configFiles)
	
	expect_warning(
		output <- render_clinDataReviewReport(
			configDir = testDir,
			outputDir = testDir,
			inputDir = testDir,
			intermediateDir = testDir,
			quiet = TRUE, # suppress printing of pandoc cmd line
			verbose = FALSE
		),
		"configFile2.* cannot be found"
	)
	expect_type(output, "character")
	expect_match(output, "introduction")
      
})

test_that("A warning is generated if a template report is already available", {
			
	skip_on_cran()
	
	# fix for: 'Using anchor_sections requires Pandoc 2.0+'
	skip_if_not(
		condition = rmarkdown::pandoc_available(version = "2.0"), 
		message = "pandoc 2.0 is not available"
	)
	
	# get example config files
	testPathBase <- normalizePath(path = "../files")
	indexPath <- file.path(testPathBase, "index.Rmd")
      
	testDir <- tempfile("report")
	dir.create(testDir)
			
	file.copy(from = indexPath, to = testDir)
      
	# file 1
	configFile1 <- file.path(testDir, "configFile1.yml")
	write_yaml(
		x = list(
			template = "divisionTemplate.Rmd",
			templatePackage = "clinDataReview",
			reportTitle = "Adverse events",
			reportTitleLevel = 1          
		),
		file = configFile1 
	)
      
	# general config file
	configFileGeneral <- file.path(testDir, "config.yml") 
	write_yaml(
		x = list(
			study = "Study name",
			pathDataFolder = "path/to/data",
			config = list(
				basename(configFile1)
			)
		),
		file = configFileGeneral
	)
	configFiles <- c(configFileGeneral, configFile1)
	configFiles <- basename(configFiles)
	
	# progress messages during execution
	firstRun <- render_clinDataReviewReport(
		configDir = testDir,
		outputDir = testDir,
		inputDir = testDir,
		intermediateDir = testDir,
		quiet = TRUE, # suppress printing of pandoc cmd line
		verbose = FALSE
	)
	
	expect_warning(
		secondRun <- render_clinDataReviewReport(
			configDir = testDir,
			outputDir = testDir,
			inputDir = testDir,
			intermediateDir = testDir,
			quiet = TRUE, # suppress printing of pandoc cmd line 
			verbose = FALSE
		),
		"Document with similar name than specified template from"
	)
	expect_type(secondRun, "character")
	expect_match(secondRun, regexp = "introduction")
      
})

test_that("A warning is generated if the creation of a chapter fails'", {
     
	skip_on_cran()
	
	# fix for: 'Using anchor_sections requires Pandoc 2.0+'
	skip_if_not(
		condition = rmarkdown::pandoc_available(version = "2.0"), 
		message = "pandoc 2.0 is not available"
	)
	
	# get example config files
	testPathBase <- normalizePath(path = "../files")
	indexPath <- file.path(testPathBase, "index.Rmd")
			
	testDir <- tempfile("report")
	dir.create(testDir)
	
	file.copy(from = indexPath, to = testDir)
      
	# file 1
	configFile1 <- file.path(testDir, "configFile1.yml")
	write_yaml(
	  x = list(
	    template = "templateWithError.Rmd", 
	    sectionTitle = "Chapter with error"
	 ), 
    file = configFile1
  )
	
	# create a Rmd file returning an error
	cat(
		"```{r}", 
		"stop('test')",
		"```",
		file = file.path(testDir, "templateWithError.Rmd"),
		sep = "\n"
	)
      
	# general config file
	configFileGeneral <- file.path(testDir, "config.yml") 
	write_yaml(
		x = list(
			study = "Study name",
			pathDataFolder = "path/to/data",
			config = list(basename(configFile1))
		),
		file = configFileGeneral
	)
	configFiles <- c(configFileGeneral, configFile1)
	configFiles <- basename(configFiles)
	
	expect_warning(
	    render_clinDataReviewReport(
	      configDir = testDir,
	      outputDir = testDir,
	      intermediateDir = testDir,
	      inputDir = testDir,
	      quiet = TRUE, # suppress printing of pandoc cmd line 
		  verbose = FALSE
		),
    	paste(
		"Rendering of the .+ report for config file:.+configFile1.yml.+failed,",
      	"a report with only the section title is created.+")
  )
      
})

test_that("A warning is generated if the template name is missing in a config file", {
			
	skip_on_cran()
	
	# fix for: 'Using anchor_sections requires Pandoc 2.0+'
	skip_if_not(
		condition = rmarkdown::pandoc_available(version = "2.0"), 
		message = "pandoc 2.0 is not available"
	)
	
	# get example config files
	testPathBase <- normalizePath(path = "../files")
	indexPath <- file.path(testPathBase, "index.Rmd")
			
	testDir <- tempfile("report")
	dir.create(testDir)
	
	file.copy(from = indexPath, to = testDir)
      
	# file 1
	configFile1 <- file.path(testDir, "configFile1.yml")
	write_yaml(
		x = list(
			reportTitle = "Adverse events",
			reportTitleLevel = 1          
		),
		file = configFile1 
	)
      
	# general config file
	configFileGeneral <- file.path(testDir, "config.yml") 
	write_yaml(
		x = list(
			study = "Study name",
			pathDataFolder = "path/to/data",
			config = list(basename(configFile1))
		),
		file = configFileGeneral
	)
	configFiles <- c(configFileGeneral, configFile1)
	configFiles <- basename(configFiles)
	
	expect_warning(
		res <- render_clinDataReviewReport(
			configDir = testDir,
			outputDir = testDir,
			intermediateDir = testDir,
			inputDir = testDir,
			quiet = TRUE, # suppress printing of pandoc cmd line 
			verbose = FALSE
		),
		"Template missing for config file: .+."
	)
	expect_type(res, "character")
	expect_match(object = res, regexp = "introduction")      
      
})

test_that("A warning is generated if the config parameters don't comply to the template specifications", {
    
	skip_on_cran()
	
	# fix for: 'Using anchor_sections requires Pandoc 2.0+'
	skip_if_not(
		condition = rmarkdown::pandoc_available(version = "2.0"), 
		message = "pandoc 2.0 is not available"
	)
	
	# get example config files
	testPathBase <- normalizePath(path = "../files")
	indexPath <- file.path(testPathBase, "index.Rmd")
			
	testDir <- tempfile("report")
	dir.create(testDir)
	
	file.copy(from = indexPath, to = testDir)
      
	# file 1
	configFile1 <- file.path(testDir, "configFile1.yml")
	write_yaml(
		x = list(
			template = "divisionTemplate.Rmd",
			templatePackage = "clinDataReview"
		),
		file = configFile1 
	)
      
	# general config file
	configFileGeneral <- file.path(testDir, "config.yml") 
	write_yaml(
		x = list(
			study = "Study name",
			pathDataFolder = "path/to/data",
			config = list(basename(configFile1))
		),
		file =  configFileGeneral
	)
	configFiles <- c(configFileGeneral, configFile1)
	configFiles <- basename(configFiles)
	
	expect_warning(
		res <- render_clinDataReviewReport(
			configDir = testDir,
			outputDir = testDir,
			intermediateDir = testDir,
			inputDir = testDir,
			quiet = TRUE, # suppress printing of pandoc cmd line     
			verbose = FALSE
		),
		"The report for the config file: .+ is not created because the check of the parameters failed"
	)
	expect_type(res, "character")
	expect_match(object = res, regexp = "introduction")      

})

test_that("A warning is generated if a template is not available in the specified package", {
      
	skip_on_cran()
	
	# fix for: 'Using anchor_sections requires Pandoc 2.0+'
	skip_if_not(
		condition = rmarkdown::pandoc_available(version = "2.0"), 
		message = "pandoc 2.0 is not available"
	)
	
	# get example config files
	testPathBase <- normalizePath(path = "../files")
	indexPath <- file.path(testPathBase, "index.Rmd")
			
	testDir <- tempfile("report")
	dir.create(testDir)
			
	file.copy(from = indexPath, to = testDir)
      
	# file 1
	configFile1 <- file.path(testDir, "configFile1.yml")
	write_yaml(
		x = list(
			template = "divisionTemplate.Rmd",
			templatePackage = "myPackage"       
		),
		file = configFile1 
	)
      
	# general config file
	configFileGeneral <- file.path(testDir, "config.yml") 
	write_yaml(
		x = list(
			study = "Study name",
			pathDataFolder = "path/to/data",
			config = list(basename(configFile1))
		),
		file = configFileGeneral
	)
	configFiles <- c(configFileGeneral, configFile1)
	configFiles <- basename(configFiles)
	
	expect_warning(
		res <- render_clinDataReviewReport(
			configDir = testDir,
			outputDir = testDir,
			intermediateDir = testDir,
			inputDir = testDir ,
			quiet = TRUE, # suppress printing of pandoc cmd line 
			verbose = FALSE
		),
		"Template file: .+ not available in.*myPackage.+ package."
	)
	expect_type(res, "character")
	expect_match(object = res, regexp = "introduction")
      
})

test_that("A warning is generated if no config parameters are available", {
      
	skip_on_cran()
	
	# fix for: 'Using anchor_sections requires Pandoc 2.0+'
	skip_if_not(
		condition = rmarkdown::pandoc_available(version = "2.0"), 
		message = "pandoc 2.0 is not available"
	)
	
	# get example config files
	testPathBase <- normalizePath(path = "../files")
	indexPath <- file.path(testPathBase, "index.Rmd")
			
	testDir <- tempfile("report")
	dir.create(testDir)
			
	file.copy(from = indexPath, to = testDir)
      
	# file 1
	configFile1 <- file.path(testDir, "configFile1.yml")
	write_yaml(
		x = list(
			template = "subjectProfile.Rnw",
			templatePackage = "patientProfilesVis"
		),
		file = configFile1 
	)
      
	# general config file
	configFileGeneral <- file.path(testDir, "config.yml") 
	write_yaml(
		x = list(
			study = "Study name",
			pathDataFolder = "path/to/data",
			config = list(basename(configFile1))
		),
		configFileGeneral
	)
	configFiles <- c(configFileGeneral, configFile1)
	configFiles <- basename(configFiles)
	     
	expect_warning(
		res <- render_clinDataReviewReport(
			configDir = testDir,
			outputDir = testDir,
			intermediateDir = testDir,
			inputDir = testDir,
			quiet = TRUE, # suppress printing of pandoc cmd line     
			verbose = FALSE
		),
		"No config parameter available, input parameters for the report are not checked."
	)
      
})

test_that("A warning is generated if some Markdown files are missing for conversion to html", {
			
	testDir <- tempfile("report")
	dir.create(testDir)
	
	# get example config files
	testPathBase <- normalizePath(path = "../files")
	indexPath <- file.path(testPathBase, "index.Rmd")
			
	file.copy(from = indexPath, to = testDir)
      
	# file 1
	configFile1 <- file.path(testDir, "configFile1.yml")
	write_yaml(
		x = list(
			template = "divisionTemplate.Rmd",
			templatePackage = "myPackage"  
		),
		file = configFile1 
	)
      
	# general config file
	configFileGeneral <- file.path(testDir, "config.yml") 
	write_yaml(
		x = list(
			study = "Study name",
			pathDataFolder = "path/to/data",
			config = list(basename(configFile1))
		),
		file = configFileGeneral
	)
	configFiles <- c(configFileGeneral, configFile1)
	configFiles <- basename(configFiles)
	
	expect_error(
		expect_warning(
			postProcessReport(
				configDir = testDir,
				outputDir = testDir,
				intermediateDir = testDir,
				inputDir = testDir,
				mdFiles = NULL,
				quiet = TRUE, # suppress printing of pandoc cmd line  
				verbose = FALSE
			),
			"Markdown file(s): .+ are missing, these files are ignored."
		)
	)
      
})
	
test_that("Config parameters with R code are correctly evaluated", {
	
	configDir <- tempfile("config-r-eval")
	dir.create(configDir)
	
	# get example general config file
	testPathConfig <- normalizePath(file.path("../files", "config"))
	file.copy(from = file.path(testPathConfig, "config.yml"), to = configDir)
			
	configFileTemp <- tempfile(pattern = "config-", fileext = ".yml", tmpdir = configDir)
	write_yaml(list(param = structure("toString(c('a', 'b'))", tag = "!r")), configFileTemp)
			
	# parameter is replaced by its evaluated version:
	params <- getParamsFromConfig(
		configFile = basename(configFileTemp), 
		configDir = configDir
	)
	expect_equal(params$param, "a, b")
				
})
	
test_that("Config parameters with R code and lazy-evaluation are imported non evaluated", {
	
	configDir <- tempfile("config-r-eval-lazy")
	dir.create(configDir)
	
	# get example general config file
	testPathConfig <- normalizePath(file.path("../files", "config"))
	file.copy(from = file.path(testPathConfig, "config.yml"), to = configDir)
			
	configFileTemp <- tempfile(pattern = "config-", fileext = ".yml", tmpdir = configDir)
	write_yaml(list(param = structure("nrow(dataI)", tag = "!r-lazy")), configFileTemp)
				
	params <- getParamsFromConfig(
		configFile = basename(configFileTemp), 
		configDir = configDir
	)
	expect_type(params, "list")
	expect_s3_class(params[["param"]], c("r-lazy", "character"))
	expect_equal(as.character(params[["param"]]), "nrow(dataI)")
				
})

test_that("An error is generated if the R code specified in config parameters with lazy-evaluation contains errors", {
	
	params <- list(nrowData = structure("nrow(customData)", class = "r-lazy"))
	# error if R object is missing
	expect_error(
		forceParams(params),
		".*customData.* not found"
	)
	
})

test_that("Config parameters with R code and lazy-evaluation are correctly evaluated when requested", {
			
	params <- list(nrowData = structure("nrow(customData)", class = "r-lazy"))
	
	# parameter is replaced by its evaluated version:
	customData <- data.frame(A = seq.int(2), B = c("A", "B"))
	expect_silent(paramEvaluated <- forceParams(params))
	expect_type(paramEvaluated, "list")
	expect_named(paramEvaluated, "nrowData")
	expect_equal(paramEvaluated$nrowData, nrow(customData))
			
})

test_that("A clinical data report is created successfully in parallel", {
			
	skip_on_cran()
	
	# fix for: 'Using anchor_sections requires Pandoc 2.0+'
	skip_if_not(
		condition = rmarkdown::pandoc_available(version = "2.0"), 
		message = "pandoc 2.0 is not available"
	)
	
	testDir <- tempfile("report")
	dir.create(testDir)
	
	# create multiple config files
	configDir <- file.path(testDir, "config")
	dir.create(configDir)
	nChapters <- 4
	configFiles <- file.path(configDir, 
		paste0("config-division-", seq_len(nChapters), ".yml")
	)
	for(i in seq_len(nChapters)){
		yaml::write_yaml(
			list(
				template = "divisionTemplate.Rmd",
				templatePackage = "clinDataReview",
				reportTitle = paste("Chapter", i),
				reportTitleLevel = 1
			),
			configFiles[i]
		)
	}
	# create general config file
	yaml::write_yaml(
		x = list(config = as.list(basename(configFiles))),
		file = file.path(configDir, "config.yml")
	)
			
	# example index file
	indexPath <- file.path(testDir, "index.Rmd")
	cat(
		"---",
		"title: 'Clinical data report'",
		"output: clinDataReview::gitbook_clinDataReview_report",
		"---",
		"# Introduction", 
		file = indexPath, sep = "\n"
	)
	
	outputDir <- file.path(testDir, "report")
	output <- render_clinDataReviewReport(
		configDir = configDir,
		inputDir = testDir,
		intermediateDir = file.path(testDir, "interim"),
		outputDir = outputDir,
		nCores = min(parallel::detectCores(), 2),
		quiet = TRUE, # suppress printing of pandoc cmd line
		verbose = FALSE
	)
	
	# check that all filenames are correct
	htmlFiles <- list.files(pattern = ".html$", outputDir)
	htmlFiles <- setdiff(htmlFiles, "404.html")
	htmlFilesExp <- c(
		"1-introduction.html", 
		paste0(2:5, "-chapter-", 1:4, ".html"),
		"6-appendix.html"
	)
	expect_setequal(object = htmlFiles, expected = htmlFilesExp)
	
})

test_that("A clinical data report is created successfully in parallel with one chapter containing parallel execution", {
  
  skip_on_cran()
  
  # fix for: 'Using anchor_sections requires Pandoc 2.0+'
  skip_if_not(
	  condition = rmarkdown::pandoc_available(version = "2.0"), 
	  message = "pandoc 2.0 is not available"
  )
  
  testDir <- tempfile("report")
  dir.create(testDir)
  
  configDir <- file.path(testDir, "config")
  dir.create(configDir)
  
  # create multiple config files
  nChapters <- 4
  configFiles <- file.path(configDir, 
    paste0("config-division-", seq_len(nChapters), ".yml")
  )
  for(i in seq_len(nChapters)){
    yaml::write_yaml(
      x = list(
        template = "divisionTemplate.Rmd",
        templatePackage = "clinDataReview",
        reportTitle = paste("Chapter", i),
        reportTitleLevel = 1
      ),
      file = configFiles[i]
    )
  }
  
  # create example chapter run in parallel
  chapterParallel <- file.path(testDir, "chapter-parallel.Rmd")
  cat(
    "# Parallel",
    "```{r eval = TRUE, echo = FALSE, class.output = \"testParallel\", comment = NA}",
    "library(parallel)",
    "cl <- makeCluster(2)",
    "print(clusterApply(cl, 1:2, get('+'), 3))",
    "stopCluster(cl = cl)",
    "```",
    file = chapterParallel, sep = "\n"
  )
  configFileParallel <- file.path(configDir, "config-parallel.yml")
  yaml::write_yaml(
    x = list(
      template = basename(chapterParallel),
      parallel = TRUE
    ),
    file = configFileParallel
  )
  configFiles <- c(configFiles, configFileParallel)
  
  # create general config file
  yaml::write_yaml(
    x = list(config = as.list(basename(configFiles))),
    file = file.path(configDir, "config.yml")
  )
  
  # example index file
  indexPath <- file.path(testDir, "index.Rmd")
  cat(
    "---",
    "title: 'Clinical data report'",
    "output: clinDataReview::gitbook_clinDataReview_report",
    "---",
    "# Introduction", 
    file = indexPath, sep = "\n"
  )
  
  # progress messages during execution
  outputDir <- file.path(testDir, "report")
  expect_error(
	output <- render_clinDataReviewReport(
		configDir = configDir,
		inputDir = testDir,
		intermediateDir = file.path(testDir, "interim"),
		outputDir = outputDir,
		nCores = min(parallel::detectCores(), 2),
		quiet = TRUE, # suppress printing of pandoc cmd line
		verbose = FALSE
	),
    NA
  )
  
  # check that all filenames are correct
  htmlFiles <- list.files(pattern = "html", outputDir)
  htmlFiles <- setdiff(htmlFiles, "404.html")
  htmlFilesExp <- c(
    "1-introduction.html", 
    paste0(2:5, "-chapter-", 1:4, ".html"),
    "6-parallel.html",
    "7-appendix.html"
  )
  expect_setequal(object = htmlFiles, expected = htmlFilesExp)

  # check the content of the chapter run in parallel
  htmlFileParallel <- xml2::read_xml(x = file.path(outputDir, "6-parallel.html"))
  headXML <- xml2::xml_find_all(x = htmlFileParallel, xpath = ".//h1[span]")
  expect_equal(object = xml2::xml_text(headXML), expected = "6 Parallel")
  
  codeXML <- xml2::xml_find_all(
    x = htmlFileParallel, 
    xpath = ".//pre[contains(@class, 'testParallel')]//code"
  )
  expect_match(object = xml2::xml_text(codeXML), regexp = "4.+5")
  
})

test_that("A report is correctly split at different chapter-specific levels", {
			
	skip_on_cran()
	
	# fix for: 'Using anchor_sections requires Pandoc 2.0+'
	skip_if_not(
		condition = rmarkdown::pandoc_available(version = "2.0"), 
		message = "pandoc 2.0 is not available"
	)
			
	testDir <- tempfile("report")
	dir.create(testDir)
			
	# create multiple config files + template
	template <- file.path(testDir, "chapter.Rmd")
	cat(
		"# Chapter",
		"## Section 1",
		"### Subsection 1",
		"### Subsection 2",
		"## Section 2",
		file = template, sep = "\n"
	)
	configDir <- file.path(testDir, "config")
	dir.create(configDir)
	configFiles <- file.path(configDir, 
		paste0("config-division-", c("chapter", "section", "subsection"), ".yml")
	)
	for(i in seq_along(configFiles)){
		yaml::write_yaml(
			x = list(
				template = basename(template),
				split_by = as.integer(i),
				reportTitle = paste("Chapter split at level", i)
			),
			file = configFiles[i]
		)
	}
	# create general config file
	yaml::write_yaml(
		x = list(config = as.list(basename(configFiles))),
		file = file.path(configDir, "config.yml")
	)
			
	# example index file
	indexPath <- file.path(testDir, "index.Rmd")
	cat(
		"---",
		"title: 'Clinical data report'",
		"output:",
		"  clinDataReview::gitbook_clinDataReview_report:",
		"    split_by: 'section+number'",
		"---",
		"# Introduction", 
		file = indexPath, sep = "\n"
	)
			
	# progress messages during execution
	outputDir <- file.path(testDir, "report")
	output <- render_clinDataReviewReport(
		configDir = configDir,
		inputDir = testDir,
		intermediateDir = file.path(testDir, "interim"),
		outputDir = outputDir,
		quiet = TRUE, # suppress printing of pandoc cmd line
		verbose = FALSE
	)
	expect_type(output, "character")
	expect_match(output, "introduction")
	
	# check that all filenames are correct
	htmlFiles <- list.files(pattern = "html", outputDir)
	htmlFiles <- setdiff(htmlFiles, "404.html")
	htmlFilesExp <- c(
		"1-introduction.html", 
		# split at chapter level
		"2-chapter.html", 
		# split at the section level
		"3-chapter.html",
		"3.1-section-1.html", "3.2-section-2.html",
		# split at the subsection level
		"4-chapter.html",
		"4.1-section-1.html", "4.1.1-subsection-1.html", 
		"4.1.2-subsection-2.html", "4.2-section-2.html",
		"5-appendix.html",
		"5.1-session-information.html"
	)
	expect_setequal(object = htmlFiles, expected = htmlFilesExp)
	
	# check if content is correct
	getHeaders <- function(file){
	  x <- xml2::read_xml(x = file)
		headXML <- xml2::xml_find_all(x = x, xpath = ".//h1[span]|.//h2[span]|.//h3[span]")
		headers <- xml2::xml_text(headXML)
		return(headers)
	}
	
	# split at the chapter level
	expect_setequal(
		object = getHeaders(file = file.path(outputDir, "2-chapter.html")),
		expected = c("2 Chapter", "2.1 Section 1", "2.1.1 Subsection 1", 
			"2.1.2 Subsection 2", "2.2 Section 2")
	)
	# split at the section level
	expect_setequal(
		object = getHeaders(file = file.path(outputDir, "3-chapter.html")),
		expected = "3 Chapter"
	)
	expect_setequal(
		object = getHeaders(file = file.path(outputDir, "3.1-section-1.html")),
		expected = c("3.1 Section 1", "3.1.1 Subsection 1", "3.1.2 Subsection 2")
	)
	expect_setequal(
		object = getHeaders(file = file.path(outputDir, "3.2-section-2.html")),
		expected = "3.2 Section 2"
	)
	
	# split at the subsection level
	expect_setequal(
		object = getHeaders(file = file.path(outputDir, "4-chapter.html")),
		expected = "4 Chapter"
	)
	expect_setequal(
		object = getHeaders(file = file.path(outputDir, "4.1-section-1.html")),
		expected = "4.1 Section 1"
	)
	expect_setequal(
		object = getHeaders(file = file.path(outputDir, "4.1.1-subsection-1.html")),
		expected = "4.1.1 Subsection 1"
	)
	expect_setequal(
		object = getHeaders(file = file.path(outputDir, "4.1.2-subsection-2.html")),
		expected = "4.1.2 Subsection 2"
	)
	expect_setequal(
		object = getHeaders(file = file.path(outputDir, "4.2-section-2.html")),
		expected = "4.2 Section 2"
	)
	
})

test_that("The page titles are correctly set for a report", {
  
  skip_on_cran()
  
  # fix for: 'Using anchor_sections requires Pandoc 2.0+'
  skip_if_not(
		condition = rmarkdown::pandoc_available(version = "2.0"), 
		message = "pandoc 2.0 is not available"
  )
  
  testDir <- tempfile("report")
  dir.create(testDir)
  
  # create multiple config files + template
  template <- file.path(testDir, "chapter.Rmd")
  cat(
    "# Chapter",
    "## Section 1",
    "### Subsection 1",
    "### Subsection 2",
    "## Section 2",
    file = template, sep = "\n"
  )
  configDir <- file.path(testDir, "config")
  dir.create(configDir)
  configFiles <- file.path(configDir, 
    paste0("config-division-", c("chapter", "section", "subsection"), ".yml")
  )
  for(i in seq_along(configFiles)){
    yaml::write_yaml(
      x = list(
        template = basename(template),
        split_by = as.integer(i),
        reportTitle = paste("Chapter split at level", i)
      ),
      file = configFiles[i]
    )
  }
  # create general config file
  yaml::write_yaml(
    x = list(config = as.list(basename(configFiles))),
    file = file.path(configDir, "config.yml")
  )
  
  # example index file
  indexPath <- file.path(testDir, "index.Rmd")
  cat(
    "---",
    "title: 'Clinical data report'",
    "output:",
    "  clinDataReview::gitbook_clinDataReview_report:",
    "    split_by: 'section+number'",
    "---",
    "# Introduction", 
    file = indexPath, sep = "\n"
  )
  
  # progress messages during execution
  outputDir <- file.path(testDir, "report")
	output <- render_clinDataReviewReport(
      configDir = configDir,
      inputDir = testDir,
      intermediateDir = file.path(testDir, "interim"),
      outputDir = outputDir,
      quiet = TRUE, # suppress printing of pandoc cmd line
	  verbose = FALSE
    )
  
  getTitle <- function(file){
    x <- xml2::read_xml(x = file)
    titleXML <- xml2::xml_find_all(x = x, xpath = ".//title")
    titles <- xml2::xml_text(titleXML)
    return(titles)
  }
  
  # split at the chapter level
  expect_equal(
    object = getTitle(file = file.path(outputDir, "2-chapter.html")), 
    expected = "2 Chapter | Clinical data report"
  )
  # split at the section level
  expect_equal(
    object = getTitle(file = file.path(outputDir, "3-chapter.html")),
    expected = "3 Chapter | Clinical data report"
  )
  expect_equal(
    object = getTitle(file = file.path(outputDir, "3.1-section-1.html")),
    expected = "3.1 Section 1 | Clinical data report"
  )
  expect_equal(
    object = getTitle(file = file.path(outputDir, "3.2-section-2.html")),
    expected = "3.2 Section 2 | Clinical data report"
  )
  
  # split at the subsection level
  expect_setequal(
    object = getTitle(file = file.path(outputDir, "4-chapter.html")),
    expected = "4 Chapter | Clinical data report"
  )
  expect_equal(
    object = getTitle(file = file.path(outputDir, "4.1-section-1.html")),
    expected = "4.1 Section 1 | Clinical data report"
  )
  expect_equal(
    object = getTitle(file = file.path(outputDir, "4.1.1-subsection-1.html")),
    expected = "4.1.1 Subsection 1 | Clinical data report"
  )
  expect_equal(
    object = getTitle(file = file.path(outputDir, "4.1.2-subsection-2.html")),
    expected = "4.1.2 Subsection 2 | Clinical data report"
  )
  expect_equal(
    object = getTitle(file = file.path(outputDir, "4.2-section-2.html")),
    expected = "4.2 Section 2 | Clinical data report"
  )
  
})

test_that("The table of contents is correctly set for a report", {
  
  skip_on_cran()
  
  # fix for: 'Using anchor_sections requires Pandoc 2.0+'
  skip_if_not(
		  condition = rmarkdown::pandoc_available(version = "2.0"), 
		  message = "pandoc 2.0 is not available"
  )
  
  testDir <- tempfile("report")
  dir.create(testDir)
  
  # create multiple config files + template
  template <- file.path(testDir, "chapter.Rmd")
  cat(
    "# Chapter",
    "## Section 1",
    "## Section 2",
    "### Subsection 1",
    "### Subsection 2", # to test difference of level > 1 with next chapter
    file = template, sep = "\n"
  )
  configDir <- file.path(testDir, "config")
  dir.create(configDir)
  configFiles <- file.path(configDir, 
    paste0("config-division-", c("chapter", "section"), ".yml")
  )
  for(i in seq_along(configFiles)){
    yaml::write_yaml(
      x = list(
        template = basename(template),
        split_by = as.integer(i),
        reportTitle = paste("Chapter split at level", i)
      ),
      file = configFiles[i]
    )
  }
  # create general config file
  yaml::write_yaml(
    x = list(config = as.list(basename(configFiles))),
    file = file.path(configDir, "config.yml")
  )
  
  # example index file
  indexPath <- file.path(testDir, "index.Rmd")
  cat(
    "---",
    "title: 'Clinical data report'",
    "output:",
    "  clinDataReview::gitbook_clinDataReview_report:",
    "    split_by: 'section+number'",
    "---",
    "# Introduction", 
    file = indexPath, sep = "\n"
  )
  
  # progress messages during execution
  outputDir <- file.path(testDir, "report")
    output <- render_clinDataReviewReport(
      configDir = configDir,
      inputDir = testDir,
      intermediateDir = file.path(testDir, "interim"),
      outputDir = outputDir,
      quiet = TRUE, # suppress printing of pandoc cmd line
	  verbose = FALSE
    )
  
	# import introduction and table of contents
	book <- read_html(x = file.path(outputDir, "1-introduction.html"))
	toc <- xml_find_all(book, ".//ul[@class='summary']")
	
	checkHeader <- function(x, title, link){
	  head <- xml2::xml_find_first(x, ".//a")
	  expect_equal(object = xml2::xml_text(head), expected = title)
	  expect_equal(object = xml2::xml_attr(head, "href"), expected = link)
	}
	getSublist <- function(x){
	  xml_children(xml2::xml_find_first(x, ".//ul"))
	}
	
	tocChapters <- xml2::xml_children(toc)
	
	# intro
	checkHeader(
	  x = tocChapters[[1]], 
	  title = "1 Introduction", 
	  link = "1-introduction.html#introduction"
  )
	
	# chapter split at the chapter level
	toc2 <- tocChapters[[2]]
	checkHeader(x = toc2, title = "2 Chapter", link = "2-chapter.html#chapter")
	toc2Sect <- getSublist(toc2)
	checkHeader(x = toc2Sect[[1]], title = "2.1 Section 1", 
	   link = "2-chapter.html#section-1")
	checkHeader(x = toc2Sect[[2]], title = "2.2 Section 2", 
	   link = "2-chapter.html#section-2")
	expect_length(object = getSublist(toc2Sect[[1]]), n = 0)
	toc22Sub <- getSublist(toc2Sect[[2]])
	checkHeader(x = toc22Sub[[1]], title = "2.2.1 Subsection 1", 
	   link = "2-chapter.html#subsection-1")
	checkHeader(x = toc22Sub[[2]], title = "2.2.2 Subsection 2", 
	   link = "2-chapter.html#subsection-2")
	
	# chapter split at the section level
	toc3 <- tocChapters[[3]]
	checkHeader(x = toc3, title = "3 Chapter", 
	   link = "3-chapter.html#chapter")
	toc3Sect <- getSublist(toc3)
	checkHeader(x = toc3Sect[[1]], title = "3.1 Section 1", 
	   link = "3.1-section-1.html#section-1")
	checkHeader(x = toc3Sect[[2]], title = "3.2 Section 2", 
	   link = "3.2-section-2.html#section-2")
	expect_length(object = getSublist(toc3Sect[[1]]), n = 0)
	toc32Sub <- getSublist(toc3Sect[[2]])
	checkHeader(x = toc32Sub[[1]], title = "3.2.1 Subsection 1", 
	   link = "3.2-section-2.html#subsection-1")
	checkHeader(x = toc32Sub[[2]], title = "3.2.2 Subsection 2",
	   link = "3.2-section-2.html#subsection-2")

	# appendix
	tocAppendix <- tocChapters[[4]]
	checkHeader(x = tocAppendix, title = "4 Appendix", 
	   link = "4-appendix.html#appendix")
	tocAppendixSections <- getSublist(tocAppendix)
	checkHeader(x = tocAppendixSections, title = "4.1 Session information", 
	   link = "4.1-session-information.html#session-information")
	
})

test_that("The chapter numbers are correctly set when there are more than 10 sections", {
  
  secLevels <- c(1, rep(2, 11), rep(3, 2), rep(1, 10))
  expect_equal(
    object = clinDataReview:::getTocNumbering(levels = secLevels),
    expected = c("1", paste0("1.", 1:11), paste0("1.11.", 1:2), as.character(2:11))
  )
  
})
