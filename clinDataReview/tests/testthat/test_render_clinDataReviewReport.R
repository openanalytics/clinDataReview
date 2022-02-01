context("Render a clinical data review report")

library(yaml)

test_that("Report titles are extracted for each config file", {
      
	# get example config files
	testPathConfig <- normalizePath(file.path("../files", "config"))
	configFiles <- list.files(testPathConfig)
	otherConfigs <- configFiles[!grepl("config.yml", configFiles)]
			
	reportTitles <- checkReportTitles(configFiles, configDir = testPathConfig)
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
		checkReportTitles(configFilesError, configDir = testPathConfig),
		"The title .+ is duplicated."
	)
	
})


test_that("A warning is generated if a config is generated if a config file doesn't exist", {
	
	# get example config files
	testPathConfig <- normalizePath(file.path("../files", "config"))
	configFiles <- list.files(testPathConfig)
			
	configFilesWarning <- c(configFiles, "config-ciao")
	expect_warning(
		checkReportTitles(configFilesWarning, configDir = testPathConfig),
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

test_that("The creation of html file from markdown files for specified config files is successful", {
     
	skip_if_not(
		condition = rmarkdown::pandoc_available(), 
		message = "pandoc is not available"
	)
	
	# get example config files
	testPathBase <- normalizePath(path = "../files")
	testPathConfig <- file.path(testPathBase, "config")
	testPathInterim <- file.path(testPathBase, "interim")
	
	# copy md file to temp directory
	testDir <- tempfile("mdConversion")
	dir.create(testDir)
	file.copy(from = testPathInterim, to = testDir, recursive = TRUE)
	interimDir <- file.path(testDir, basename(testPathInterim))
	
	htmlOutput <- convertMdToHtml(
		inputDir = testDir,
		outputDir = testDir,
		intermediateDir = interimDir,
		configDir = testPathConfig, 
		mdFiles = NULL,
		indexPath = "index.Rmd",
		quiet = TRUE # suppress printing of pandoc cmd line
	)
	expect_type(htmlOutput, "character")
	expect_true(file.exists(htmlOutput))
	expect_equal(normalizePath(dirname(htmlOutput)), normalizePath(testDir))
	
})

test_that("The creation of html file from the specified markdown files is successful", {
			
	skip_if_not(
		condition = rmarkdown::pandoc_available(), 
		message = "pandoc is not available"
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
	filePathMd <- list.files(pattern = "md", interimDir, full.names = TRUE)
      
	htmlOutput <- convertMdToHtml(
		outputDir = testDir,
		intermediateDir = interimDir,
		configDir = testPathConfig, 
		mdFiles = filePathMd,
		indexPath = "index.Rmd",
		quiet = TRUE # suppress printing of pandoc cmd line
	)
	expect_type(htmlOutput, "character")
	expect_true(file.exists(htmlOutput))
	expect_equal(normalizePath(dirname(htmlOutput)), normalizePath(testDir))
      
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
      
	# progress messages during execution
	expect_message(
		output <- render_clinDataReviewReport(
			configFiles = configFiles,
			configDir = testPathConfig,
			inputDir = testDir,
			intermediateDir = file.path(testDir, "interim"),
			outputDir = outputDir,
			quiet = TRUE # suppress printing of pandoc cmd line
		)
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
      
	# progress messages during execution
	expect_message(
		output <- render_clinDataReviewReport(
			configDir = testPathConfig,
			inputDir = testDir,
			intermediateDir = file.path(testDir, "interim"),
			outputDir = file.path(testDir, "report"),
			logFile = logPath,
			quiet = TRUE # suppress printing of pandoc cmd line
		)
	)
	expect_type(output, "character")
	expect_match(output, "introduction")
	expect_true(file.exists(logPath))

})

test_that("A clinical data report is created successfully via the specification of a config directory", {

	skip_on_cran()
	
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
	
	# progress messages during execution
	expect_message(
		output <- render_clinDataReviewReport(
			configFiles = NULL,
			configDir = file.path(testDir, basename(testPathConfig)),
			inputDir = testDir,
			intermediateDir = file.path(testDir, "interim"),
			outputDir = outputDir,
			quiet = TRUE # suppress printing of pandoc cmd line
		)
	)
	expect_type(output, "character")
	expect_match(output, "introduction")
	htmlFiles <- list.files(pattern = "html", path = outputDir)
	sectionName <- checkReportTitles(configFiles, configDir = testPathConfig)
	sectionName <- gsub(" ", "-", sectionName)
	expect_true(
		any(grepl(sectionName, htmlFiles, ignore.case = TRUE))
	)
      
})

test_that("A warning is generated if a specified config file doesn't exist", {
      
	skip_on_cran()
	
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
		# progress messages during execution
		expect_message(
			output <- render_clinDataReviewReport(
				configDir = testDir,
				outputDir = testDir,
				inputDir = testDir,
				intermediateDir = testDir,
				quiet = TRUE # suppress printing of pandoc cmd line
			)
		),
		"configFile2.* cannot be found"
	)
	expect_type(output, "character")
	expect_match(output, "introduction")
      
})

test_that("A warning is generated if a template report is already available", {
			
	skip_on_cran()
	
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
	expect_message(
		firstRun <- render_clinDataReviewReport(
			configDir = testDir,
			outputDir = testDir,
			inputDir = testDir,
			intermediateDir = testDir,
			quiet = TRUE # suppress printing of pandoc cmd line
		)
	)
	
	expect_warning(
		# progress messages during execution
		expect_message(
			secondRun <- render_clinDataReviewReport(
				configDir = testDir,
				outputDir = testDir,
				inputDir = testDir,
				intermediateDir = testDir,
				quiet = TRUE # suppress printing of pandoc cmd line   
			)
		),
		"Document with similar name than specified template from"
	)
	expect_type(secondRun, "character")
	expect_match(secondRun, regexp = "introduction")
      
})

test_that("A warning is generated if the creation of a chapter fails'", {
     
	skip_on_cran()
	
	# get example config files
	testPathBase <- normalizePath(path = "../files")
	indexPath <- file.path(testPathBase, "index.Rmd")
			
	testDir <- tempfile("report")
	dir.create(testDir)
	
	file.copy(from = indexPath, to = testDir)
      
	# file 1
	configFile1 <- file.path(testDir, "configFile1.yml")
	write_yaml(x = list(template = "templateWithError.Rmd"), file = configFile1)
	
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
		expect_error(
			# progress messages during execution
			expect_message(
				render_clinDataReviewReport(
					configDir = testDir,
					outputDir = testDir,
					intermediateDir = testDir,
					inputDir = testDir,
					quiet = TRUE # suppress printing of pandoc cmd line   
				)
			)
		),
		"Rendering of the .+ report for config file: .+ failed, a report with only the section title is created."
	)
      
})

test_that("A warning is generated if the template name is missing in a config file", {
			
	skip_on_cran()
	
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
		# progress messages during execution
		expect_message(	
			res <- render_clinDataReviewReport(
				configDir = testDir,
				outputDir = testDir,
				intermediateDir = testDir,
				inputDir = testDir,
				quiet = TRUE # suppress printing of pandoc cmd line  
			)
		),
		"Template missing for config file: .+."
	)
	expect_type(res, "character")
	expect_match(object = res, regexp = "introduction")      
      
})

test_that("A warning is generated if the config parameters don't comply to the template specifications", {
    
	skip_on_cran()
	
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
		# progress messages during execution
		expect_message(
			res <- render_clinDataReviewReport(
				configDir = testDir,
				outputDir = testDir,
				intermediateDir = testDir,
				inputDir = testDir,
				quiet = TRUE # suppress printing of pandoc cmd line     
			)
		),
		"The report for the config file: .+ is not created because the check of the parameters failed"
	)
	expect_type(res, "character")
	expect_match(object = res, regexp = "introduction")      

})

test_that("A warning is generated if a template is not available in the specified package", {
      
	skip_on_cran()
	
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
		# progress messages during execution
		expect_message(	
			res <- render_clinDataReviewReport(
				configDir = testDir,
				outputDir = testDir,
				intermediateDir = testDir,
				inputDir = testDir ,
				quiet = TRUE # suppress printing of pandoc cmd line     
			)
		),
		"Template file: .+ not available in.*myPackage.+ package."
	)
	expect_type(res, "character")
	expect_match(object = res, regexp = "introduction")
      
})

test_that("A warning is generated if no config parameters are available", {
      
	skip_on_cran()
	
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
	
	expect_error(          
		expect_warning(
			# progress messages during execution
			expect_message(	
				res <- render_clinDataReviewReport(
					configDir = testDir,
					outputDir = testDir,
					intermediateDir = testDir,
					inputDir = testDir,
					quiet = TRUE # suppress printing of pandoc cmd line     
				)
			),
			"No config parameter available, input parameters for the report are not checked."
		)
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
			# progress messages during execution
			expect_message(	
				convertMdToHtml(
					configDir = testDir,
					outputDir = testDir,
					intermediateDir = testDir,
					inputDir = testDir,
					mdFiles = NULL,
					quiet = TRUE # suppress printing of pandoc cmd line  
				)
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
