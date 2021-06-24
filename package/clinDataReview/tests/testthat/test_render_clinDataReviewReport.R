context("Test the 'render_clinDataReviewReport'")

library(yaml)

testPathBase <- normalizePath(path = "../files")
testPathConfig <- file.path(testPathBase, "config")
testPathInterim <- file.path(testPathBase, "interim")
indexPath <- file.path(testPathBase, "index.Rmd")

configFiles <- list.files(testPathConfig)
filePathConfig <- file.path(testPathConfig, configFiles)
# Other config file
otherConfigs <- configFiles[! grepl("config.yml", configFiles)]
filePathOtherConfigs <- file.path(testPathConfig, otherConfigs)
filePathGeneralConfig <- setdiff(filePathConfig, filePathOtherConfigs)

test_that("Report titles are extracted for each config file", {
      
	reportTitles <- checkReportTitles(configFiles, configDir = testPathConfig)
	expect_is(reportTitles, "character")
	expect_length(reportTitles, length(otherConfigs))
	expect_named(reportTitles, otherConfigs)
      
})

test_that("Report titles are unique", {
      
      configFilesError <- c(configFiles[1], configFiles[1])
      expect_error(
          checkReportTitles(configFilesError, configDir = testPathConfig),
          "The title .+ is duplicated."
      )
      configFilesWarning <- c(configFiles, "config-ciao")
      expect_warning(
          checkReportTitles(configFilesWarning, configDir = testPathConfig),
          "Please check the spelling is correct"
      )
      
    })

test_that("Paths of markdown files are extracted correctly from the config files", {
      
      mdFiles <- clinDataReview:::getMdFromConfig(configFiles, intermediateDir = testPathInterim)
      expect_is(mdFiles, "character")
      expect_length(mdFiles, length(configFiles))
      
      mdNames <- file.path(
          testPathInterim,
          c(
              gsub("config-(.+).yml", "\\1.md", otherConfigs),
              "index.md"
          ))
      expect_identical(mdNames, mdFiles)
      
    })

test_that("Names of markdown files are extracted correctly from the config files when custom index file is specified", {
      
      # Different name for indexPath
      mdFilesIndex <- clinDataReview:::getMdFromConfig(
          configFiles,
          indexPath = "myIndex.Rmd",
          intermediateDir = testPathInterim
      )
      expect_is(mdFilesIndex, "character")
      expect_length(mdFilesIndex, length(configFiles))
      mdNames <- file.path(
          testPathInterim,
          c(
              gsub("config-(.+).yml", "\\1.md", otherConfigs),
              "myIndex.md"
          ))
      expect_identical(mdNames, mdFilesIndex)
      
})

test_that("Names of markdown files are extracted correctly from the config files when custom intermediate directory is specified", {
			 
      # Different intermediateDir
      mdFilesInterim <- clinDataReview:::getMdFromConfig(configFiles, intermediateDir = "myDir")
      expect_is(mdFilesInterim, "character")
      expect_length(mdFilesInterim, length(configFiles))
      mdNames <- file.path("myDir", c(
              gsub("config-(.+).yml", "\\1.md", otherConfigs),
              "index.md"
          ))
      expect_identical(mdNames, mdFilesInterim)
      
    })


test_that("Parameters are extracted successfully from a general config file", {
      
      listConfig <- getParamsFromConfig("config.yml", configDir = testPathConfig)
      configFromYaml <- read_yaml(filePathGeneralConfig)
      n <- length(configFromYaml)
      expect_is(listConfig, "list")
      expect_length(listConfig, n)
      expect_identical(
          names(listConfig),
          names(configFromYaml)
      ) 
      
    })

test_that("Parameters are extracted successfully from a chapter-specific config file", {
      
      listConfig <- getParamsFromConfig(otherConfigs, configDir = testPathConfig)
      configFromYamlGeneral <- read_yaml(filePathGeneralConfig)
      configFromYaml <- read_yaml(filePathOtherConfigs)
      n <- length(configFromYamlGeneral) + length(configFromYaml)
      expect_is(listConfig, "list")
      expect_length(listConfig, n)
      expect_identical(
          names(listConfig),
          c(names(configFromYamlGeneral), names(configFromYaml))
      )
      
    })

test_that("An error is generated if the config directory is not available", {
      
	configDir <- tempfile(pattern = "config")		
			
	expect_error(
		getParamsFromConfig(configFile = "config.yml", configDir = configDir),
		"Config directory: .+ doesn't exist."
	)
	
})

test_that("An error is generated if the config directory is not available", {
			
	configDir <- tempfile(pattern = "config")
	dir.create(configDir)
	
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

test_that("The creation of html file from markdown files is successful", {
      
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
		indexPath = "index.Rmd"
	)
	expect_is(htmlOutput, "character")
	expect_true(file.exists(htmlOutput))
	expect_equal(normalizePath(dirname(htmlOutput)), normalizePath(testDir))
      
	# Md files
	filePathMd <- list.files(pattern = "md", interimDir, full.names = TRUE)
      
	htmlOutput <- convertMdToHtml(
		outputDir = testDir,
		intermediateDir = interimDir,
		configDir = testPathConfig, 
		mdFiles = filePathMd,
		indexPath = "index.Rmd"
	)
	expect_is(htmlOutput, "character")
	expect_true(file.exists(htmlOutput))
	expect_equal(normalizePath(dirname(htmlOutput)), normalizePath(testDir))
      
})

test_that("The check of template name from correct config files is successful", {
      
	expect_silent(
		checkedConfig <- clinDataReview:::checkTemplatesName(
			configFiles = configFiles, configDir = testPathConfig
		)
	)
	expect_is(checkedConfig, "character")
	expect_identical(configFiles, checkedConfig)
      
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

test_that("A warning is generated if config files with the same template name in different package are used", {
      
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
	expect_is(sessionInfo, "sessionInfo")
	expect_is(sessionInfo, "list")
      
})

test_that("The export of empty session infos to Markdown returns empty output", {
      
	expect_null(clinDataReview:::exportSessionInfoToMd(sessionInfos = NULL))
	
})

test_that("The export of session infos to Markdown runs successfully", {
			
	interimDir <- tempfile("interim")		
			
	sessionInfos <- list(sessionInfo(), sessionInfo())
      
	mdFile <- clinDataReview:::exportSessionInfoToMd(
		sessionInfos = sessionInfos, 
		intermediateDir = interimDir
	)
	expect_is(mdFile, "character")
	expect_true(file.exists(mdFile))
	expect_identical(mdFile, file.path(interimDir, "sessionInfo.md"))
      
})

test_that("A clinical data report is created successfully via specification of config files", {
	
	skip_on_cran()
			
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
		outputDir = outputDir
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

test_that("A clinical data report is created successfully via config directory", {

	skip_on_cran()
			
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
		outputDir = outputDir
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
			
      testDir <- tempfile("report")
      dir.create(testDir)
	  
	  # copy index file
	  file.copy(from = indexPath, to = testDir)
      
      ############
      ## File 1 ##
      configFile1 <- file.path(testDir, "configFile1.yml")
      write_yaml(
          list(
              template = "divisionTemplate.Rmd",
              templatePackage = "clinDataReview",
              reportTitle = "Adverse events",
              reportTitleLevel = 1          
          ),
          configFile1 
      )
      
      #########################
      ## General config file ##
      configFileGeneral <- file.path(testDir, "config.yml") 
      write_yaml(
          list(
              study = "Study name",
              pathDataFolder = "path/to/data",
              config = list(
                  basename(configFile1),
                  "configFile2.yml"
              )
          ),
          configFileGeneral
      )
      configFiles <- c(configFileGeneral, configFile1, "configFile2.yml")
      configFiles <- basename(configFiles)
      
      expect_warning(
          output <- render_clinDataReviewReport(
              configDir = testDir,
              outputDir = testDir,
			  inputDir = testDir,
              intermediateDir = testDir
          ),
		  "configFile2.* cannot be found"
      )
      expect_type(output, "character")
      expect_match(output, "introduction")
      
})

test_that("A warning is generated if a template Rmd is already available", {
			
	skip_on_cran()
      
	testDir <- tempfile("report")
	dir.create(testDir)
			
	file.copy(from = indexPath, to = testDir)
      
	############
	## File 1 ##
	configFile1 <- file.path(testDir, "configFile1.yml")
	write_yaml(
		list(
			template = "divisionTemplate.Rmd",
			templatePackage = "clinDataReview",
			reportTitle = "Adverse events",
			reportTitleLevel = 1          
		),
		configFile1 
	)
      
	#########################
	## General config file ##
	configFileGeneral <- file.path(testDir, "config.yml") 
	write_yaml(
		list(
			study = "Study name",
			pathDataFolder = "path/to/data",
			config = list(
				basename(configFile1)
			)
		),
		configFileGeneral
	)
	configFiles <- c(configFileGeneral, configFile1)
	configFiles <- basename(configFiles)
	  
	firstRun <- render_clinDataReviewReport(
		configDir = testDir,
		outputDir = testDir,
		inputDir = testDir,
		intermediateDir = testDir
	)
      
	expect_warning(
		secondRun <- render_clinDataReviewReport(
			configDir = testDir,
			outputDir = testDir,
			inputDir = testDir,
			intermediateDir = testDir    
		),
		"Document with similar name than specified template from"
	)
	expect_type(secondRun, "character")
	expect_match(secondRun, regexp = "introduction")
      
})

test_that("A warning is generated if the creation of a chapter fails'", {
     
	skip_on_cran()
			
	testDir <- tempfile("report")
	dir.create(testDir)
	
	file.copy(from = indexPath, to = testDir)
      
	############
	## File 1 ##
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
      
	#########################
	## General config file ##
	
	#########################
	## General config file ##
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
			render_clinDataReviewReport(
				configDir = testDir,
				outputDir = testDir,
				intermediateDir = testDir,
				inputDir = testDir
			)
		),
		"Rendering of the .+ report for config file: .+ failed, a report with only the section title is created."
	)
      
})

test_that("A warning is generated if the template name is missing in a config file", {
			
	skip_on_cran()
			
	testDir <- tempfile("report")
	dir.create(testDir)
	
	file.copy(from = indexPath, to = testDir)
      
	############
	## File 1 ##
	configFile1 <- file.path(testDir, "configFile1.yml")
	write_yaml(
		x = list(
			reportTitle = "Adverse events",
			reportTitleLevel = 1          
		),
		file = configFile1 
	)
      
	#########################
	## General config file ##
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
			inputDir = testDir
		),
		"Template missing for config file: .+."
	)
	expect_type(res, "character")
	expect_match(object = res, regexp = "introduction")      
      
})

test_that("A warning is generated if the config parameters don't comply to the template specifications", {
    
	skip_on_cran()
			
	testDir <- tempfile("report")
	dir.create(testDir)
	
	file.copy(from = indexPath, to = testDir)
      
	############
	## File 1 ##
	configFile1 <- file.path(testDir, "configFile1.yml")
	write_yaml(
		x = list(
			template = "divisionTemplate.Rmd",
			templatePackage = "clinDataReview"
		),
		file = configFile1 
	)
      
	#########################
	## General config file ##
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
			inputDir = testDir     
		),
		"The report for the config file: .+ is not created because the check of the parameters failed"
	)
	expect_type(res, "character")
	expect_match(object = res, regexp = "introduction")      

})

test_that("A warning is generated if a template is not available in the specified package", {
      
	skip_on_cran()
			
	testDir <- tempfile("report")
	dir.create(testDir)
			
	file.copy(from = indexPath, to = testDir)
      
	############
	## File 1 ##
	configFile1 <- file.path(testDir, "configFile1.yml")
	write_yaml(
		x = list(
			template = "divisionTemplate.Rmd",
			templatePackage = "myPackage"       
		),
		file = configFile1 
	)
      
	#########################
	## General config file ##
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
			inputDir = testDir     
		),
		"Template file: .+ not available in.*myPackage.+ package."
	)
	expect_type(res, "character")
	expect_match(object = res, regexp = "introduction")
      
})

test_that("A warning is generated if no config parameters are available", {
      
	skip_on_cran()		
			
	testDir <- tempfile("report")
	dir.create(testDir)
			
	file.copy(from = indexPath, to = testDir)
      
	############
	## File 1 ##
	configFile1 <- file.path(testDir, "configFile1.yml")
	write_yaml(
		x = list(
			template = "subjectProfile.Rnw",
			templatePackage = "patientProfilesVis"
		),
		file = configFile1 
	)
      
	#########################
	## General config file ##
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
			res <- render_clinDataReviewReport(
				configDir = testDir,
				outputDir = testDir,
				intermediateDir = testDir,
				inputDir = testDir     
			),
			"No config parameter available, input parameters for the report are not checked."
		)
	)
      
})

test_that("A warning is generated if some Markdown files are missing for conversion to html", {
			
	testDir <- tempfile("report")
	dir.create(testDir)
			
	file.copy(from = indexPath, to = testDir)
      
	############
	## File 1 ##
	configFile1 <- file.path(testDir, "configFile1.yml")
	write_yaml(
		x = list(
			template = "divisionTemplate.Rmd",
			templatePackage = "myPackage"  
		),
		file = configFile1 
	)
      
	#########################
	## General config file ##
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
			convertMdToHtml(
				configDir = testDir,
				outputDir = testDir,
				intermediateDir = testDir,
				inputDir = testDir,
				mdFiles = NULL
			),
			"Markdown file(s): .+ are missing, these files are ignored."
		)
	)
      
})
	
test_that("Config parameters with R code are correctly evaluated", {
	
	configDir <- tempfile("config-r-eval")
	dir.create(configDir)
			
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
			
	configFileTemp <- tempfile(pattern = "config-", fileext = ".yml", tmpdir = configDir)
	write_yaml(list(param = structure("nrow(dataI)", tag = "!r-lazy")), configFileTemp)
				
	params <- getParamsFromConfig(
		configFile = basename(configFileTemp), 
		configDir = configDir
	)
	expect_is(params, "list")
	expect_is(params[["param"]], c("r-lazy", "character"))
	expect_equal(as.character(params[["param"]]), "nrow(dataI)")
				
})

test_that("Config parameters with R code and lazy-evaluation are evaluated when requested", {
	
	params <- list(nrowData = structure("nrow(customData)", class = "r-lazy"))
	
	# error if R object is missing
	expect_error(
		forceParams(params),
		".*customData.* not found"
	)
	
	# parameter is replaced by its evaluated version:
	customData <- iris
	expect_silent(paramEvaluated <- forceParams(params))
	expect_is(paramEvaluated, "list")
	expect_named(paramEvaluated, "nrowData")
	expect_equal(paramEvaluated$nrowData, nrow(customData))
			
})
