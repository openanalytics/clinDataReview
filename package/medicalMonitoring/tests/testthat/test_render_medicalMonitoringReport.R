library(yaml)

tmpdir <- tempdir()

testPathBase <- normalizePath(path = "../files")
testPathConfig <- file.path(testPathBase, "config")
testPathInterim <- file.path(testPathBase, "interim")
outputDir <- file.path(testPathBase, "report")
configFiles <- list.files(testPathConfig)
filePathConfig <- file.path(testPathConfig, configFiles)
# Other config file
otherConfigs <- configFiles[! grepl("config.yml", configFiles)]
filePathOtherConfigs <- file.path(testPathConfig, otherConfigs)
filePathGeneralConfig <- setdiff(filePathConfig, filePathOtherConfigs)

test_that("Check extraction of report titles", {
      
      reportTitles <- checkReportTitles(configFiles, configDir = testPathConfig)
      expect_is(reportTitles, "character")
      expect_length(reportTitles, length(otherConfigs))
      expect_named(reportTitles, otherConfigs)
      
    })

test_that("Check uniqueness of report titles", {
      
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

test_that("Get path of Md from config file - default settings", {
      
      mdFiles <- getMdFromConfig(configFiles, intermediateDir = testPathInterim)
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

test_that("Get path of Md from config file - not default settings", {
      
      # Different name for indexPath
      mdFilesIndex <- getMdFromConfig(
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
      
      # Different intermediateDir
      mdFilesInterim <- getMdFromConfig(configFiles, intermediateDir = "myDir")
      expect_is(mdFilesInterim, "character")
      expect_length(mdFilesInterim, length(configFiles))
      mdNames <- file.path("myDir", c(
              gsub("config-(.+).yml", "\\1.md", otherConfigs),
              "index.md"
          ))
      expect_identical(mdNames, mdFilesInterim)
      
    })


test_that("Get parameters from general config file", {
      
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

test_that("Get parameters from chapter-specific config file", {
      
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

test_that("Test errors of 'getParamsFromConfig'", {
      
      expect_error(
          getParamsFromConfig("config.yml"),
          "Config directory: .+ doesn't exist."
      )
      
      expect_error(
          getParamsFromConfig(otherConfigs, configDir = tmpdir),
          "File .+ cannot be found."
      )
      
    })

test_that("Test when general config file is not available", {
      
      configFileTemp <- tempfile(pattern = "config-", fileext = ".yml", tmpdir = tmpdir)
      write_yaml(list(), configFileTemp)
      
      expect_warning(
          getParamsFromConfig(basename(configFileTemp) , configDir = tmpdir),
          "General config file: .+ not available"
      )
      output <- getParamsFromConfig(basename(configFileTemp) , configDir = tmpdir)
      expect_type(output, "list")
      
    })

test_that("Convert Md file to Html", {
      
      filePathSessionInfo <- file.path(testPathInterim, "sessionInfo.md")
      if(file.exists(filePathSessionInfo)) file.remove(filePathSessionInfo)
      
      htmlOutput <- convertMdToHtml(
          outputDir = outputDir,
          intermediateDir = testPathInterim,
          configDir = testPathConfig, 
          mdFiles = NULL,
          indexPath = "index.Rmd"
      )
      expect_is(htmlOutput, "character")
      expect_true(grepl(outputDir, htmlOutput))
      
      if(file.exists(filePathSessionInfo)) file.remove(filePathSessionInfo)
      
      # Md files
      mdFiles <- list.files(pattern = "md", file.path(testPathInterim))
      filePathMd <- file.path(testPathInterim, mdFiles)
      
      htmlOutput <- convertMdToHtml(
          outputDir = outputDir,
          intermediateDir = testPathInterim,
          configDir = testPathConfig, 
          mdFiles = filePathMd,
          indexPath = "index.Rmd"
      )
      expect_is(htmlOutput, "character")
      expect_true(grepl(outputDir, htmlOutput))
      
      if(file.exists(filePathSessionInfo)) file.remove(filePathSessionInfo)
      
    })

test_that("Check template name in config", {
      
      checkedConfig <- checkTemplatesName(configFiles, configDir = testPathConfig)
      expect_is(checkedConfig, "character")
      expect_identical(configFiles, checkedConfig)
      
    })

test_that("Check template name for config file without template specification", {
      
      configFileTemplateGeneral <- file.path(tmpdir, "config.yml")
      file.create(configFileTemplateGeneral)
      write_yaml(list(), configFileTemplateGeneral)
      
      configFileTemplate <- tempfile(pattern = "config-", fileext = ".yml", tmpdir = tmpdir)
      write_yaml(
          list(reportTitle = "Title", reportTitleLevel = 2),
          configFileTemplate 
      )
      configFileTemplate <- basename(configFileTemplate)  
      
      expect_warning(
          checkTemplatesName(configFileTemplate, tmpdir),
          "Import of parameters from config file .+ failed with error:"
      )
      
    })

test_that("Check template name for config files with same template package", {
      
      configFileTemplateGeneral <- file.path(tmpdir, "config.yml")
      file.create(configFileTemplateGeneral)
      write_yaml(list(), configFileTemplateGeneral)
      
      configFileTemplate <- tempfile(pattern = "config-", fileext = ".yml", tmpdir = tmpdir)
      write_yaml(
          list(reportTitle = "Title", reportTitleLevel = 2,
              template = "divisionTemplate", templatePackage = "custom"
          ),
          configFileTemplate 
      )
      configFileTemplate <- basename(configFileTemplate)  
      
      configFileTemplateBis <- tempfile(pattern = "config-", fileext = ".yml", tmpdir = tmpdir)
      write_yaml(
          list(reportTitle = "Title", reportTitleLevel = 2,
              template = "divisionTemplate", templatePackage = "medicalMonitoring"
          ),
          configFileTemplateBis 
      )
      configFileTemplateBis <- basename(configFileTemplateBis)  
      
      configFileTemplates <- c(configFileTemplate, configFileTemplateBis)
      expect_warning(
          checkTemplatesName(configFileTemplates, configDir = tmpdir),
          "The following config file[(]s[)] are ignored, because the same template name is used"
      )
      output <- checkTemplatesName(configFileTemplates, configDir = tmpdir)
      expect_length(output, 0)
      
    })

test_that("Merge of session infos", {
      
      sessionInfos <- list(sessionInfo(), sessionInfo())
      sessionInfo <- do.call(merge.sessionInfo, sessionInfos)
      expect_is(sessionInfo, "sessionInfo")
      expect_is(sessionInfo, "list")
      
    })

test_that("Export of session infos", {
      
      expect_null(exportSessionInfoToMd(sessionInfos = NULL))
      
      sessionInfos <- list(sessionInfo(), sessionInfo())
      
      mdFile <-exportSessionInfoToMd(sessionInfos, intermediateDir = testPathInterim)
      expect_is(mdFile, "character")
      expect_identical(mdFile, file.path(testPathInterim, "sessionInfo.md"))
      file.remove(file.path(testPathInterim, "sessionInfo.md"))
      
    })

test_that("Test render medical monitoring report", {
      
      output <- render_medicalMonitoringReport(
          configFiles = configFiles,
          configDir = testPathConfig,
          outputDir = outputDir,
          indexPath = file.path(testPathBase, "index.Rmd"),
          intermediateDir = testPathInterim
      )
      expect_type(output, "character")
      expect_true(
          grepl("introduction", output)
      )
      htmlFiles <- list.files(pattern = "html", outputDir)
      expect_true(
          any(grepl("1-introduction", htmlFiles))
      )
      
      if(file.exists(list.files(pattern = ".md", getwd(), full.names = TRUE))) {
        file.remove(list.files(pattern = ".md", getwd(), full.names = TRUE))
      }
      if(file.exists(list.files(pattern = "[.]md", testPathBase, full.names = TRUE))) {
        file.remove(list.files(pattern = "[.]md", testPathBase, full.names = TRUE))
      }
      if(file.exists(list.files(pattern = "[.]css", testPathBase, full.names = TRUE))) {
        file.remove(list.files(pattern = "[.]css", testPathBase, full.names = TRUE))
      }
      if(file.exists(list.files(pattern = "sessionInfo.md", testPathInterim, full.names = TRUE))) {
        file.remove(list.files(pattern = "sessionInfo.md", testPathInterim, full.names = TRUE))
      }
      
      
    })

test_that("Test render medical monitoring report with log file", {
      
      logFile <- file.path(testPathBase, "log.txt")
      
      output <- render_medicalMonitoringReport(
          configFiles = configFiles,
          configDir = testPathConfig,
          outputDir = outputDir,
          indexPath = file.path(testPathBase, "index.Rmd"),
          intermediateDir = testPathInterim,
          logFile = logFile
      )
      expect_type(output, "character")
      expect_true(
          grepl("introduction", output)
      )
      expect_true(file.exists(logFile))
      
      if(file.exists(list.files(pattern = ".md", getwd(), full.names = TRUE))) {
        file.remove(list.files(pattern = ".md", getwd(), full.names = TRUE))
      }
      if(file.exists(list.files(pattern = "[.]md", testPathBase, full.names = TRUE))) {
        file.remove(list.files(pattern = "[.]md", testPathBase, full.names = TRUE))
      }
      if(file.exists(list.files(pattern = "[.]css", testPathBase, full.names = TRUE))) {
        file.remove(list.files(pattern = "[.]css", testPathBase, full.names = TRUE))
      }
      if(file.exists(list.files(pattern = "sessionInfo.md", testPathInterim, full.names = TRUE))) {
        file.remove(list.files(pattern = "sessionInfo.md", testPathInterim, full.names = TRUE))
      }
      if(file.exists(logFile)) file.remove(logFile)
      
    })

test_that("Test render medical monitoring report for all config files", {
      
      output <- render_medicalMonitoringReport(
          configFiles = NULL,
          configDir = testPathConfig,
          outputDir = outputDir,
          indexPath = file.path(testPathBase, "index.Rmd"),
          intermediateDir = testPathInterim
      )
      expect_type(output, "character")
      expect_true(
          grepl("introduction", output)
      )
      htmlFiles <- list.files(pattern = "html", outputDir)
      sectionName <- checkReportTitles(configFiles, configDir = testPathConfig)
      sectionName <- gsub(" ", "-", sectionName)
      expect_true(
          any(grepl(sectionName, htmlFiles, ignore.case = TRUE))
      )      
      
      if(file.exists(list.files(pattern = ".md", getwd(), full.names = TRUE))) {
        file.remove(list.files(pattern = ".md", getwd(), full.names = TRUE))
      }
      if(file.exists(list.files(pattern = "[.]md", testPathBase, full.names = TRUE))) {
        file.remove(list.files(pattern = "[.]md", testPathBase, full.names = TRUE))
      }
      if(file.exists(list.files(pattern = "[.]css", testPathBase, full.names = TRUE))) {
        file.remove(list.files(pattern = "[.]css", testPathBase, full.names = TRUE))
      }
      if(file.exists(list.files(pattern = "sessionInfo.md", testPathInterim, full.names = TRUE))) {
        file.remove(list.files(pattern = "sessionInfo.md", testPathInterim, full.names = TRUE))
      }
      
    })

test_that("Test warning of not existance of config file for 'render_medicalMonitoringReport'", {
      
      tmpFolder <- tempfile()
      dir.create(tmpFolder)
      
      ############
      ## File 1 ##
      configFile1 <- file.path(tmpFolder, "configFile1.yml")
      write_yaml(
          list(
              template = "divisionTemplate.Rmd",
              templatePackage = "medicalMonitoring",
              reportTitle = "Adverse events",
              reportTitleLevel = 1          
          ),
          configFile1 
      )
      
      #########################
      ## General config file ##
      configFileGeneral <- file.path(tmpFolder, "config.yml") 
      write_yaml(
          list(
              study = "Study name",
              pathDataFolder = "path/to/data",
              config = list(
                  basename(configFile1),
                  "configFile2"
              )
          ),
          configFileGeneral
      )
      configFiles <- c(configFileGeneral, configFile1, "configFile2")
      configFiles <- basename(configFiles)
      
      ################
      ## Index file ##
      idxFile <- file.path(testPathBase, "index.Rmd")
      
      ################
      ## Extra dirs ##
      extraTmpDirs <- c(file.path(tmpFolder, "tables"), file.path(tmpFolder, "figures"))
      
      expect_warning(
          output <- render_medicalMonitoringReport(
              configFiles = configFiles,
              configDir = tmpFolder,
              outputDir = tmpFolder,
              intermediateDir = tmpFolder,
              indexPath = idxFile,
              extraDirs = extraTmpDirs
          )
      )
      expect_type(output, "character")
      expect_true(
          grepl("introduction", output)
      ) 
      
    })

test_that("Test warning of template Rmd already available for 'render_medicalMonitoringReport'", {
      
      tmpFolder <- tempfile()
      dir.create(tmpFolder)
      
      ############
      ## File 1 ##
      configFile1 <- file.path(tmpFolder, "configFile1.yml")
      write_yaml(
          list(
              template = "divisionTemplate.Rmd",
              templatePackage = "medicalMonitoring",
              reportTitle = "Adverse events",
              reportTitleLevel = 1          
          ),
          configFile1 
      )
      
      #########################
      ## General config file ##
      configFileGeneral <- file.path(tmpFolder, "config.yml") 
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
      
      ################
      ## Index file ##
      idxFile <- file.path(testPathBase, "index.Rmd")
      
      ################
      ## Extra dirs ##
      extraTmpDirs <- c(file.path(tmpFolder, "tables"), file.path(tmpFolder, "figures"))      
      dir.create(extraTmpDirs[1]); dir.create(extraTmpDirs[2])
      
      firstRun <- render_medicalMonitoringReport(
          configFiles = configFiles,
          configDir = tmpdir,
          outputDir = tmpdir,
          intermediateDir = tmpdir,
          indexPath = idxFile,
          extraDirs = extraTmpDirs 
      )
      
      expect_warning(
          secondRun <- render_medicalMonitoringReport(
              configFiles = configFiles,
              configDir = tmpFolder,
              outputDir = tmpFolder,
              intermediateDir = tmpFolder,
              indexPath = idxFile,
              extraDirs = extraTmpDirs       
          ),
          "Document with similar name than specified template from"
      )
      expect_type(secondRun, "character")
      expect_true(
          grepl("introduction", secondRun)
      )         
      
    })

test_that("Warning for creation of a report with only section title for 'render_medicalMonitoringReport'", {
      
      ################################
      ## Can this test be improved? ##
      
      tmpFolder <- tempfile()
      dir.create(tmpFolder)
      
      ############
      ## File 1 ##
      configFile1 <- file.path(tmpFolder, "configFile1.yml")
      write_yaml(
          list(
              template = "divisionTemplate.Rmd",
              templatePackage = "medicalMonitoring",
              reportTitle = "Adverse events",
              reportTitleLevel = 1          
          ),
          configFile1 
      )
      
      #########################
      ## General config file ##
      configFileGeneral <- file.path(tmpFolder, "config.yml") 
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
      
      ################
      ## Extra dirs ##
      extraTmpDirs <- c(file.path(tmpFolder, "tables"), file.path(tmpFolder, "figures"))      
      dir.create(extraTmpDirs[1]); dir.create(extraTmpDirs[2])
      
      expect_error(
          expect_warning(
              render_medicalMonitoringReport(
                  configFiles = configFiles,
                  configDir = tmpFolder,
                  outputDir = tmpFolder,
                  intermediateDir = tmpFolder,
                  indexPath = "index.Rmd",
                  extraDirs = extraTmpDirs       
              ),
              "Rendering of the .+ report for config file: .+ failed, a report with only the section title is created."
          )
      )      
      
    })

test_that("Warning of not available template name in config for 'render_medicalMonitoringReport'", {
      
      tmpFolder <- tempfile()
      dir.create(tmpFolder)
      
      ############
      ## File 1 ##
      configFile1 <- file.path(tmpFolder, "configFile1.yml")
      write_yaml(
          list(
              #template = "divisionTemplate.Rmd",
              #templatePackage = "medicalMonitoring",
              reportTitle = "Adverse events",
              reportTitleLevel = 1          
          ),
          configFile1 
      )
      
      #########################
      ## General config file ##
      configFileGeneral <- file.path(tmpFolder, "config.yml") 
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
      
      ################
      ## Index file ##
      idxFile <- file.path(testPathBase, "index.Rmd")
      
      ################
      ## Extra dirs ##
      extraTmpDirs <- c(file.path(tmpFolder, "tables"), file.path(tmpFolder, "figures"))      
      dir.create(extraTmpDirs[1]); dir.create(extraTmpDirs[2])
      
      expect_warning(
          res <- render_medicalMonitoringReport(
              configFiles = configFiles,
              configDir = tmpFolder,
              outputDir = tmpFolder,
              intermediateDir = tmpFolder,
              indexPath = idxFile,
              extraDirs = extraTmpDirs       
          ),
          "Template missing for config file: .+."
      )
      expect_type(res, "character")
      expect_true(
          grepl("introduction", res)
      )      
      
    })

test_that("Warning of failed check for config for 'render_medicalMonitoringReport'", {
      
      tmpFolder <- tempfile()
      dir.create(tmpFolder)
      
      ############
      ## File 1 ##
      configFile1 <- file.path(tmpFolder, "configFile1.yml")
      write_yaml(
          list(
              template = "divisionTemplate.Rmd",
              templatePackage = "medicalMonitoring" #,
          #reportTitle = "Adverse events",
          #reportTitleLevel = 1          
          ),
          configFile1 
      )
      
      #########################
      ## General config file ##
      configFileGeneral <- file.path(tmpFolder, "config.yml") 
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
      
      ################
      ## Index file ##
      idxFile <- file.path(testPathBase, "index.Rmd")
      
      ################
      ## Extra dirs ##
      extraTmpDirs <- c(file.path(tmpFolder, "tables"), file.path(tmpFolder, "figures"))      
      dir.create(extraTmpDirs[1]); dir.create(extraTmpDirs[2])
      
      expect_warning(
          res <- render_medicalMonitoringReport(
              configFiles = configFiles,
              configDir = tmpFolder,
              outputDir = tmpFolder,
              intermediateDir = tmpFolder,
              indexPath = idxFile,
              extraDirs = extraTmpDirs       
          ),
          "The report for the config file: .+ is not created because the check of the parameters failed"
      )
      expect_type(res, "character")
      expect_true(
          grepl("introduction", res)
      )      
      
      
    })

test_that("Warning of template from not available package for 'render_medicalMonitoringReport'", {
      
      tmpFolder <- tempfile()
      dir.create(tmpFolder)
      
      ############
      ## File 1 ##
      configFile1 <- file.path(tmpFolder, "configFile1.yml")
      write_yaml(
          list(
              template = "divisionTemplate.Rmd",
              templatePackage = "myPackage" #,
          #reportTitle = "Adverse events",
          #reportTitleLevel = 1          
          ),
          configFile1 
      )
      
      #########################
      ## General config file ##
      configFileGeneral <- file.path(tmpFolder, "config.yml") 
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
      
      ################
      ## Index file ##
      idxFile <- file.path(testPathBase, "index.Rmd")
      
      ################
      ## Extra dirs ##
      extraTmpDirs <- c(file.path(tmpFolder, "tables"), file.path(tmpFolder, "figures"))      
      dir.create(extraTmpDirs[1]); dir.create(extraTmpDirs[2])
      
      expect_warning(
          res <- render_medicalMonitoringReport(
              configFiles = configFiles,
              configDir = tmpFolder,
              outputDir = tmpFolder,
              intermediateDir = tmpFolder,
              indexPath = idxFile,
              extraDirs = extraTmpDirs       
          ),
          "Template file: .+ not available in the .+ package."
      )
      expect_type(res, "character")
      expect_true(
          grepl("introduction", res)
      )      
      
    })

test_that("Warning of template in another package for 'render_medicalMonitoringReport'", {
      
      tmpFolder <- tempfile()
      dir.create(tmpFolder)
      
      ############
      ## File 1 ##
      configFile1 <- file.path(tmpFolder, "configFile1.yml")
      write_yaml(
          list(
              template = "subjectProfile.Rnw",
              templatePackage = "patientProfilesVis" #,
          #reportTitle = "Adverse events",
          #reportTitleLevel = 1          
          ),
          configFile1 
      )
      
      #########################
      ## General config file ##
      configFileGeneral <- file.path(tmpFolder, "config.yml") 
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
      
      ################
      ## Index file ##
      idxFile <- file.path(testPathBase, "index.Rmd")
      
      ################
      ## Extra dirs ##
      extraTmpDirs <- c(file.path(tmpFolder, "tables"), file.path(tmpFolder, "figures"))      
      dir.create(extraTmpDirs[1]); dir.create(extraTmpDirs[2])
      
      expect_error(          
          expect_warning(
              res <- render_medicalMonitoringReport(
                  configFiles = configFiles,
                  configDir = tmpFolder,
                  outputDir = tmpFolder,
                  intermediateDir = tmpFolder,
                  indexPath = idxFile,
                  extraDirs = extraTmpDirs       
              ),
              "No config parameter available, input parameters for the report are not checked."
          )
      )
      
    })

test_that("Warning for ignore missing Md files", {
      
      tmpFolder <- tempfile()
      dir.create(tmpFolder)
      
      ############
      ## File 1 ##
      configFile1 <- file.path(tmpFolder, "configFile1.yml")
      write_yaml(
          list(
              template = "divisionTemplate.Rmd",
              templatePackage = "myPackage" #,
          #reportTitle = "Adverse events",
          #reportTitleLevel = 1          
          ),
          configFile1 
      )
      
      #########################
      ## General config file ##
      configFileGeneral <- file.path(tmpFolder, "config.yml") 
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
      
      ################
      ## Index file ##
      idxFile <- file.path(testPathBase, "index.Rmd")
      
      ################
      ## Extra dirs ##
      extraTmpDirs <- c(file.path(tmpFolder, "tables"), file.path(tmpFolder, "figures"))      
      dir.create(extraTmpDirs[1]); dir.create(extraTmpDirs[2])
      
      expect_error(
          expect_warning(
              convertMdToHtml(
                  outputDir = tmpFolder,
                  intermediateDir = tmpFolder,
                  configDir = tmpFolder,
                  mdFiles = NULL,
                  indexPath = idxFile
              ),
              "Markdown file(s): .+ are missing, these files are ignored."
          )
      )
      
    })
	
test_that("parameters with R code and lazy-loading are imported from a config file", {
				
	configFileTemp <- tempfile(pattern = "config-", fileext = ".yml", tmpdir = tmpdir)
	write_yaml(list(param = structure("nrow(dataI)", tag = "!expr-lazy")), configFileTemp)
				
	params <- getParamsFromConfig(basename(configFileTemp), configDir = tmpdir)
	expect_is(params, "list")
	expect_is(params[["param"]], c("expr-lazy", "character"))
	expect_equal(as.character(params[["param"]]), "nrow(dataI)")
				
})

test_that("parameters with R code and lazy-loading are evaluated", {
	
	params <- list(nrowData = structure("nrow(customData)", class = "expr-lazy"))
	
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
