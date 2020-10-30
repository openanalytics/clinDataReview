library(yaml)

tmpdir <- tempdir()

## File 1
#configFile1 <- tempfile(pattern = "config-", fileext = ".yml", tmpdir = tmpdir)
#write_yaml(
#    list(
#        reportTitle = "Title One",
#        reportTitleLevel = 2
#    ),
#    configFile1 
#)
#
### File 2
#configFile2 <- tempfile(pattern = "config-", fileext = ".yml", tmpdir = tmpdir)
#write_yaml(
#    list(
#        reportTitle = "Title Two",
#        reportTitleLevel = 2
#    ),
#    configFile2 
#)
#
### General config file
#file.create("config.yaml")
#configFileGeneral <- paste0(tmpdir, "/config.yml") 
#write_yaml(
#    list(
#        study = "Study name",
#        config = list(basename(configFile1), basename(configFile2))
#    ),
#    configFileGeneral
#)
#configFiles <- c(configFileGeneral, configFile1, configFile2)
#configFiles <- basename(configFiles)
#testPathBase <- file.path(
#    "~", "git",
#    "GLPGMedicalMonitoring",
#    "package",
#    "medicalMonitoring",
#    "tests", "files"
#)
testPathBase <- file.path(getwd(), "subfolderInTests")
testPathConfig <- file.path(testPathBase, "config")
testPathInterim <- file.path(testPathBase, "interim")
configFiles <- list.files(testPathConfig)
filePathConfig <- file.path(testPathConfig, configFiles)
# Other config file
otherConfigs <- configFiles[! grepl("config.yml", configFiles)]

test_that("Check uniqueness of report titles", {
      
      reportTitles <- checkReportTitles(configFiles, configDir = testPathConfig)
      expect_is(reportTitles, "character")
      expect_length(reportTitles, 1) #2
      expect_named(reportTitles, configFiles[1]) # [2 : 3]
      
      configFilesError <- c(configFiles[1], configFiles[1])
      expect_error(
          checkReportTitles(configFilesError, configDir = testPathConfig)
      )
      configFilesWarning <- c(configFiles, "config-ciao")
      expect_warning(
          checkReportTitles(configFilesWarning, configDir = testPathConfig)
      )
      
    })

test_that("Get path of Md from config file", {
      
      # ????
      mdFiles <- getMdFromConfig(configFiles, intermediateDir = testPathInterim)
      expect_is(mdFiles, "character")
      expect_length(mdFiles, length(configFiles))
      
      configFilesShortName <- gsub("config-", "", tools::file_path_sans_ext(configFiles))
      mdFilesNames <- sprintf("%s.md",
          c("index", configFilesShortName[configFilesShortName != "config"])
      )
      referenceNames <- file.path(testPathInterim, mdFilesNames)
      #expect_identical(referenceNames, mdFiles)
      
      # Different name for indexPath
      mdFilesIndex <- getMdFromConfig(
          configFiles,
          indexPath = "myIndex.Rmd",
          intermediateDir = testPathInterim
      )
      expect_is(mdFilesIndex, "character")
      expect_length(mdFilesIndex, length(configFiles))
      mdFilesMyNames <- sprintf("%s.md",
          c("myIndex", configFilesShortName[configFilesShortName != "config"])
      )
      referenceNames <- file.path(testPathInterim, mdFilesMyNames)
      #expect_identical(referenceNames, mdFilesIndex)
      
      # Different intermediateDir
      mdFilesInterim <- getMdFromConfig(configFiles, intermediateDir = "myDir")
      expect_is(mdFilesInterim, "character")
      expect_length(mdFilesInterim, length(configFiles))
      referenceNames <- file.path("myDir", mdFilesNames)
      #expect_identical(referenceNames, mdFilesInterim)
      
    })


test_that("Get parameters from general config file", {
      
      expect_error(
          getParamsFromConfig("config.yml"),
          "Config directory:‘./config’doesn't exist."
      )
      
      listConfig <- getParamsFromConfig("config.yml", configDir = testPathConfig)
      configFromYaml <- read_yaml(filePathConfig[2])
      n <- length(configFromYaml)
      expect_is(listConfig, "list")
      expect_length(listConfig, n)
      expect_identical(
          names(listConfig),
          names(configFromYaml)
      ) 
     
    })

test_that("Get parameters from chapter-specific config file", {
      
      listConfig <- getParamsFromConfig(otherConfig, configDir = testPathConfig)
      configFromYamlGeneral <- read_yaml(filePathConfig[2])
      configFromYaml <- read_yaml(filePathConfig[1])
      n <- length(configFromYamlGeneral) + length(configFromYaml)
      expect_is(listConfig, "list")
      expect_length(listConfig, n)
      #expect_identical(listConfig, c(names(configFromYamlGeneral), names(configFromYaml)))
      
      
    })

test_that("Convert Md file to Html", {
      
      outputDir <- file.path(testPathBase, "report")
      
      # Create md files
      mdFiles <- list.files(pattern = "md", file.path(testPathInterim))
      filePathMd <- file.path(testPathInterim, mdFiles)
      
      # Create rds files
      rdsFiles <- list.files(pattern = "rds", file.path(testPathInterim))
      
      htmlOutput <- convertMdToHtml(
          outputDir = outputDir,
          intermediateDir = testPathInterim,
          configDir = testPathConfig, 
          mdFiles = filePathMd,
          indexPath = "index.Rmd"
      )
      expect_is(htmlOutput, "character")
      #expect_true(grepl(outputDir, htmlOutput))
      
    })


test_that("Check template name in config", {
      
      # No template available
      #expect_warning(checkTemplatesName(configFiles, testPathBase))
      
#      configFileTemplate <- tempfile(pattern = "config-", fileext = ".yml", tmpdir = tmpdir)
#      write_yaml(
#          list(
#              reportTitle = "Title",
#              reportTitleLevel = 2,
#              template = "divisionTemplate",
#              templatePackage = "custom"
#          ),
#          configFileTemplate 
#      )
#      configFileTemplate <- basename(configFileTemplate)  
#      
#      checkedConfig <- checkTemplatesName(configFileTemplate, configDir = tmpdir)
#      expect_is(checkedConfig, "character")
#      expect_identical(configFileTemplate, checkedConfig)
#      
#      # Mispecification of template
#      configFileTemplate2 <- tempfile(pattern = "config-", fileext = ".yml", tmpdir = tmpdir)
#      write_yaml(
#          list(
#              reportTitle = "Title",
#              reportTitleLevel = 2,
#              template = "divisionTemplate",
#              templatePackage = "medicalMonitoring"
#          ),
#          configFileTemplate2
#      )
#      configFileTemplate2 <- basename(configFileTemplate2)     
#      
#      configFileTemplates <- c(configFileTemplate, configFileTemplate2)
#      expect_warning(
#          checkTemplatesName(configFileTemplates, configDir = tmpdir)
#      )
#      output <- checkTemplatesName(configFileTemplates, configDir = tmpdir)
#      expect_length(output, 0)
      
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
      
      mdFile <- exportSessionInfoToMd(sessionInfos, intermediateDir = testPathInterim)
      expect_is(mdFile, "character")
      expect_identical(mdFile, file.path(testPathInterim, "sessionInfo.md"))
      
    })

test_that("Test error of not available config.yml in 'getParamsFromConfig'", {
      
      # Remove general config file
#      file.remove(filePath[1])
#      expect_warning(getParamsFromConfig(otherConfigs, configDir = testPathConfig))
      
      
    })
