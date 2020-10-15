library(yaml)

tmpdir <- tempdir()

## File 1
configFile1 <- tempfile(pattern = "config-", fileext = ".yml", tmpdir = tmpdir)
write_yaml(
    list(
        reportTitle = "Title One",
        reportTitleLevel = 2
    ),
    configFile1 
)

## File 2
configFile2 <- tempfile(pattern = "config-", fileext = ".yml", tmpdir = tmpdir)
write_yaml(
    list(
        reportTitle = "Title Two",
        reportTitleLevel = 2
    ),
    configFile2 
)

## General config file
file.create("config.yaml")
configFileGeneral <- paste0(tmpdir, "/config.yml") 
write_yaml(
    list(
        study = "Study name",
        config = list(basename(configFile1), basename(configFile2))
    ),
    configFileGeneral
)
configFiles <- c(configFileGeneral, configFile1, configFile2)
configFiles <- basename(configFiles)
filePath <- sprintf("%s/%s", tmpdir, configFiles)

test_that("Check uniqueness of report titles", {
      
      reportTitles <- checkReportTitles(configFiles, configDir = tmpdir)
      expect_is(reportTitles, "character")
      expect_length(reportTitles, 2)
      expect_named(reportTitles, configFiles[2 : 3])
      
      configFilesError <- c(configFiles[2], configFiles[2])
      expect_error(
          checkReportTitles(configFilesError, configDir = tmpdir)
      )
      configFilesWarning <- c(configFiles, "config-ciao")
      expect_warning(
          checkReportTitles(configFilesWarning, configDir = tmpdir)
      )
      
    })

test_that("Get path of Md from config file", {
      
      mdFiles <- getMdFromConfig(configFiles)
      expect_is(mdFiles, "character")
      expect_length(mdFiles, length(configFiles))
      
      configFilesShortName <- gsub("config-", "", tools::file_path_sans_ext(configFiles))
      referenceNames <- sprintf("./interim/%s.md", c("index", configFilesShortName[configFilesShortName != "config"]))
      expect_identical(referenceNames, mdFiles)
      
      # Different name for indexPath
      mdFilesIndex <- getMdFromConfig(configFiles, indexPath = "myIndex.Rmd")
      expect_is(mdFilesIndex, "character")
      expect_length(mdFilesIndex, length(configFiles))
      referenceNames <- sprintf("./interim/%s.md", c("myIndex", configFilesShortName[configFilesShortName != "config"]))
      expect_identical(referenceNames, mdFilesIndex)
      
      # Different intermediateDir
      mdFilesInterim <-getMdFromConfig(configFiles, intermediateDir = "myDir")
      expect_is(mdFilesInterim, "character")
      expect_length(mdFilesInterim, length(configFiles))
      referenceNames <- sprintf("myDir/%s.md", c("index", configFilesShortName[configFilesShortName != "config"]))
      expect_identical(referenceNames, mdFilesInterim)
      
    })


test_that("Get parameters from config file", {
      
      expect_error(getParamsFromConfig(configFiles[1]))
      
      # General config file
      listConfig <- getParamsFromConfig(configFiles[1], configDir = tmpdir)
      n <- length(read_yaml(filePath[1]))
      expect_is(listConfig, "list")
      expect_length(listConfig, n)
      expect_identical(names(listConfig), c("study", "config"))
      
      # Other config file
      listConfig <- getParamsFromConfig(configFiles[2], configDir = tmpdir)
      n <- length(read_yaml(filePath[2])) + length(read_yaml(filePath[1]))
      expect_is(listConfig, "list")
      expect_length(listConfig, n)
      expect_identical(listConfig, c(read_yaml(filePath[1]), read_yaml(filePath[2])))
      
    })

test_that("Convert Md file to Html", {
      
      outputDir <- sprintf("%s/report", tmpdir)
      intermediateDir <- sprintf("%s/interim", tmpdir)
      configDir <- tmpdir
      dir.create(outputDir)
      dir.create(intermediateDir)
      
      # Create md files
      mdFiles <- gsub("config-(.+)", "\\1.md", basename(tools::file_path_sans_ext(configFiles)))
      mdFiles <- mdFiles[-1]      
      file.create(sprintf("%s/%s", intermediateDir, mdFiles))
      
      # Create rds files
      rdsFiles <- sprintf("%s.rds", tools::file_path_sans_ext(mdFiles))
      file.create(sprintf("%s/%s", intermediateDir, rdsFiles))
      sessList <- list(knitMeta = sessionInfo())
      saveRDS(sessList, sprintf("%s/%s", intermediateDir, rdsFiles[1]))
      saveRDS(sessList, sprintf("%s/%s", intermediateDir, rdsFiles[2]))
      
#      expect_warning(
#          convertMdToHtml(
#              outputDir = outputDir,
#              intermediateDir = intermediateDir,
#              configDir = configDir, 
#              mdFiles = sprintf("%s/%s", intermediateDir, mdFiles),
#              indexPath = "index.Rmd"
#          )
#      )
      htmlOutput <- convertMdToHtml(
          outputDir = outputDir,
          intermediateDir = intermediateDir,
          configDir = configDir, 
          mdFiles = sprintf("%s/%s", intermediateDir, mdFiles),
          indexPath = "index.Rmd"
      )
      expect_is(htmlOutput, "character")
      expect_true(grepl(outputDir, htmlOutput))
            
    })


test_that("Check template name in config", {
      
      # No template available
      expect_warning(checkTemplatesName(configFiles, tmpdir))
      
      configFileTemplate <- tempfile(pattern = "config-", fileext = ".yml", tmpdir = tmpdir)
      write_yaml(
          list(
              reportTitle = "Title",
              reportTitleLevel = 2,
              template = "divisionTemplate",
              templatePackage = "custom"
          ),
          configFileTemplate 
      )
      configFileTemplate <- basename(configFileTemplate)  
      
      checkedConfig <- checkTemplatesName(configFileTemplate, configDir = tmpdir)
      expect_is(checkedConfig, "character")
      expect_identical(configFileTemplate, checkedConfig)
      
      # Mispecification of template
      configFileTemplate2 <- tempfile(pattern = "config-", fileext = ".yml", tmpdir = tmpdir)
      write_yaml(
          list(
              reportTitle = "Title",
              reportTitleLevel = 2,
              template = "divisionTemplate",
              templatePackage = "medicalMonitoring"
          ),
          configFileTemplate2
      )
      configFileTemplate2 <- basename(configFileTemplate2)     
      
      configFileTemplates <- c(configFileTemplate, configFileTemplate2)
      expect_warning(
          checkTemplatesName(configFileTemplates, configDir = tmpdir)
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
      
      mdFile <- exportSessionInfoToMd(sessionInfos, intermediateDir = tmpdir)
      expect_is(mdFile, "character")
      expect_identical(mdFile, sprintf("%s/sessionInfo.md", tmpdir))
      
    })

test_that("Test error of not available config.yml in 'getParamsFromConfig'", {
      
      # Remove general config file
      file.remove(filePath[1])
      expect_warning(getParamsFromConfig(configFiles[2], configDir = tmpdir))
      
      
    })


#file.remove(configFiles)