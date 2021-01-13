context("Test template creation")

library(yaml)
library(rmarkdown)

tmpdir <- tempdir()
testPathData <- normalizePath(path = "../dataTesting")

test_that("Creation of division template", {
      
      # Read division config from available one in tests folder
      testPathFiles <- normalizePath(path = "../files")
      testPathConfig <- file.path(testPathFiles, "config")
      configFiles <- list.files(testPathConfig)
      filePathConfig <- file.path(testPathConfig, configFiles)
      
      # Division config file
      divisionConfigs <- configFiles[! grepl("config.yml", configFiles)]
      filePathOtherConfigs <- file.path(testPathConfig, divisionConfigs)
      params <- read_yaml(filePathOtherConfigs)
      
      
      templateName <- "divisionTemplate.Rmd"
      pathTemplate <- system.file("template", templateName, package = "medicalMonitoring")      
      
      rmarkdown::render(
          pathTemplate,
          output_file = file.path(tmpdir, templateName)
      )
      expect_true(any(file.exists(templateName, tmpdir)))
      expect_true(templateName %in% list.files(tmpdir))
      detach(params)
      rm(params)
      
    })

test_that("Creation of listing template", {
      
      templateName <- "listingTemplate.Rmd"
      configFilePath <- file.path(tmpdir,
          sprintf("%sConfig.yml", tools::file_path_sans_ext(templateName))
      )
      
      paramsYaml <- list(
          pathDataFolder = testPathData,
          template = templateName,
          templatePackage = "medicalMonitoring",
          reportTitle = gsub("(.+)Template[.].+", "\\1 template", templateName),
          reportTitleLevel = 2,
          dataFileName =  list.files(testPathData, pattern = "xpt"),
          tableParams = list(
              tableVars = c("USUBJID", "EXDOSE")
          )
      )      
      write_yaml(
          paramsYaml,
          file = configFilePath
      )
      params <- read_yaml(configFilePath)
      
      pathTemplate <- system.file("template", templateName, package = "medicalMonitoring")      
      
      rmarkdown::render(
          pathTemplate,
          output_file = file.path(tmpdir, templateName)
      )
      expect_true(any(file.exists(templateName, tmpdir)))
      expect_true(templateName %in% list.files(tmpdir))
      detach(params)
      rm(params)
      
    })

test_that("Creation of counts visualization template", {
      
      templateName <- "countsVisualizationTemplate.Rmd"
      configFilePath <- file.path(tmpdir,
          sprintf("%sConfig.yml", tools::file_path_sans_ext(templateName))
      )
      
      paramsYaml <- list(
          pathDataFolder = testPathData,
          template = templateName,
          templatePackage = "medicalMonitoring",
          reportTitle = gsub("(.+)Template[.].+", "\\1 template", templateName),
          dataFileName =  list.files(testPathData, pattern = "xpt"),
          countVar = "EXDOSE"
      )      
      
      write_yaml(
          paramsYaml,
          file = configFilePath
      )
      params <- read_yaml(configFilePath)
      
      pathTemplate <- system.file("template", templateName, package = "medicalMonitoring")      
      
      rmarkdown::render(
          pathTemplate,
          output_file = file.path(tmpdir, templateName)
      )
      expect_true(any(file.exists(templateName, tmpdir)))
      expect_true(templateName %in% list.files(tmpdir))
      detach(params)
      rm(params)
      
    })

test_that("Creation of plot template", {
      
      templateName <- "plotTemplate.Rmd"
      configFilePath <- file.path(tmpdir,
          sprintf("%sConfig.yml", tools::file_path_sans_ext(templateName))
      )
      
      paramsYaml <- list(
          pathDataFolder = testPathData,
          template = templateName,
          templatePackage = "medicalMonitoring",
          reportTitle = gsub("(.+)Template[.].+", "\\1 template", templateName),
          dataFileName =  list.files(testPathData, pattern = "xpt"),
          plotFunction = "scatterplotMonitoring",
          plotParams = list(xVar = "USUBJID", yVar = "EXDOSE")
      )      
      
      write_yaml(
          paramsYaml,
          file = configFilePath
      )
      params <- read_yaml(configFilePath)
      
      pathTemplate <- system.file("template", templateName, package = "medicalMonitoring")      
      
      rmarkdown::render(
          pathTemplate,
          output_file = file.path(tmpdir, templateName)
      )
      expect_true(any(file.exists(templateName, tmpdir)))
      expect_true(templateName %in% list.files(tmpdir))
      detach(params)
      rm(params)
      
    })

test_that("Creation of summary plot template", {
      
      templateName <- "summaryPlotTemplate.Rmd"
      configFilePath <- file.path(tmpdir,
          sprintf("%sConfig.yml", tools::file_path_sans_ext(templateName))
      )
      
      paramsYaml <- list(
          pathDataFolder = testPathData,
          template = templateName,
          templatePackage = "medicalMonitoring",
          reportTitle = gsub("(.+)Template[.].+", "\\1 template", templateName),
          dataFileName =  list.files(testPathData, pattern = "xpt"),
          tableParams = list(
              var = c("USUBJID", "EXDOSE"),
              stats = "getStats(type = 'n')"
          ),
          plotFunction = "barplotMonitoring",
          plotParams = list(xVar = "variableGroup", yVar = "statN")
      )      
      
      write_yaml(
          paramsYaml,
          file = configFilePath
      )
      params <- read_yaml(configFilePath)
      
      pathTemplate <- system.file("template", templateName, package = "medicalMonitoring")      
      
      rmarkdown::render(
          pathTemplate,
          output_file = file.path(tmpdir, templateName)
      )
      expect_true(any(file.exists(templateName, tmpdir)))
      expect_true(templateName %in% list.files(tmpdir))
      detach(params)
      rm(params)
      
    })

