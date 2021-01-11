context("Test template creation")

library(yaml)
library(rmarkdown)

tmpdir <- tempdir()

expect_that("Creation of division template", {
      
      # Read division config from available one in tests folder
      testPathBase <- normalizePath(path = "../files")
      testPathConfig <- file.path(testPathBase, "config")
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

expect_that("Creation of listing template", {
      
      testPathBase <- normalizePath(path = "../dataTesting")
      configFilePath <- file.path(tmpdir, "listingConfig.yml")
      paramsYaml <- list(
          pathDataFolder = testPathBase,
          template = "listingTemplate.Rmd",
          templatePackage = "medicalMonitoring",
          reportTitle = "Adverse events: listing",
          reportTitleLevel = 2,
          dataFileName =  list.files(testPathBase, pattern = "xpt"),
          tableParams = list(
              tableVars = c("USUBJID", "EXDOSE")
          )
      )      
      write_yaml(
          paramsYaml,
          file = configFilePath
      )
      params <- read_yaml(configFilePath)
      
      templateName <- "listingTemplate.Rmd"
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