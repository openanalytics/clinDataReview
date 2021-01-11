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
      
      divTemplate <- rmarkdown::render(
          pathTemplate,
          output_file = file.path(tmpdir, templateName)
      )
      expect_true(any(file.exists(templateName, tmpdir)))
      detach(params)
      
    })

