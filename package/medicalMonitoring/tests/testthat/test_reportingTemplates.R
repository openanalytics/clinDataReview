context("Test reporting template functions")

library(yaml)
library(jsonlite)

test_that("Test check of config file", {
      
      tmpdir <- tempdir()
      
      ## Division config file
      file.create("configDivision.yaml")
      configFileDivision <- paste0(tmpdir, "/configDivision.yml") 
      write_yaml(
          list(
              reportTitle = "Study name"
          ),
          configFileDivision
      )
      
      refConfig <- system.file(package = "medicalMonitoring", "template", "divisionTemplate.json")
      
      expect_error(
          checkConfigFile(configFileDivision, configSpecFile = refConfig)
      )
      
      write_yaml(
          list(
              template = "divisionTemplate.Rmd",
              templatePackage = "medicalMonitoring",
              reportTitle = "Study name"
          ),
          configFileDivision
      )
      expect_silent(
          checkConfigFile(configFileDivision, configSpecFile = refConfig)
      )
      
    })

test_that("Get the path to Rmd templates", {
      
      path <- getPathTemplate(file = "divisionTemplate.Rmd")
      expect_is(path, "character")
      
      expect_warning(getPathTemplate(file = "divisionTempl.Rmd"))
      
      pathEmpty <- getPathTemplate(file = "divisionTempl.Rmd")
      expect_is(pathEmpty, "character")
      
    })

test_that("Get documentation from a JSON schema", {
      
      path <- system.file("template", package = "medicalMonitoring")
      
      jsonFileNames <- list.files(
          pattern = ".json",
          path
      )
      fileSpecPath <- sprintf("%s/%s", path, jsonFileNames[1])      
      templateSpec <- jsonlite::fromJSON(fileSpecPath)
      
      jsonSchemaDoc <- JSONSchToRd(JSONSch = templateSpec)
      expect_is(jsonSchemaDoc, "character")
      
    })
