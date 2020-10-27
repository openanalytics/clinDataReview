context("Test reporting template functions")

library(yaml)
library(jsonlite)

test_that("Test check of config file", {
      
      tmpdir <- tempdir()
      
      ## Division config file
      file.create("configDivision.yaml")
      configFileDivision <- file.path(tmpdir, "configDivision.yml") 
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

test_that("Template documentation has been properly created", {
      
	  helpTemplate <- help("medicalMonitoring-templates", package = "medicalMonitoring", help_type = "text")
	  helpTemplateText <- capture.output(print(helpTemplate))
	  
	  expect_true(
		any(grepl(
			".*This report includes a division, i.e. extra chapter, section of.*",
			helpTemplateText
		)) 
	 )
      
})

test_that("Get documentation from a JSON schema", {
      
      path <- system.file("template", package = "medicalMonitoring")
      
      jsonFileNames <- list.files(
          pattern = ".json",
          path
      )
      fileSpecPath <- file.path(path, jsonFileNames[1])      
      templateSpec <- jsonlite::fromJSON(fileSpecPath)
      
      jsonSchemaDoc <- JSONSchToRd(JSONSch = templateSpec)
      expect_is(jsonSchemaDoc, "character")
      
})
