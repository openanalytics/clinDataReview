context("Test reporting template functions")

library(yaml)

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
