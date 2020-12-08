context("Test reporting template functions")

library(yaml)
library(jsonlite)

test_that("Test check of config file", {
      
      tmpdir <- tempdir()
      
      ## Division config file
      configFileDivision <- file.path(tmpdir, "configDivision.yml")
      file.create(configFileDivision)
      write_yaml(
          list(
              reportTitle = "Study name"
          ),
          configFileDivision
      )
      
      # R CMD check runs on package binary, so without 'inst' folder:
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

test_that("Create template documentation", {
      
      # R CMD check runs on package binary, so without 'inst' folder:
      doc <- createTemplateDoc(
          templatePath = system.file("template", package = "medicalMonitoring")
      )
      expect_is(doc, "character")
      
      docRoxParType <- paste0(
          "\\section{Parameter type}{Please note that the type mentioned below ",
          "corresponds to the type in the config file (in YAML/JSON format).",
          "The mapping to R data type is as followed:",
          "\\itemize{",
          "\\item{string: }{character vector of length 1}",
          "\\item{integer: }{integer vector of length 1}",
          "\\item{array: }{vector/list without names}",
          "\\item{object: }{list with names}",
          "}}"
      )
      expect_identical(docRoxParType, doc[1])
      
    })

test_that("Invisible output from create template documentation", {
      
      expect_silent(
          res <- createTemplateDoc()
      )
      expect_equal(class(res), "character")
      expect_identical(res, "")
      
    })

test_that("Get documentation from a JSON schema", {
      
      # R CMD check runs on package binary, so without 'inst' folder:
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

test_that("Documentation for template not available from JSON schema", {
      
      tmpFolder <- tempfile()
      dir.create(tmpFolder)
      
      templateName <- file.path(tmpFolder, "template.Rmd")
      file.create(templateName)
      
      expect_silent(
          res <- createTemplateDoc(templatePath = tmpFolder)
      )
      expect_type(res, "character")
      expect_true(any(grepl("[\\]section[{]template[}]", res)))
      
    })
