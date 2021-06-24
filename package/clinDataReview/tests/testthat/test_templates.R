context("Test template creation")

library(yaml)
library(rmarkdown)
library(haven)
library(tools)

tmpdir <- tempdir()

dataEX <- data.frame(
    "USUBJID" = c(1, 1, 2, 3, 4),
    "EXDOSE" = "100",
    stringsAsFactors = FALSE
)
testPathData <-  file.path(tmpdir, "dataTesting")
dir.create(testPathData)
write_xpt(dataEX, file.path(testPathData, "adex.xpt"))


test_that("Creation of division template", {
      
      # Read division config from available one in tests folder
      testPathFiles <- normalizePath(path = "../files")
      testPathConfig <- file.path(testPathFiles, "config")
	  
	  # Division config file
      configFile <- list.files(
			 path = testPathConfig, 
			 pattern = "config-adverseEvents-division.yml",
			 full.names = TRUE
	 )
      params <- read_yaml(configFile)
     
      templateName <- "divisionTemplate.Rmd"
      pathTemplate <- system.file("template", templateName, package = "clinDataReview")      
	  
	  expect_error(
		outputFile <- rmarkdown::render(
          input = pathTemplate,
		  output_dir = tmpdir,
		  intermediates_dir = tmpdir
      	),
		NA
	)
	  expect_true(file.exists(outputFile))
      detach(params)
      rm(params)
      
})

test_that("Creation of listing template", {
			
	skip_on_cran()
      
      templateName <- "listingTemplate.Rmd"
      configFilePath <- file.path(tmpdir,
          sprintf("%sConfig.yml", file_path_sans_ext(templateName))
      )
      
      paramsYaml <- list(
          pathDataFolder = testPathData,
          template = templateName,
          templatePackage = "clinDataReview",
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
      
      pathTemplate <- system.file("template", templateName, package = "clinDataReview")      
	  
	  expect_error(
	      outputFile <- rmarkdown::render(
	          pathTemplate,
	          output_dir = tmpdir,
			  intermediates_dir = tmpdir
	      ),
		  NA
  	)
	  expect_true(file.exists(outputFile))
      detach(params)
      rm(params)
      
    })

test_that("Creation of counts visualization template", {
			
	skip_on_cran()
      
      templateName <- "countsVisualizationTemplate.Rmd"
      configFilePath <- file.path(tmpdir,
          sprintf("%sConfig.yml", file_path_sans_ext(templateName))
      )
      
      paramsYaml <- list(
          pathDataFolder = testPathData,
          template = templateName,
          templatePackage = "clinDataReview",
          reportTitle = gsub("(.+)Template[.].+", "\\1 template", templateName),
          dataFileName =  list.files(testPathData, pattern = "xpt"),
          countVar = "EXDOSE"
      )      
      
      write_yaml(
          paramsYaml,
          file = configFilePath
      )
      params <- read_yaml(configFilePath)
      
      pathTemplate <- system.file("template", templateName, package = "clinDataReview")      
      
	  expect_error(
	      outputFile <- rmarkdown::render(
	          pathTemplate,
			  output_dir = tmpdir,
			  intermediates_dir = tmpdir
	      ),
		  NA
  		)
	  expect_true(file.exists(outputFile))
      detach(params)
      rm(params)
      
    })

test_that("Creation of plot template", {
			
	skip_on_cran()
      
      templateName <- "plotTemplate.Rmd"
      configFilePath <- file.path(tmpdir,
          sprintf("%sConfig.yml", file_path_sans_ext(templateName))
      )
      
      paramsYaml <- list(
          pathDataFolder = testPathData,
          template = templateName,
          templatePackage = "clinDataReview",
          reportTitle = gsub("(.+)Template[.].+", "\\1 template", templateName),
          dataFileName =  list.files(testPathData, pattern = "xpt"),
          plotFunction = "scatterplotClinData",
          plotParams = list(xVar = "USUBJID", yVar = "EXDOSE")
      )      
      
      write_yaml(
          paramsYaml,
          file = configFilePath
      )
      params <- read_yaml(configFilePath)
      
      pathTemplate <- system.file("template", templateName, package = "clinDataReview")      
      
	  expect_error(
	      outputFile <- rmarkdown::render(
	          pathTemplate,
			  output_dir = tmpdir,
			  intermediates_dir = tmpdir
	      ),
		  NA
  	)
	  expect_true(file.exists(outputFile))
      detach(params)
      rm(params)
      
    })

test_that("Creation of summary plot template", {
			
	skip_on_cran()
      
      templateName <- "summaryPlotTemplate.Rmd"
      configFilePath <- file.path(tmpdir,
          sprintf("%sConfig.yml", file_path_sans_ext(templateName))
      )
      
      paramsYaml <- list(
          pathDataFolder = testPathData,
          template = templateName,
          templatePackage = "clinDataReview",
          reportTitle = gsub("(.+)Template[.].+", "\\1 template", templateName),
          dataFileName =  list.files(testPathData, pattern = "xpt"),
          tableParams = list(
              var = c("USUBJID", "EXDOSE"),
              stats = "getStats(type = 'n')"
          ),
          plotFunction = "barplotClinData",
          plotParams = list(xVar = "variableGroup", yVar = "statN")
      )      
      
      write_yaml(
          paramsYaml,
          file = configFilePath
      )
      params <- read_yaml(configFilePath)
      
      pathTemplate <- system.file("template", templateName, package = "clinDataReview")      
      
	  expect_error(
		  outputFile <- rmarkdown::render(
	          pathTemplate,
	          output_file = tmpdir,
			  intermediates_dir = tmpdir
	      ),
		  NA
  		)
	  expect_true(file.exists(outputFile))
      detach(params)
      rm(params)
      
    })

test_that("Creation of summary table template", {
			
	skip_on_cran()
      
      templateName <- "summaryTableTemplate.Rmd"
      configFilePath <- file.path(tmpdir,
          sprintf("%sConfig.yml", file_path_sans_ext(templateName))
      )
      
      paramsYaml <- list(
          pathDataFolder = testPathData,
          template = templateName,
          templatePackage = "clinDataReview",
          reportTitle = gsub("(.+)Template[.].+", "\\1 template", templateName),
          dataFileName =  list.files(testPathData, pattern = "xpt"),
          tableParams = list(
              var = c("USUBJID", "EXDOSE"),
              stats = "setNames(getStats(type = 'n'), nm = 'n')"
          )
      )      
      
      write_yaml(
          paramsYaml,
          file = configFilePath
      )
      params <- read_yaml(configFilePath)
      
      pathTemplate <- system.file("template", templateName, package = "clinDataReview")      
      
	  expect_error(
		  outputFile <- rmarkdown::render(
	          pathTemplate,
			  output_file = tmpdir,
			  intermediates_dir = tmpdir
	      ),
		  NA
  		)
	  expect_true(file.exists(outputFile))
      detach(params)
      rm(params)
      
    })


test_that("Creation of patient profiles template", {
			
	skip_on_cran()
      
      templateName <- "patientProfilesTemplate.Rmd"
      configFilePath <- file.path(tmpdir,
          sprintf("%sConfig.yml", file_path_sans_ext(templateName))
      )
      
      paramsYaml <- list(
          pathDataFolder = testPathData,
          patientProfilePath = file.path(tmpdir, "patientProfiles"),
          template = templateName,
          templatePackage = "clinDataReview",
          reportTitle = gsub("(.+)Template[.].+", "\\1 template", templateName),
          patientProfilesParams = list(
              list(
                  typePlot = "text",
                  dataFileName =  list.files(testPathData, pattern = "xpt"),
                  plotParams = list(
                      paramValueVar = c("USUBJID", "EXDOSE")
                  )
              )
          )
      )      
      
      write_yaml(
          paramsYaml,
          file = configFilePath
      )
      params <- read_yaml(configFilePath)
      
      pathTemplate <- system.file("template", templateName, package = "clinDataReview")      
      
	  expect_error(
	      outputFile <- rmarkdown::render(
	          pathTemplate,
			  output_file = tmpdir,
			  intermediates_dir = tmpdir
	      ),
		  NA
  		)
	  expect_true(file.exists(outputFile))
      expect_true(file.exists(file.path(tmpdir, "patientProfiles")))
      expect_true(all(
              grepl("subjectProfile-",
                  list.files(file.path(tmpdir, "patientProfiles"))
              )
          )
      )     
      detach(params)
      rm(params)
      
    })

