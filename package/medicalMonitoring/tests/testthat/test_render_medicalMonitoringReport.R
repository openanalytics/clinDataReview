library(yaml)

tmpdir <- tempdir()

## General config file
file.create("config.yaml")
configFileGeneral <- paste0(tmpdir, "/config.yml") 
write_yaml(
    list(
        study = "Study name"
    ),
    configFileGeneral
)

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
configFiles <- c(configFileGeneral, configFile1, configFile2)
configFiles <- gsub(".+[/]config(.+)", "config\\1", configFiles)

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

# Add tests for getParamsFromConfig
# getParamsFromConfig(configFile = configFiles[1], configDir = tmpdir)

file.remove(configFiles)