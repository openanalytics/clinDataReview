context("Test 'runPatientProfileTemplateReport'")

test_that("Run the patient profile template", {
      
      dataPath <- system.file(
          "data", "ADaMDataPelican.RData",
          package = "glpgUtilityFct"
      )      
      expect_error(
          runPatientProfileTemplateReport(
              dataPath = dataPath,
              outputFile = file.path(tempdir(), "profile.pdf")
          )
      )
      dataPath <- system.file(
          "extdata",
          package = "glpgUtilityFct"
      )
      expect_error(
          runPatientProfileTemplateReport(
              dataPath = dataPath,
              outputFile = file.path(tempdir(), "profile.pdf")
          ),
          "Patient profiles template:‘patientProfiles-medicalMonitoring.Rmd’ already exists"
      )
      report <- runPatientProfileTemplateReport(
          dataPath = dataPath,
          outputFile = file.path(tempdir(), "subjectProfile.pdf"),
          overwrite = TRUE
      )
      expect_is(report, "character")
      expect_identical(report, file.path(tempdir(), "patientProfiles-medicalMonitoring.html"))
      
    })