context("Test reporting formats")

test_that("Format of gitbook for medical monitoring", {
      
      output <- gitbook_medicalMonitoring_report()
      expect_is(output, list)
      expect_legth(output, 12)
      
      output <- gitbook_medicalMonitoring_report(
          split_by = 'section',
      )
      expect_is(output, list)
      
      # Further testing in glpgStyle::gitbook_report
    })