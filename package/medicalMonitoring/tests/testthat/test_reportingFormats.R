context("Test reporting formats")

test_that("Format of gitbook for medical monitoring", {
      
      output <- gitbook_medicalMonitoring_report()
      expect_is(output, "rmarkdown_output_format")
      #expect_length(output, 13)
      
      output <- gitbook_medicalMonitoring_report(
          split_by = 'section',
      )
      expect_is(output, "rmarkdown_output_format")
      
      # Further testing in glpgStyle::gitbook_report
    })