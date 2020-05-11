context("Test miscellaneous functions")


test_that("createPatientProfileVar", {
			
			someData <- data.frame(
					"USUBJID" = c("ID1", "ID2"),
					"VAR" = c(1, 2)
			)
#			patientProfilePath <- system.file(
#					...,
#					package = "patientProfilesVis"
#					)			
			expect_error(
					createPatientProfileVar(
							data = someData,
							patientProfilePath = "unExistingFolder"
					)
			)
#			expect_error(
#					createPatientProfileVar(
#							data = someData,
#							patientProfilePath = patientProfilePath,
#							subjectVar = "usubjid"
#					)
#			)
			
		})