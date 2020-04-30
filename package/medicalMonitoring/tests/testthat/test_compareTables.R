context("Test 'compareTables'")

library(glpgUtilityFct)
data(SDTMDataPelican)
labelVars <- attr(SDTMDataPelican, "labelVars")
dataAE <- SDTMDataPelican$AE

newData <- dataAE
oldData <- dataAE
oldData[oldData$USUBJID == "GLPG2737-CL-202-4902-01", "AESER"] <- "Y"
oldData$AESEV[oldData$USUBJID == "GLPG2737-CL-202-4903-03"] <- "SEVERE"

referenceVars <- c("USUBJID", "AEBODSYS", "AEDECOD")
changeableVars  <- c("AESER", "AESEV", "AEREL", "AEACN")

test_that("Compare data sets", {
			
			expect_error(
					compareTables(
							newData = newData,
							oldData = oldData
					)
			)
			expect_error(
					compareTables(
							newData = newData,
							oldData = oldData,
							referenceVars = referenceVars
					)					
			)
			expect_error(compareTables(
							newData = newData,
							oldData = oldData,
							changeableVars = changeableVars
					)
			)
			expect_warning(
					expect_error(
							compareTables(
									newData = newData,
									oldData = newData,
									referenceVars = referenceVars,
									changeableVars = changeableVars
							)
					)
			)
			################
			## Simulation ##
			expect_message(
					expect_warning(
							compareTables(
									newData = newData,
									oldData = oldData,
									referenceVars = referenceVars,
									changeableVars = changeableVars
							)
					)
			)
			dtOutput <- compareTables(
					newData = newData,
					oldData = oldData,
					referenceVars = referenceVars,
					changeableVars = changeableVars,
					labelVars = labelVars
			)
			expect_is(dtOutput, "datatables")
			
		})


test_that("getComparisonDF", {
			
			varsToUse <- c(referenceVars, changeableVars)
			
			oldDataToUse <- oldData[, getColumnsIdx(oldData, varsToUse)]
			newDataToUse <- newData[, getColumnsIdx(newData, varsToUse)]
			varsToUse <- c(referenceVars, changeableVars)
			labeVarsToUse <- labelVars[varsToUse]
			
			expect_message(
					getComparisonDF(
							newData = newDataToUse,
							oldData = oldDataToUse,
							referenceVars = referenceVars,
							labelVars = labeVarsToUse)
			)
			dfOutput <- getComparisonDF(
					newData = newDataToUse,
					oldData = oldDataToUse,
					referenceVars = referenceVars,
					labelVars = labeVarsToUse)
			
			expect_is(dfOutput, "data.frame")
			
			expect_identical(
					colnames(dfOutput)[1 : 2],
					c("grp", "chng_type")
			)			
			expect_setequal(
					colnames(dfOutput)[getColumnsIdx(dfOutput, labeVarsToUse)],
					labeVarsToUse
			)
			expect_setequal(
					colnames(dfOutput)[grepl("_diff", colnames(dfOutput))],
					c("grp_diff", "chng_type_diff", sprintf("%s_diff", varsToUse))
			)			
			
		})

test_that("formatComparisonDF", {
			
			
		})

