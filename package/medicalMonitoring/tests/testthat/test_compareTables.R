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
			expect_error(
					compareTables(
							newData = newData,
							oldData = newData,
							referenceVars = referenceVars,
							changeableVars = changeableVars,
							labelVars = labelVars,
							subjectIDvar = "usubjid"
					)
			)			
			################
			## Simulation ##
			expect_message(
					compareTables(
							newData = newData,
							oldData = oldData,
							referenceVars = referenceVars,
							changeableVars = changeableVars,
							labelVars = labelVars
					)
			)
			comparisonOutput <- compareTables(
					newData = newData,
					oldData = oldData,
					referenceVars = referenceVars,
					changeableVars = changeableVars,
					labelVars = labelVars
			)
			expect_is(comparisonOutput, "list")
			expect_is(comparisonOutput[["summaryChanges"]], "matrix")
			expect_is(comparisonOutput[["summaryChangesInteractive"]], "datatables")
			expect_is(comparisonOutput[["comparisonTableInteractive"]], "datatables")
			
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
							referenceVars = referenceVars
					)
			)
			dfOutput <- getComparisonDF(
					newData = newDataToUse,
					oldData = oldDataToUse,
					referenceVars = referenceVars
			)			
			expect_type(dfOutput, "list")
			expect_length(dfOutput, 2)
			
			comparisonDF <- dfOutput[["comparisonDF"]]
			
			##########################
			## Test on comparisonDF ##
			expect_identical(
					colnames(comparisonDF)[1 : 2],
					c("grp", "chng_type")
			)
			expect_setequal(
					colnames(comparisonDF)[grepl("_diff", colnames(comparisonDF))],
					c("grp_diff", "chng_type_diff", sprintf("%s_diff", varsToUse))
			)
			############################
			## Test on summaryChanges ##
			summaryChanges <- dfOutput[["summaryChanges"]]
			expect_length(summaryChanges, 5)
			expect_is(summaryChanges, "numeric")
			
		})

test_that("formatComparisonDF", {
			
#			expect_setequal(
#					colnames(dfOutput)[getColumnsIdx(dfOutput, labeVarsToUse)],
#					labeVarsToUse
#			)
		})

