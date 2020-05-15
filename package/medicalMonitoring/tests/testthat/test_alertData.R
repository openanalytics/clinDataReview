
library(glpgUtilityFct)
data(SDTMDataPelican)
dataPath <- system.file("extdata", package = "glpgUtilityFct")

dataDM <- SDTMDataPelican$DM

test_that("Correct extraction of data from list", {
			
			# Option 1
			alertsArgs <- list(
							dataset = "adsl"
					)
				
			alertData <- getDataFromAlertList(
					listArgs = alertsArgs,
					dataPath = dataPath
			)
			labelVars <- attr(alertData, "labelVars")
			
			expect_is(alertData, "data.frame")
			expect_is(class(labelVars), "character")
			
			# Option 2
			annotateParams <- NULL
			dataAnnot <- annotateData(
					data = dataDM, 
					dataPath = pathDataFolder, 
					annotations = annotateParams,
					verbose = TRUE,
					labelVars = labelVars
			)
			
			alertsArgs <- list(
					data = dataAnnot
			)
			
			alertData <- getDataFromAlertList(
					listArgs = alertsArgs,
					dataPath = dataPath
			)
			labelVars <- attr(alertData, "labelVars")
			
			expect_is(alertData, "data.frame")
			expect_is(class(labelVars), "character")
			
			# Option 3
			filterParams <- list(
					list(var = "RACE", value = "WHITE")
			)
			dataFiltered <- filterData(
					data = dataAnnot, 
					filters = filterParams, 
					verbose = TRUE,
					labelVars = labelVars
			)
			alertsArgs <- list(
					data = dataFiltered
			)		
			alertData <- getDataFromAlertList(
					listArgs = alertsArgs,
					dataPath = dataPath
			)
			labelVars <- attr(alertData, "labelVars")
			
			expect_is(alertData, "data.frame")
			expect_is(class(labelVars), "character")
			
			
			wrongList <- list("A" = NULL)
			expect_error(getDataFromAlertList(listArgs = wrongList, dataPath = dataPath))
			
		
		})
