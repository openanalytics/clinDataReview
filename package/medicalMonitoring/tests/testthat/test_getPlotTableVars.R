context("Test extraction of variables")

# Seems from 'coverage' report that
# 'getPlotTableVars' is also tested through the other plot functionalities?
# So other tests are skipped

data <- data.frame(A = c(1, 2), B = c(3, 4))
plotArgs <- list(
    data = data,
    xVar = "A", yVar = "B",
    tableVars = colnames(data),
    tableLab = NULL
)
labelVars <- c(A = "firstName", B = "secondName")
plotArgsWithLabel <- c(plotArgs, list(labelVars = labelVars))

test_that("Error for no current implementation", {
      
      plotFunction <- "plot"
      expect_error(
          getPlotTableVars(plotFunction, plotArgs),
          "Extraction of table parameters for function: .* is not yet implemented."
      )
      
    })

test_that("Extraction of table label for barplot when no labels are specified", {
      
      plotFunction <- "barplotMonitoring"
      
      expect_silent(
          res <- getPlotTableVars(plotFunction, plotArgs)
      )
      expect_equal(class(res), "character")
      expect_length(res, 2)
      attrRes <- attributes(res)
      expect_equal(class(attrRes), "list")
      expect_named(attrRes$tableLab)
      expect_equal(attrRes$tableLab, c(A = "A", B  = "B"))
      
    })

test_that("Extraction of table label for barplot when labels are specified", {
      
      plotFunction <- "barplotMonitoring"
      
      expect_silent(
          res <- getPlotTableVars(plotFunction, plotArgsWithLabel)
      )
      expect_equal(class(res), "character")
      expect_length(res, 2)
      attrRes <- attributes(res)
      expect_equal(class(attrRes), "list")
      expect_named(attrRes$tableLab)
      expect_equal(attrRes$tableLab, c(A = "firstName", B  = "secondName"))
      expect_equal(attrRes$tableLab, labelVars)
      
    })

test_that("Extraction of table label for scatterplot when no labels are specified", {
      
      plotFunction <- "scatterplotMonitoring"
      
      expect_silent(
          res <- getPlotTableVars(plotFunction, plotArgs)
      )
      expect_equal(class(res), "character")
      expect_length(res, 2)
      attrRes <- attributes(res)
      expect_equal(class(attrRes), "list")
      expect_named(attrRes$tableLab)
      expect_equal(attrRes$tableLab, c(A = "A", B  = "B"))  
      
    })

test_that("Extraction of table label for scatterplot when labels are specified", {
      
      plotFunction <- "scatterplotMonitoring"
      
      expect_silent(
          res <- getPlotTableVars(plotFunction, plotArgsWithLabel)
      )
      expect_equal(class(res), "character")
      expect_length(res, 2)
      attrRes <- attributes(res)
      expect_equal(class(attrRes), "list")
      expect_named(attrRes$tableLab)
      expect_equal(attrRes$tableLab, c(A = "firstName", B  = "secondName"))  
      expect_equal(attrRes$tableLab, labelVars)  
      
    })

test_that("Extraction of table label for timeProfileIntervalPlot when no labels are specified", {
      
      plotFunction <- "timeProfileIntervalPlot"
      
      expect_silent(
          res <- getPlotTableVars(plotFunction, plotArgs)
      )
      expect_equal(class(res), "character")
      expect_length(res, 2)
      attrRes <- attributes(res)
      expect_equal(class(attrRes), "list")
      expect_named(attrRes$tableLab)
      expect_equal(attrRes$tableLab, c(A = "A", B  = "B"))  
      
    })


test_that("Extraction of table label for timeProfileIntervalPlot when labels are specified", {
      
      plotFunction <- "timeProfileIntervalPlot"
      
      expect_silent(
          res <- getPlotTableVars(plotFunction, plotArgsWithLabel)
      )
      expect_equal(class(res), "character")
      expect_length(res, 2)
      attrRes <- attributes(res)
      expect_equal(class(attrRes), "list")
      expect_named(attrRes$tableLab)
      expect_equal(attrRes$tableLab, c(A = "firstName", B  = "secondName"))  
      expect_equal(attrRes$tableLab, labelVars)  
      
    })


test_that("Extraction of table label for other plots when no labels are specified", {
      
      plotFunction <- "plotCountMonitoring"
      plotArgsCounts <- c(plotArgs, list(valueVar = "A"))
      
      expect_silent(
          res <- getPlotTableVars(plotFunction, plotArgsCounts)
      )
      expect_equal(class(res), "character")
      expect_length(res, 2)
      attrRes <- attributes(res)
      expect_equal(class(attrRes), "list")
      expect_named(attrRes$tableLab)
      expect_equal(attrRes$tableLab, c(A = "A", B  = "B"))  
      
    })

test_that("Extraction of table label for other plots when labels are specified", {
      
      plotFunction <- "plotCountMonitoring"
      plotArgsCounts <- c(plotArgsWithLabel, list(valueVar = "A"))
      
      expect_silent(
          res <- getPlotTableVars(plotFunction, plotArgsCounts)
      )
      expect_equal(class(res), "character")
      expect_length(res, 2)
      attrRes <- attributes(res)
      expect_equal(class(attrRes), "list")
      expect_named(attrRes$tableLab)
      expect_equal(attrRes$tableLab, c(A = "firstName", B  = "secondName"))  
      expect_equal(attrRes$tableLab, labelVars)       
      
    })

test_that("Extraction of table label and valueVar for other plots when labels are specified", {
      
      dataCounts <- data
      dataCounts$valueV <- c(5, 6)
      labelVarsCounts <- c(labelVars, valueV = "thirdName")
      plotFunction <- "plotCountMonitoring"
      plotArgsCounts <- list(
          data = dataCounts,
          xVar = "A", yVar = "B",
          tableVars = c("A", "B"),
          valueVar = "valueV",
          tableLab = NULL,
          labelVars = labelVarsCounts
      )
      
#      expect_silent(
#          res <- getPlotTableVars(plotFunction, plotArgsCounts)
#      )
      
    })