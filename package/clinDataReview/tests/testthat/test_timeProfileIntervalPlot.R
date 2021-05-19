context("Test the 'timeProfileIntervalPlot' function")

library(plotly)

subjectIDs <- as.character(rep(c(1, 2, 3), each = 3))
startDay <- c(NA, NA, 11, 44, 12, 7, 48, 54, 11)
endDay <- c(NA, NA, 12, NA, 26, 9, 50, NA, 13)

myData <- data.frame(
    subjectID = subjectIDs,
    startDay = startDay,
    endDay = endDay
)

####
# Not sure those tests make sense as it is?


test_that("Test plot of 'timeProfileIntervalPlot'", {
      
      plotObj <- timeProfileIntervalPlot(
          data = myData,
          paramVar = "subjectID",
          timeStartVar = "startDay",
          timeEndVar = "endDay"
      )
      expect_is(plotObj, "plotly")
      
#      plData <- plotly_build(plotObj)$x$data
#      plDataPointsAll <- plData[sapply(plData, function(x) (x$mode == "markers" & !is.null(x$set)))]
#      plDataPoints <- lapply(plDataPointsAll, function(x)
#            data.frame(
#                x = as.numeric(x$x), 
#                y = as.numeric(x$y), 
#                key = as.character(x$key),
#                stringsAsFactors = FALSE
#            )
#      )
#      cols <- c("endDay", "startDay", "subjectID")
#      dataPointLine <- dataPlot[, cols]
#      dataPointLine <- dataPointLine[do.call(order, dataPointLine), ]
#      
#      
#      plDataPoints <- setNames(do.call(rbind, plDataPoints), cols)
#      plDataPoints <- plDataPoints[do.call(order, plDataPoints), ]
      
      
    })

test_that("Test plot and table of 'timeProfileIntervalPlot'", {
      
      expect_warning(
          timeProfileIntervalPlot(
              data = myData,
              paramVar = "subjectID",
              timeStartVar = "startDay",
              timeEndVar = "endDay",
              table = TRUE
          ),
          "Subject ID variable: USUBJID is not available in the data, so it is ignored." 
      )
      expect_silent(
          timeProfileIntervalPlot(
              data = myData,
              paramVar = "subjectID",
              timeStartVar = "startDay",
              timeEndVar = "endDay",
              table = TRUE,
              idVar = "subjectID"
          )
      )
      plotObjList <- timeProfileIntervalPlot(
          data = myData,
          paramVar = "subjectID",
          timeStartVar = "startDay",
          timeEndVar = "endDay",
          table = TRUE,
          idVar = "subjectID"
      )
      expect_is(plotObjList, "clinDataReview")
      expect_is(plotObjList, "list")
      expect_is(plotObjList[[1]], "plotly")
      expect_is(plotObjList[[2]], "datatables")
      
    })

test_that("Use two variables for 'paramVar'", {
      
      plotObj <- timeProfileIntervalPlot(
          data = myData,
          paramVar = c("subjectID", "startDay"),
          timeStartVar = "startDay",
          timeEndVar = "endDay"
      )
      expect_is(plotObj, "htmlwidget")
     
    })

test_that("Use shape variables for time", {
      
      plotObj <- timeProfileIntervalPlot(
          data = myData,
          paramVar = "subjectID",
          timeStartVar = "startDay",
          timeEndVar = "endDay",
          timeStartShapeVar = "startDay",
          timeEndShapeVar = "endDay"
      )
      expect_is(plotObj, "htmlwidget")
            
    })

test_that("Set hover vars but not hover lab", {
      
      plotObj <- timeProfileIntervalPlot(
          data = myData,
          paramVar = "subjectID",
          timeStartVar = "startDay",
          timeEndVar = "endDay",
          timeStartShapeVar = "startDay",
          timeEndShapeVar = "endDay",
          hoverVars = c("startDay", "endDay")
      )
      expect_is(plotObj, "htmlwidget")     
      
    })

test_that("Set color var but not color palette", {
      
      plotObj <- timeProfileIntervalPlot(
          data = myData,
          paramVar = "subjectID",
          timeStartVar = "startDay",
          timeEndVar = "endDay",
          timeStartShapeVar = "startDay",
          timeEndShapeVar = "endDay",
          colorVar = "startDay"
      )
      expect_is(plotObj, "htmlwidget")
      
    })
