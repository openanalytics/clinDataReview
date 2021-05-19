context("Test format plotly clinical data objects")

# Seems from 'coverage' report that
# 'formatPlotlyClinData' is also tested through the other plot functionalities?
# So other tests are skipped

library(plotly)

data <- data.frame(
    USUBJID = c("ID1", "ID2", "ID3", "ID4"),
    IDVAR2 = c("i1", "i2", "i3", "i4"),
    A = c(10, 12, 16, 18),
    pathVar = "<a href=\"./path-to-report\">label</a>",
    stringsAsFactors = FALSE
)
plotlyObj <- plot_ly()

test_that("Use 'pathVar' argument", {
      
      plotlyOut <- formatPlotlyClinData(
          pl = plotlyObj,
          data = data,
          pathVar = "pathVar"
      )
      expect_is(plotlyOut, "plotly")
      
    })

test_that("Use 'pathVar' in combination with more than one 'idVar'", {
      
      plotlyOut <- formatPlotlyClinData(
          pl = plotlyObj,
          data = data,
          idVar = c("USUBJID", "IDVAR2"),
          pathVar = "pathVar"
      )
      expect_is(plotlyOut, "plotly")
      
    })

test_that("Error when duplicates in data lead to different paths", {
      
      data <- data.frame(
          USUBJID = c("ID1", "ID2", "ID3", "ID3"),
          IDVAR2 = c("i1", "i2", "i3", "i3"),
          A = c(10, 12, 16, 18),
          pathVar = c(
              "<a href=\"./path-to-report\">label</a>",
              "<a href=\"./path-to-report\">label</a>",
              "<a href=\"./path-to-report\">label</a>",
              "<a href=\"./path-to-report-other\">labelOther</a>"
          ),
          stringsAsFactors = FALSE
      )
      
      expect_error(
          formatPlotlyClinData(
              pl = plotlyObj,
              data = data,
              idVar = c("USUBJID", "IDVAR2"),
              pathVar = "pathVar"
          ),
          "Different .* available for specific .*"
      )
      
    })