context("Test table for clinical data")

# Seems from 'coverage' report that
# 'tableClinData' is also tested through the other plot functionalities?
# So other tests are skipped

data <- data.frame(
    USUBJID = c("ID1", "ID2", "ID3", "ID4"),
    A = c(10, 12, 16, 18),
    pathVar = "<a href=\"./path-to-report\">label</a>",
    stringsAsFactors = FALSE
)

test_that("Table in the default application", {
      
      expect_silent(
          tableMon <- 
              tableClinData(
                  data
              )
      )
      expect_is(tableMon, "datatables")
      expect_is(tableMon, "htmlwidget")
      
    })

test_that("Have warning for 'pathVar' argument", {
      
      expect_warning(
          tableClinData(
              data,
              pathVar = "varName"
          ),
          "Variable with path to subject profile: .* is not available"
      )
      
    })

test_that("Use 'pathVar' argument", {
      
      expect_silent(
          tableMon <- tableClinData(
              data,
              pathVar = "pathVar"
          )
      )
      expect_is(tableMon, "datatables")
      expect_is(tableMon, "htmlwidget")
           
    })

test_that("Use 'pathExpand' argument", {
      
      expect_silent(
          tableMon <- tableClinData(
              data,
              pathVar = "pathVar",
              pathExpand = TRUE
          )
      )
      expect_is(tableMon, "datatables")
      expect_is(tableMon, "htmlwidget")
      
    })

