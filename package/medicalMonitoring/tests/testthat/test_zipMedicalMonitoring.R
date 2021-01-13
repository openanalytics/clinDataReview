context("Test zip of reports")

tmpdir <- tempdir()

test_that("Creation of redirect page", {
      
      createRedirectPage(
          dependencyDir = tmpdir,
          redirectPage = file.path(tmpdir, "report.html")
      )
      expect_true("report.html" %in% list.files(tmpdir))
      
      textInHtml <- readLines(file.path(tmpdir, "report.html"))
      expect_is(textInHtml, "character")
      expect_true(any(grepl("1-introduction.html", textInHtml)))
      
    })