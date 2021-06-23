context("Test reporting formats")

data(iris)
vars <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")

dirPlots <- file.path(tempdir(), "figures")
dir.create(dirPlots)

pathPlotBasic <- file.path(dirPlots, paste0("scatterplotIris-basic", ".png"))
png(pathPlotBasic)
pairs(iris[, vars])
tmp <- dev.off()

test_that("Format of gitbook for clinical data", {
      
	output <- gitbook_clinDataReview_report()
	expect_is(output, "rmarkdown_output_format")
	#expect_length(output, 13)
      
	output <- gitbook_clinDataReview_report(
		split_by = 'section',
	)
	expect_is(output, "rmarkdown_output_format")
	unlink("gitbook.css")
      
})

test_that("Format html report", {
      
      res <- html_clinDataReview_report()
      expect_is(res, "rmarkdown_output_format")
      expect_identical(
          res$pandoc$to,
          "html"
      )
      
    })

test_that("Add logo", {
      
      expect_silent(
          res <- clinDataReview:::addLogoGitbook(
              logo = pathPlotBasic
          )
      )
      expect_is(res, "list")
      expect_named(res, "in_header")
      expect_is(res$in_header, "character")
      
    })

test_that("Add logo in gitbook for clinical data", {
      
      expect_silent(
          res <- gitbook_clinDataReview_report(logo = pathPlotBasic)
      )
      expect_is(res, "rmarkdown_output_format")
	  unlink("gitbook.css")
      
    })