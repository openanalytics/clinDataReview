context("Test plots utility functions for facets")

# Seems from 'coverage' report that
# some of the functions have already be tested through the other plot functionalities?
# So other tests are skipped

data <- data.frame(
    A = c(1, 2), B = c(3, 4)
)

test_that("Set facetting layout for for not specified ncol", {
      
      facetPars <- list(
          facets = 2,
          ncol = NULL,
          nrow = 1
      )
      expect_silent(
          res <- clinDataReview:::setFacetLayoutWrap(data, facetPars)
      )
      expect_equal(class(res), "list")
      expect_equal(res$nrow, 1)
      expect_equal(res$ncol, ceiling(2/facetPars$nrow))
      
    })


test_that("Set facetting layout for for not specified nrow", {
      
      facetPars <- list(
          facets = 2,
          ncol = 3,
          nrow = NULL
      )
      
      expect_silent(
          res <- clinDataReview:::setFacetLayoutWrap(data, facetPars)
      )
      expect_silent(
          res <- clinDataReview:::setFacetLayoutWrap(data, facetPars)
      )
      expect_equal(class(res), "list")
      expect_equal(res$ncol, 3)
      expect_equal(res$nrow, ceiling(2/facetPars$ncol))
      
    })

test_that("Get dimentions of plot when width is not specified", {
      
      expect_silent(
          res <- clinDataReview:::getSizePlotClinData(
              width = NULL,
              height = 700
          )
      )
      expect_equal(class(res), "numeric")
      expect_named(res)
      expect_true(res["height"] == 700)
      
      # Compute by hand accordingly to the function
      plotSize <- 700/1
      width <- plotSize*1
      width <- width + width/1*0.2
      
      expect_true(res["width"] == width)
      
    })

test_that("Get dimentions of plot when width is not specified and legend is FALSE", {
      
      expect_silent(
          res <- clinDataReview:::getSizePlotClinData(
              width = NULL,
              height = 700,
              legend = FALSE
          )
      )
      expect_equal(class(res), "numeric")
      expect_named(res)
      expect_true(res["height"] == 700)      
      expect_true(res["width"] == 700)
      
    })

test_that("Get dimentions of plot when height is not specified and legend is top", {
      
      expect_silent(
          res <- clinDataReview:::getSizePlotClinData(
              width = 700,
              height = NULL,
              legendPosition = "top"
          )
      )
      expect_equal(class(res), "numeric")
      expect_named(res)
      expect_true(res["width"] == 700)
      
      # Compute by hand accordingly to the function
      plotSize <- 700/1
      height <- plotSize*1
      height <- height + height/1*0.2
      
      expect_true(res["height"] == height)
      
    })

test_that("Get dimentions of plot when height is not specified and legend is FALSE", {
      
      expect_silent(
          res <- clinDataReview:::getSizePlotClinData(
              width = 700,
              height = NULL,
              legend = FALSE
          )
      )
      expect_equal(class(res), "numeric")
      expect_named(res)
      expect_true(res["height"] == 700)      
      expect_true(res["width"] == 700)
      
    })


