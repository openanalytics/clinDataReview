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



