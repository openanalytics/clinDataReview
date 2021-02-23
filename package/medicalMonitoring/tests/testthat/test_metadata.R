context("Test metadata functionality")

test_that("Check availability of metadata", {
      
      listName <- list(A = c(1, 2))
      
      expect_identical(
          checkAvailabilityMetadata(listName, subListName = "A"),
          listName$A
      )
      
      expect_identical(
          checkAvailabilityMetadata(listName, subListName = "B"),
          "Not available"
      )
      
    })