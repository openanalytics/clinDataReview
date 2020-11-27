context("Test formatToHierarchicalData")

data <- data.frame(
    varParent = c(NA, NA, "A", "B", "B"),
    varChild = c("a", NA, "Total", "c", "Total"),
    n = c(NA, 2, NA, 5, NA)
)

test_that("Filter records with NA", {
      expect_error(
          expect_warning(
              formatToHierarchicalData(
                  data = data,
                  vars = c("varParent", "varChild")
              ),
              "record(s) are filtered from the data"
          )
      )
    })