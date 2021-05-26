library(covr)
pc <- package_coverage(
    path = "~/git/clinDataReview/package/clinDataReview",
    type = "tests", quiet = FALSE, clean = FALSE
)
report(x = pc,
    file = paste0("testCoverage-tests-clinDataReview",
        packageVersion("clinDataReview"), ".html")
)