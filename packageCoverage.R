library(covr)
pc <- package_coverage(
    path = "~/git/GLPGMedicalMonitoring/package/medicalMonitoring",
    type = "tests", quiet = FALSE, clean = FALSE
)
report(x = pc,
    file = paste0("testCoverage-tests-medicalMonitoring",
        packageVersion("medicalMonitoring"), ".html")
)
