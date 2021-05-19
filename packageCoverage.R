library(covr)
pc <- package_coverage(
    path = "~/git/GLPGMedicalMonitoring/package/clinDataReview",
    type = "tests", quiet = FALSE, clean = FALSE
)
report(x = pc,
    file = paste0("testCoverage-tests-clinDataReview",
        packageVersion("clinDataReview"), ".html")
)


#cyclocomp(function(arg) {
#      calulate(this); and(that) }
#)
#
#cyclocomp_package("glpgStyle")
#cyclocomp_package("inTextSummaryTable")
#cyclocomp_package("glpgUtilityFct")
#cyclocomp_package("clinDataReview")
#cyclocomp_package("packamon")