library(covr)
pc <- package_coverage(
    path = "~/git/GLPGMedicalMonitoring/package/medicalMonitoring",
    type = "tests", quiet = FALSE, clean = FALSE
)
report(x = pc,
    file = paste0("testCoverage-tests-medicalMonitoring",
        packageVersion("medicalMonitoring"), ".html")
)


#cyclocomp(function(arg) {
#      calulate(this); and(that) }
#)
#
#cyclocomp_package("glpgStyle")
#cyclocomp_package("inTextSummaryTable")
#cyclocomp_package("glpgUtilityFct")
#cyclocomp_package("medicalMonitoring")
#cyclocomp_package("packamon")