# Run tests and check tests coverage for the 'clinDataReview' package
# 
# Author: Laure Cougnaud
###############################################################################

packagePath <- "../clinDataReview/"

## create reference figures for vdiffr::expect_doppelganger

library(vdiffr)

# The same collate statement should be used than the R CMD check
# at the creation of the reference figures with 'manage_cases'
# this affect the order of the elements with 'reorder'
# if different, the order of the parameters in the y-axis of the plot might differ
Sys.setlocale(category = "LC_COLLATE", locale = "C")

# create reference figures in 'tests/fig' package
validate_cases(collect_cases(package = packagePath))

## create the package
library(devtools)
pkgTarballPath <- build(pkg = packagePath, args = "--resave-data")

## check the package
checkDir <- normalizePath(dirname(packagePath))
check_built(
	path = pkgTarballPath, 
	args = paste0("--output=", checkDir)
)

## check the package coverage:

library(covr)

# test coverage: 
pc <- package_coverage(
	path = packagePath, 
	type = c("tests", "vignettes", "examples"), 
#	function_exclusions = "^(?!subjectProfile.*Plot)", 
	combine_types = TRUE # report coverage for each type
)
report(x = pc, file = "testCoverage-clinDataReview.html")

