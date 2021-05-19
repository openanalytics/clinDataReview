#' @importFrom clinUtils clinColors clinShapes clinLinetypes
.onAttach <- function(libname, pkgname) {
	options(clinDataReview.colors = clinColors)
	options(clinDataReview.shapes = clinShapes)
	options(clinDataReview.linetypes = clinLinetypes)
}