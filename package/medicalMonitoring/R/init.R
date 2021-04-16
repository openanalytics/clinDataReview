#' @importFrom clinUtils clinColors clinShapes
.onAttach <- function(libname, pkgname) {
	options(medicalMonitoring.colors = clinColors)
	options(medicalMonitoring.shapes = clinShapes)
}