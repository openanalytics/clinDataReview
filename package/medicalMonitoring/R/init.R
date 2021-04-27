#' @importFrom clinUtils clinColors clinShapes clinLinetypes
.onAttach <- function(libname, pkgname) {
	options(medicalMonitoring.colors = clinColors)
	options(medicalMonitoring.shapes = clinShapes)
	options(medicalMonitoring.linetypes = clinLinetypes)
}