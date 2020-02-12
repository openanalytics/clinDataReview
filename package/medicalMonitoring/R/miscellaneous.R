#' Format hover text for use in plotly interactive plots.
#' The labels are wrapped to multiple lines if exceed the width of the plotly
#' hover box, e.g. in case labels for points with same x/y coordinates overlap,
#' and corresponding labels are truncated.
#' @param x Vector with hover text information.
#' @param label Label for the variable
#' @param width Integer, number of characters at 
#' which the hover text should be cut at to multiple lines.
#' @return String with formatted hover label.
#' @author Laure Cougnaud
#' @export
formatHoverText <- function(x, label, width = 50){
	formatLongLabel <- function(x)
		vapply(x, function(x1)
			xRF <- paste(strwrap(x1, width = width), collapse = "<br>"),
			FUN.VALUE = character(1)
		)
	formatLongLabel(
		paste0(
			label, ": ",
			paste(unique(as.character(x)), collapse = ", ")
		)
	)
}