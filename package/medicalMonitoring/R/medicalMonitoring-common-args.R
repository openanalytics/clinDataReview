#' Common arguments for the functions of the medicalMonitoring package
#' @param data Data.frame with data.
#' @param gg \code{\link[ggplot2]{ggplot}} object.
#' @param xVar String with column of \code{data} containing x-variable.
#' @param yVar String with column of \code{data} containing y-variable.
#' @param xLim,yLim Numeric vector of length 2 with limits for the x/y axes.
#' @param idVar String with variable containing subject ID.
#' @param facetPars List with facetting parameters, passed to the facetting function.
#' @param lineVars List with parameters for the reference lines.
#' @param width,height Width/height of the plot in pixels.
#' @param hoverVar Character vector with variables to be displayed in the hover,
#' by default \code{xVar}, \code{yVar} and any aesthetic variables.
#' @param hoverLab Named character vector with labels for \code{hoverVar}.
#' @param pathVar String with variable of \code{data} containing path
#' to a subject-specific report (e.g. patient profiles).
#' The report should be unique by element of \code{idVar}.
#' This report will be:
#' \itemize{
#' \item{downloaded if the user clicks on the 'Ctrl'+'Enter' key
#' when hovering on a point of the plot}
#' \item{opened in a brower via hyperlink in the \code{idVar} of the table 
#' column (if specified via \code{tableVars})}
#' }
#' @param table Logical, if TRUE (FALSE by default)
#' returns also a \code{datatable} containing the plot data.
#' @param refLinePars (optional) Nested list, with parameters for each reference line(s).
#' Each sublist (a.k.a reference line) contains:
#' \itemize{
#' \item{aesthetic value(s) or variable(s) for the lines
#' (in this case column names of \code{data}) for reference lines.
#' The line position is controlled by the aesthetics supported in
#' \code{\link[ggplot2]{geom_vline}}, \code{\link[ggplot2]{geom_hline}} 
#' and \code{\link[ggplot2]{geom_abline}}.
#' }
#' \item{'label': }{(optional) Logical specifying if the line
#' should be annotated (\code{FALSE} to not annotate the line)
#' or string with annotation label. By default, the value
#' of the position of the horizontal/vertical line or the equation
#' of the diagonal line is displayed.
#' }
#' }
#' @name medicalMonitoring-common-args
NULL
