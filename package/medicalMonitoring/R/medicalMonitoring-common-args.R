#' Common arguments for the functions of the medicalMonitoring package
#' @param data Data.frame with data.
#' @param gg \code{\link[ggplot2]{ggplot}} object.
#' @param xVar String with column of \code{data} containing x-variable.
#' @param yVar String with column of \code{data} containing y-variable.
#' @param xLim,yLim Numeric vector of length 2 with limits for the x/y axes.
#' @param idVar String with variable containing subject ID.
#' @param facetPars List with facetting parameters, passed to the facetting function.
#' @param lineVars List with parameters for the reference lines.
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
