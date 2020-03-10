#' Common arguments for the functions of the medicalMonitoring package
#' @param data Data.frame with data.
#' @param gg \code{\link[ggplot2]{ggplot}} object.
#' @param xVar String with column of \code{data} containing x-variable.
#' @param yVar String with column of \code{data} containing y-variable.
#' @param xLim,yLim Numeric vector of length 2 with limits for the x/y axes.
#' @param idVar String with variable containing subject ID.
#' @param idLab String with label for \code{idVar}.
#' @param facetPars List with facetting parameters, passed to the facetting function.
#' @param lineVars List with parameters for the reference lines.
#' @param hoverVars Character vector with variable(s) to be displayed in the hover,
#' by default any position and aesthetic variables displayed in the plot.
#' @param hoverLab Named character vector with labels for \code{hoverVars}.
#' @param pathExpand Logical, should the variable in \code{pathExpand}
#' be included in a collapsible row or as hyperlink in the table?
#' Should be TRUE for if multiple paths are included for each \code{idVar},
#' FALSE otherwise (by default).
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
#' @param labelVars Named character vector containing variable labels,
#' used by default for all labels in the plot.
#' @param id String with general id for the plot:
#' \itemize{
#' \item{'id' is used as \code{group} for the \code{\link[crosstalk]{SharedData}}}
#' \item{'button:[id]' is used as button ID if \code{table} is TRUE}
#' }
#' If not specified, a random id, as 'plotMonitoring[X]' is used.
#' @param title String with title for the plot.
#' @param titleExtra String with extra title for the plot (appended after \code{title}).
#' @param width,height Width/height of the plot in pixels.
#' @name medicalMonitoring-common-args
NULL
