#' Common arguments for the functions of the medicalMonitoring package
#' @param pathVar String with variable of \code{data} containing hyperlinks 
#' with path to the subject-specific report, formatted as: \cr
#' \preformatted{<a href="./path-to-report">label</a>}.\cr 
#' If multiple, they should be separated by: ', '.\cr
#' The report(s) will be:
#' \itemize{
#' \item{compressed to a zip file and downloaded 
#' if the user clicks on the 'p' (a.k.a 'profile') key
#' when hovering on a point of the plot}
#' \item{included in a collapsible row, and clickable with hyperlinks
#' in the table}
#' }
#' @param pathLab String with label for \code{pathVar},
#' included in the collapsible row in the table.
#' @name medicalMonitoring-common-args-summaryStatsVis
NULL
