#' Common arguments for the plotting functions summary statistics of the clinDataReview package
#' @param vars Character vector with variables of \code{data}
#' containing the groups. If multiple, they should be specified in 
#' hierarchical order (from parent to child node).
#' @param varsLab Named character vector with labels for \code{vars}.
#' @param valueVar String with numeric variable of \code{data} 
#' containing the value to display.
#' @param valueLab String with label for the \code{valueVar} variable.
#' @param valueType String with type of values in \code{valueVar}
#' (\code{branchvalues} of the \code{\link[plotly]{plot_ly}}) function),
#' among others: 'total' (default, only if sum(child) <= to parent)
#' or 'relative'.
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
#' @param table Logical, if TRUE (FALSE by default)
#' returns also a \code{datatable} containing the plot data.
#' (The plot and the table are not linked.)
#' @name clinDataReview-common-args-summaryStatsVis
#' @return No return value, used for the documentation of 
#' the plotting functions of summary statistics of the package.
NULL
