% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/doc-clinDataReview-common-args-summaryStatsVis.R
\name{clinDataReview-common-args-summaryStatsVis}
\alias{clinDataReview-common-args-summaryStatsVis}
\title{Common arguments for the plotting functions summary statistics of the clinDataReview package}
\arguments{
\item{vars}{Character vector with variables of \code{data}
containing the groups. If multiple, they should be specified in 
hierarchical order (from parent to child node).}

\item{varsLab}{Named character vector with labels for \code{vars}.}

\item{valueVar}{String with numeric variable of \code{data} 
containing the value to display.}

\item{valueLab}{String with label for the \code{valueVar} variable.}

\item{valueType}{String with type of values in \code{valueVar}
(\code{branchvalues} of the \code{\link[plotly]{plot_ly}}) function),
among others: 'total' (default, only if sum(child) <= to parent)
or 'relative'.}

\item{pathVar}{String with variable of \code{data} containing hyperlinks 
with path to the subject-specific report, formatted as: \cr
\preformatted{<a href="./path-to-report">label</a>}.\cr 
If multiple, they should be separated by: ', '.\cr
The report(s) will be:
\itemize{
\item compressed to a zip file and downloaded 
if the user clicks on the 'p' (a.k.a 'profile') key
when hovering on a point of the plot
\item included in a collapsible row, and clickable with hyperlinks
in the table
}}

\item{pathLab}{String with label for \code{pathVar},
included in the collapsible row in the table.}

\item{table}{Logical, if TRUE (FALSE by default)
returns also a \code{datatable} containing the plot data.
(The plot and the table are not linked.)}
}
\value{
No return value, used for the documentation of 
the plotting functions of summary statistics of the package.
}
\description{
Common arguments for the plotting functions summary statistics of the clinDataReview package
}
