% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots-utility-referenceLines.R
\name{getFctTypeReferenceLines}
\alias{getFctTypeReferenceLines}
\title{Get the names of the \code{\link[ggplot2]{ggplot}} function
to use for the reference lines}
\usage{
getFctTypeReferenceLines(refLinePars)
}
\arguments{
\item{refLinePars}{(optional) Nested list, with parameters for each reference line(s).
Each sublist (a.k.a reference line) contains:
\itemize{
\item aesthetic value(s) or variable(s) for the lines
(in this case column names of \code{data}) for reference lines.
The line position is controlled by the aesthetics supported in
\code{\link[ggplot2]{geom_vline}}, \code{\link[ggplot2]{geom_hline}} 
and \code{\link[ggplot2]{geom_abline}}.
\item 'label': (optional) Logical specifying if the line
should be annotated (\code{FALSE} to not annotate the line)
or string with annotation label. By default, the value
of the position of the horizontal/vertical line or the equation
of the diagonal line is displayed.
}}
}
\value{
List of type of each reference lines, among:
'vline', 'hline' and 'abline'.
}
\description{
Get the names of the \code{\link[ggplot2]{ggplot}} function
to use for the reference lines
}
\author{
Laure Cougnaud
}
