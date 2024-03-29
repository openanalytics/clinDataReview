% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots-utility-getPlotTableVars.R
\name{getPlotTableVars}
\alias{getPlotTableVars}
\title{Extract variables displayed in the attached table, for
each available plotting function of the clinDataReview package.}
\usage{
getPlotTableVars(plotFunction, plotArgs)
}
\arguments{
\item{plotFunction}{String with name of the plotting function,
be available in the \code{clinDataReview package}.}

\item{plotArgs}{List with parameters passed to the plotting function.}
}
\value{
Character vector with variable to include in the table,
with extra attributes (passed to \code{\link{tableClinData}}): 
\itemize{
\item 'tableLab': Named character vector with labels 
for the table variables
\item 'tablePars' : extra table parameters, only included if specified as 
input or specified internally.
}
labels and the table parameters .
}
\description{
This function is used in each plotting function of the package 
to extract the variable(s) displayed in the table associated to the
plot and their associated labels.\cr
This can also be used in the template reports, e.g. to extract
reference variable(s) for the comparison table functionality
in the plot template report.\cr
The following framework is used:
\itemize{
\item if variables to be displayed in the table (\code{tableVars})
are not specified:\cr all variables displayed in the plot
are selected, based on the plot arguments.\cr
 For example: the variables
displayed in the x and y axis and for coloring are extracted
for the \code{\link{scatterplotClinData}} plotting function.\cr
Label for these variable(s) are extracted from the associated parameter 
(e.g. \code{xLab} for \code{xVar} and so on) or the general
parameter for the variable labels (\code{labelVars}) if not specified.
\item if variables to be displayed in the table (\code{tableVars})
are specified:\cr these variable(s) are returned.\cr
The associated label(s) are extracted from the associated 
parameter (\code{tableLab}) or the general
parameter for the variable labels (\code{labelVars}) if not specified.
}
For the functions: \code{\link{plotCountClinData}}, 
\code{\link{treemapClinData}}, \code{\link{sunburstClinData}}:
value to represent are included in the table and colored with a bar.
}
\author{
Laure Cougnaud
}
