% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots-utility-formatToHierarchicalData.R
\name{formatToHierarchicalData}
\alias{formatToHierarchicalData}
\title{Format data to a hierarchical data, in the format
as required by the plotly sunburst and treemap.}
\usage{
formatToHierarchicalData(data, vars, valueVar)
}
\arguments{
\item{data}{Data.frame with data.}

\item{vars}{Character vector with variables of \code{data}
containing the groups. If multiple, they should be specified in 
hierarchical order (from parent to child node).}

\item{valueVar}{String with numeric variable of \code{data} 
containing the value to display.}
}
\value{
Updated data.frame with \code{vars} in 
hierarchical format, with extra attributes (in 'metadat'):
\itemize{
\item 'varID':  String with column of output
containing ID of specific element.\cr
This is a combination from the specified \code{vars},
or 'Overall' for the grand total.
\item 'varParent': String with column of output
containing ID of the parent element
\item 'varLabel': String with column of output
containing the label to display.\cr
This is usually the name of the child element.
}
}
\description{
Note that new variables are created for each variable of interest
(the variables are not overwritten) to avoid
issues with cases where the value in the child 
and parent variables are the same.
}
\author{
Laure Cougnaud
}
