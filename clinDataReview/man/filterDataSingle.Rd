% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataManipulation-filterData.R
\name{filterDataSingle}
\alias{filterDataSingle}
\title{Filter data for a single filter}
\usage{
filterDataSingle(
  data,
  filters,
  keepNA = TRUE,
  returnAll = FALSE,
  labelVars = NULL,
  labelData = "data"
)
}
\arguments{
\item{data}{Data.frame with data.}

\item{filters}{Unique filter or list of filters.}

\item{keepNA}{Logical, if TRUE (by default) missing values in \code{var} are retained.
If set to FALSE, missing values are ignored for all filters.
The specification within \code{filters} prevails on this parameter.}

\item{returnAll}{Logical:
\itemize{
\item if FALSE (by default): the \code{data} for only the filtered records
is returned.
\item if TRUE: the full \code{data} is returned. 
Records are flagged based on the \code{filters} condition, in a new column:
\code{varNew} (if specified), or 'keep' otherwise; containing TRUE
 if the record fulfill all conditions, FALSE otherwise
}}

\item{labelVars}{Named character vector containing variable labels.}

\item{labelData}{(optional) String with label for input \code{data},
that will be included in progress messages.}
}
\value{
Updated \code{data} with attributes:
\itemize{
\item 'labelVars': input \code{labelVars} with any new variables 
 if \code{labelNew} is specified.
\item 'msg': message describing the filtering process
\item 'warn': warning describing the filtering process
}
}
\description{
Filter data for a single filter
}
\author{
Laure Cougnaud
}
