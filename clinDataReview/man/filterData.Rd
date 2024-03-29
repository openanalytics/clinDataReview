% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataManipulation-filterData.R
\name{filterData}
\alias{filterData}
\title{Filter a dataset based on specified filters.}
\usage{
filterData(
  data,
  filters,
  keepNA = TRUE,
  returnAll = FALSE,
  verbose = FALSE,
  labelVars = NULL,
  labelData = "data"
)
}
\arguments{
\item{data}{Data.frame with data.}

\item{filters}{Unique filter or list of filters.\cr
Each filter is a list containing:
\itemize{
\item 'var': String with variable from \code{data} to filter on.
\item 'value': (optional) Character vector with values from \code{var} 
\strong{to consider/keep}.
\item 'valueFct': (optional) Function (or string with this function)
to be applied on \code{var} to extract value to consider.\cr
For example, \code{valueFct = max} will extract the records
with the maximum value of the variable.
\item 'op': (optional) String with operator used to retain records 
from \code{value}.
If not specified, the inclusion operator: '\%in\%' is considered, so
records with \code{var} in \code{value} are retained.
\item 'rev': (optional) Logical, if TRUE (FALSE by default), 
filtering condition based on \code{value}/\code{valueFct} is reversed.
\item 'keepNA': (optional) Logical, if TRUE (by default), 
missing values in \code{var} are retained.\cr
If not specified, \code{keepNA} general parameter is used.
\item 'varsBy': (optional) Character vector with variables in 
\code{data} containing groups to filter by
\item 'postFct': (optional) Function (or string with this function) with
post-processing applied on the results of the filtering criteria 
(TRUE/FALSE for each record). This function should return TRUE/FALSE
(for each record or for all considered records).\cr
For example, '\code{postFct = any, varsBy = "group"}' retains all groups
which contain at least one record that fulfills the criteria.
\item 'varNew': (optional) String with name of a new variable containing
the results of the filtering criteria (as TRUE/FALSE).
\item 'labelNew': (optional) String with label for the \code{varNew} variable.
}
If a list of filters is specified, the different filters are \strong{independently
executed on the entire dataset to identify the records to retain for
each filtering condition.}\cr
The resulting selections are combined 
with a \code{\link[base]{Logic}} operator ('&' by default, i.e. 'AND' condition).
A custom logic operator can be specified between the lists describing the filter, 
for example:\cr
\code{list(list(var = "SEX", value = "F"), "&", 
list(var = "COUNTRY", value = "DEU"))}.}

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

\item{verbose}{Logical, if TRUE (FALSE by default) progress messages are printed
in the current console.
For the visualizations, progress messages during download
of subject-specific report are displayed in the browser console.}

\item{labelVars}{Named character vector containing variable labels.}

\item{labelData}{(optional) String with label for input \code{data},
that will be included in progress messages.}
}
\value{
If \code{returnAll}
\itemize{
\item is \code{FALSE}: \code{data} filtered with the specified filters
\item is \code{TRUE}: \code{data} with the additional column: \code{keep} 
 or \code{varNew} (if specified), containing \code{TRUE} for records 
 which fulfill the specified condition(s) and \code{FALSE} otherwise.
}
The output contains the additional attribute: \code{msg} which contains a message
describing the filtered records.
}
\description{
A dataset can be filtered:
\itemize{
\item on a specific \code{value} of interest
\item on a function of a variable (\code{valueFct} parameter), 
e.g. maximum of the variable)
\item to retain only non missing values of a variable (\code{keepNA} 
set to \code{FALSE})
\item by groups (\code{varsBy} parameter)
}
\strong{Note that by default, missing values in the filtering variable are retained
(which differs from the default behaviour in R).}
To filter missing records, please set the \code{keepNA} parameter to \code{FALSE}.
}
\examples{
library(clinUtils)

data(dataADaMCDISCP01)
labelVars <- attr(dataADaMCDISCP01, "labelVars")

dataDM <- dataADaMCDISCP01$ADSL

## single filter

# filter with inclusion criteria:
filterData(
	data = dataDM, 
	filters = list(var = "SEX", value = "M"),
	# optional
	labelVars = labelVars, verbose = TRUE
)

# filter with non-inclusion criteria
filterData(
	data = dataDM, 
	filters = list(var = "SEX", value = "M", rev = TRUE), 
	# optional
	labelVars = labelVars, verbose = TRUE
)

# filter based on inequality operator
filterData(
	data = dataDM, 
	filters = list(var = "AGE", value = 75, op = "<="), 
	# optional
	labelVars = labelVars, verbose = TRUE
)

# missing values are retained by default!
dataDMNA <- dataDM
dataDMNA[1 : 2, "AGE"] <- NA
filterData(
	data = dataDMNA, 
	filters = list(var = "AGE", value = 75, op = "<="), 
	# optional
	labelVars = labelVars, verbose = TRUE
)

# filter missing values on variable
filterData(
	data = dataDMNA, 
	filters = list(var = "AGE", value = 75, op = "<=", keepNA = FALSE), 
	# optional
	labelVars = labelVars, verbose = TRUE
)

# retain only missing values
filterData(
	data = dataDMNA, 
	filters = list(var = "AGE", value = NA, keepNA = TRUE), 
	# optional
	labelVars = labelVars, verbose = TRUE
)

# filter missing values
filterData(
	data = dataDMNA, 
	filters = list(var = "AGE", keepNA = FALSE), 
	# optional
	labelVars = labelVars, verbose = TRUE
)


## multiple filters

# by default the records fulfilling all conditions are retained ('AND')
filterData(
	data = dataDM, 
	filters = list(
		list(var = "AGE", value = 75, op = "<="),
		list(var = "SEX", value = "M")
	), 
	# optional
	labelVars = labelVars, verbose = TRUE
)

# custom operator:
filterData(
	data = dataDM, 
	filters = list(
		list(var = "AGE", value = 75, op = "<="),
		"|",
		list(var = "SEX", value = "M")
	), 
	# optional
	labelVars = labelVars, verbose = TRUE
)

# filter by group

# only retain adverse event records with worst-case severity
dataAE <- dataADaMCDISCP01$ADAE
dataAE$AESEV <- factor(dataAE$AESEV, levels = c("MILD", "MODERATE", "SEVERE"))
dataAE$AESEVN <- as.numeric(dataAE$AESEV)
nrow(dataAE)
dataAEWorst <- filterData(
	data = dataAE,
	filters = list(
		var = "AESEVN",		
		valueFct = max,
		varsBy = c("USUBJID", "AEDECOD"),
		keepNA = FALSE
	),
	# optional
	labelVars = labelVars, verbose = TRUE
)
nrow(dataAEWorst)

# post-processing function
# keep subjects with at least one severe AE:
dataSubjectWithSevereAE <- filterData(
  data = dataAE,
  filters = list(
    var = "AESEV",		
    value = "SEVERE",
    varsBy = "USUBJID",
    postFct = any
  ),
  # optional
  labelVars = labelVars, verbose = TRUE
)

# for each laboratory parameter: keep only subjects which have at least one
# measurement classified as low or high
dataLB <- subset(dataADaMCDISCP01$ADLBC, !grepl("change", PARAM))
dataLBFiltered <- filterData(
  data = dataLB,
  filters = list(
    var = "LBNRIND",		
    value = c("LOW", "HIGH"),
    varsBy = c("PARAMCD", "USUBJID"),
    postFct = any
  ),
  # optional
  labelVars = labelVars, verbose = TRUE
)
}
\author{
Laure Cougnaud
}
