% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots-sunburstClinData.R
\name{sunburstClinData}
\alias{sunburstClinData}
\title{Sunburst interactive plot.}
\usage{
sunburstClinData(...)
}
\arguments{
\item{...}{
  Arguments passed on to \code{\link[=plotCountClinData]{plotCountClinData}}
  \describe{
    \item{\code{colorVar}}{(optional) String with coloring variable
(NULL by default).
By default, the treemap is colored based by section.}
    \item{\code{colorRange}}{(optional) Numeric vector of length 2 with range 
for the color variable, in case it is a numeric variable.}
    \item{\code{vars}}{Character vector with variables of \code{data}
containing the groups. If multiple, they should be specified in 
hierarchical order (from parent to child node).}
    \item{\code{varsLab}}{Named character vector with labels for \code{vars}.}
    \item{\code{valueVar}}{String with numeric variable of \code{data} 
containing the value to display.}
    \item{\code{valueLab}}{String with label for the \code{valueVar} variable.}
    \item{\code{valueType}}{String with type of values in \code{valueVar}
(\code{branchvalues} of the \code{\link[plotly]{plot_ly}}) function),
among others: 'total' (default, only if sum(child) <= to parent)
or 'relative'.}
    \item{\code{pathVar}}{String with variable of \code{data} containing hyperlinks 
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
    \item{\code{pathLab}}{String with label for \code{pathVar},
included in the collapsible row in the table.}
    \item{\code{table}}{Logical, if TRUE (FALSE by default)
returns also a \code{datatable} containing the plot data.
(The plot and the table are not linked.)}
    \item{\code{data}}{Data.frame with data.}
    \item{\code{verbose}}{Logical, if TRUE (FALSE by default) progress messages are printed
in the current console.
For the visualizations, progress messages during download
of subject-specific report are displayed in the browser console.}
    \item{\code{width}}{Numeric, width of the plot in pixels,
800 by default.}
    \item{\code{height}}{Numeric, height of the plot in pixels,
500 by default.}
    \item{\code{hoverVars}}{Character vector with variable(s) to be displayed in the hover,
by default any position (and axis) and aesthetic variables displayed in the plot.}
    \item{\code{hoverLab}}{Named character vector with labels for \code{hoverVars}.}
    \item{\code{labelVars}}{Named character vector containing variable labels.}
    \item{\code{id}}{String with general id for the plot:
\itemize{
\item 'id' is used as \code{group} for the \code{\link[crosstalk]{SharedData}}
\item 'button:[id]' is used as button ID if \code{table} is TRUE
}
If not specified, a random id, as 'plotClinData[X]' is used.}
    \item{\code{title}}{String with title for the plot.}
    \item{\code{titleExtra}}{String with extra title for the plot (appended after \code{title}).}
    \item{\code{caption}}{String with caption. \cr
The caption is included at the bottom right of the plot.
Please note that this might overlap with 
vertical or rotated x-axis labels.}
    \item{\code{subtitle}}{String with subtitle.\cr
The subtitle is included at the top left of the plot,
below the title.}
    \item{\code{colorLab}}{String with label for \code{colorVar}.}
    \item{\code{colorPalette}}{(optional) Named character vector with color palette.
If not specified, the viridis color palette is used.\cr
See \code{\link[clinUtils]{clinColors}}.}
    \item{\code{watermark}}{(optional) String with path to a file containing a watermark.}
    \item{\code{tableButton}}{Logical, if TRUE (by default)
the table is included within an HTML button.}
    \item{\code{tableVars}}{Character vector with variables to be included 
in the table.}
    \item{\code{tableLab}}{Named character vector with labels
for each \code{tableVars}.}
    \item{\code{tablePars}}{List with parameters passed to the
\code{\link[clinUtils]{getClinDT}} function.}
  }}
}
\value{
Either:
\itemize{
\item if a \code{table} is requested: a \code{clinDataReview} object, 
a.k.a a list with the 'plot' (\code{\link[plotly]{plotly}} object) and 'table'
(\code{\link[DT]{datatable}} object)
\item otherwise: a \code{\link[plotly]{plotly}} object
}
}
\description{
Note: the table and plot are not (yet) linked.
}
\examples{
library(clinUtils)

data(dataADaMCDISCP01)
labelVars <- attr(dataADaMCDISCP01, "labelVars")

dataAE <- dataADaMCDISCP01$ADAE
dataDM <- dataADaMCDISCP01$ADSL

## example of basic sunburst:

# sunburst takes as input table with counts
if (requireNamespace("inTextSummaryTable", quietly = TRUE)) {

# total counts: Safety Analysis Set (patients with start date for the first treatment)
dataTotal <- subset(dataDM, RFSTDTC != "")

# compute adverse event table
tableAE <- inTextSummaryTable::getSummaryStatisticsTable(
		
	data = dataAE,
	rowVar = c("AESOC", "AEDECOD"),
	dataTotal = dataTotal,
	rowOrder = "total",
	labelVars = labelVars,
	stats = inTextSummaryTable::getStats("count"),
	
	# plotly treemap requires records (rows) for each group
	rowVarTotalInclude = "AEDECOD",
	outputType = "data.frame-base"

)

dataSunburst <- tableAE

dataSunburst$n <- as.numeric(dataSunburst$n)

# create plot
sunburstClinData(
	data = dataSunburst,
	vars = c("AESOC", "AEDECOD"),
	valueVar = "n",
    valueLab = "Number of patients with adverse events"
)

## example where sum(counts) of child = counts of parent

# counts of patients per arm/site
tableDM <- inTextSummaryTable::getSummaryStatisticsTable(
	data = dataDM,
	rowVar = c("ARM", "SITEID"),
	labelVars = labelVars,
	# plotly treemap requires records (rows) for each group
	rowVarTotalInclude = "SITEID",
	rowTotalInclude = TRUE,
	outputType = "data.frame-base"
)
tableDM$statN <- as.numeric(tableDM$statN)

# create the plot
sunburstClinData(
	data = tableDM,
	vars = c("ARM", "SITEID"),
	valueVar = "statN", valueLab = "Counts of patients",
	valueType = "total",
	caption = "The sectors are colored by category.",
	subtitle = "Group: treatment and site"
)

}
}
\seealso{
Other visualizations of summary statistics for clinical data: 
\code{\link{barplotClinData}()},
\code{\link{boxplotClinData}()},
\code{\link{errorbarClinData}()},
\code{\link{plotCountClinData}()},
\code{\link{treemapClinData}()}
}
\author{
Laure Cougnaud
}
\concept{visualizations of summary statistics for clinical data}
