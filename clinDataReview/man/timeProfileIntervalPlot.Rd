% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots-timeProfileIntervalPlot.R
\name{timeProfileIntervalPlot}
\alias{timeProfileIntervalPlot}
\title{Visualize time intervals across subjects/parameters.}
\usage{
timeProfileIntervalPlot(
  data,
  paramVar,
  paramLab = getLabelVar(paramVar, labelVars = labelVars),
  paramVarSep = " - ",
  paramGroupVar = NULL,
  timeStartVar,
  timeStartLab = getLabelVar(timeStartVar, labelVars = labelVars),
  timeEndVar,
  timeEndLab = getLabelVar(timeEndVar, labelVars = labelVars),
  timeStartShapeVar = NULL,
  timeStartShapeLab = getLabelVar(timeStartShapeVar, labelVars = labelVars),
  timeEndShapeVar = NULL,
  timeEndShapeLab = getLabelVar(timeEndShapeVar, labelVars = labelVars),
  shapePalette = NULL,
  colorVar = NULL,
  colorLab = getLabelVar(colorVar, labelVars = labelVars),
  colorPalette = NULL,
  alpha = 1,
  yLab = NULL,
  xLab = paste(c(timeStartLab, timeEndLab), collapse = " and "),
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  labelVars = NULL,
  width = 800,
  height = NULL,
  hoverVars,
  hoverLab,
  idVar = "USUBJID",
  idLab = getLabelVar(idVar, labelVars = labelVars),
  pathVar = NULL,
  pathLab = getLabelVar(pathVar, labelVars = labelVars),
  id = paste0("plotClinData", sample.int(n = 1000, size = 1)),
  selectVars = NULL,
  selectLab = getLabelVar(selectVars, labelVars = labelVars),
  table = FALSE,
  tableVars,
  tableLab,
  tableButton = TRUE,
  tablePars = list(),
  watermark = NULL,
  verbose = FALSE
)
}
\arguments{
\item{data}{Data.frame with data.}

\item{paramVar}{Character vector with variable of \code{data}
to represent in the y-axis.}

\item{paramLab}{(optional) String with label for \code{paramVar}.}

\item{paramVarSep}{(optional) String with separator used to combined \code{paramVar}
if multiple.}

\item{paramGroupVar}{(optional) Character vector with variable(s) to group/order
the \code{paramVar} elements in the y-axis.}

\item{timeStartVar}{String with variable with the start of the time interval.}

\item{timeStartLab}{(optional) String with label for \code{timeStartVar}.}

\item{timeEndVar}{String with variable with the end of the time interval.}

\item{timeEndLab}{(optional) String with label for \code{timeEndVar}.}

\item{timeStartShapeVar}{(optional) String with variable used for the shape
of the start of the time interval.}

\item{timeStartShapeLab}{(optional) String with label for \code{timeStartShapeVar}.}

\item{timeEndShapeVar}{(optional) String with variable used for the shape
of the end of the time interval.}

\item{timeEndShapeLab}{(optional) String with label for \code{timeEndShapeVar}.}

\item{shapePalette}{(optional) Character vector with shape palette for
\code{timeStartShapeVar} and \code{timeEndShapeVar}.}

\item{colorVar}{(optional) String with color variable.}

\item{colorLab}{String with label for \code{colorVar}.}

\item{colorPalette}{(optional) Named character vector with color palette.
If not specified, the viridis color palette is used.\cr
See \code{\link[clinUtils]{clinColors}}.}

\item{alpha}{(optional) Numeric with transparency, 1 by default.}

\item{xLab, yLab}{(optional) String with labels for the x/y-axis.}

\item{title}{String with title for the plot.}

\item{subtitle}{String with subtitle.\cr
The subtitle is included at the top left of the plot,
below the title.}

\item{caption}{String with caption. \cr
The caption is included at the bottom right of the plot.
Please note that this might overlap with 
vertical or rotated x-axis labels.}

\item{labelVars}{Named character vector containing variable labels.}

\item{width}{Numeric, width of the plot in pixels,
800 by default.}

\item{height}{Numeric, height of the plot in pixels,
500 by default.}

\item{hoverVars}{Character vector with variable(s) to be displayed in the hover,
by default any position (and axis) and aesthetic variables displayed in the plot.}

\item{hoverLab}{Named character vector with labels for \code{hoverVars}.}

\item{idVar}{String with variable containing subject ID.}

\item{idLab}{String with label for \code{idVar}.}

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

\item{id}{String with general id for the plot:
\itemize{
\item 'id' is used as \code{group} for the \code{\link[crosstalk]{SharedData}}
\item 'button:[id]' is used as button ID if \code{table} is TRUE
}
If not specified, a random id, as 'plotClinData[X]' is used.}

\item{selectVars}{(optional) Character vector with variable(s) from \code{data}
for which a selection box should be included. This enables to select the data
displayed in the plot (and associated table).}

\item{selectLab}{(Named) character vector with label for \code{selectVars}.}

\item{table}{Logical, if TRUE (FALSE by default)
returns also a \code{datatable} containing the plot data.
The plot and table are linked when included in a Rmarkdown document: 
when clicking on an plot element,
only the corresponding records are retained in the associated table;
when some records are selected in the table, they are highlighted in the 
associated table.}

\item{tableVars}{Character vector with variables to be included 
in the table.}

\item{tableLab}{Named character vector with labels
for each \code{tableVars}.}

\item{tableButton}{Logical, if TRUE (by default)
the table is included within an HTML button.}

\item{tablePars}{List with parameters passed to the
\code{\link[clinUtils]{getClinDT}} function.}

\item{watermark}{(optional) String with path to a file containing a watermark.}

\item{verbose}{Logical, if TRUE (FALSE by default) progress messages are printed
in the current console.
For the visualizations, progress messages during download
of subject-specific report are displayed in the browser console.}
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
Visualize time intervals across subjects/parameters.
}
\examples{
library(clinUtils)

data(dataADaMCDISCP01)
labelVars <- attr(dataADaMCDISCP01, "labelVars")

dataAE <- dataADaMCDISCP01$ADAE

# basic plot
timeProfileIntervalPlot(
	data = dataAE,
	paramVar = "USUBJID",
	# time-variables
	timeStartVar = "ASTDY",
	timeEndVar = "ASTDY",
	# colored by severity
	colorVar = "AESEV",
	labelVars = labelVars
)

# add caption & subtitle
timeProfileIntervalPlot(
	data = dataAE,
	paramVar = "USUBJID",
	timeStartVar = "ASTDY",
	timeEndVar = "ASTDY",
	colorVar = "AESEV",
	labelVars = labelVars,
	title = "Adverse events",
	subtitle = "Time intervals",
	caption = "Day is relative to the study baseline"
)

# add a selection box
if(interactive()){
  timeProfileIntervalPlot(
    data = dataAE,
    paramVar = "USUBJID",
    # time-variables
    timeStartVar = "ASTDY",
    timeEndVar = "ASTDY",
    # colored by severity
    colorVar = "AESEV",
    labelVars = labelVars,
    selectVars = "AEDECOD"
  )
}
}
\seealso{
Other Clinical data visualization of individual profiles.: 
\code{\link{scatterplotClinData}()}
}
\author{
Laure Cougnaud
}
\concept{Clinical data visualization of individual profiles.}
