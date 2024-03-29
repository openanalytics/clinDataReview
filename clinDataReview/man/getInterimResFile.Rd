% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reporting-render-utility.R
\name{getInterimResFile}
\alias{getInterimResFile}
\title{Get interim res file}
\usage{
getInterimResFile(intermediateDir = "./interim", mdFile)
}
\arguments{
\item{intermediateDir}{String with intermediate directory ('interim'
by default), where
markdown files and rds file specifying Js libraries (with \code{knit_meta}) for
each sub report are stored.}

\item{mdFile}{String with path of the Markdown file}
}
\value{
String with path to the file with intermediate results.
}
\description{
Get interim res file
}
