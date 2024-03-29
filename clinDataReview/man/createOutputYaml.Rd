% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reporting-render-html.R
\name{createOutputYaml}
\alias{createOutputYaml}
\title{Create a output YAML file}
\usage{
createOutputYaml(indexPath, outputDir)
}
\arguments{
\item{indexPath}{String with path to the index file,
by default 'index.Rmd' in \code{inputDir}.}

\item{outputDir}{String with output directory,
('report' by default).}
}
\value{
String with file to the \code{_output.yml} file
in a temporary folder.
}
\description{
This file containing the contents of the
\code{output} field of the YAML header of a Markdown file.
It can be passed to the \code{output_yaml} parameter
of the \code{\link[rmarkdown]{render}} function.
}
