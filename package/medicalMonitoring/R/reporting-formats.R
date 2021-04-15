#' Medical monitoring format for bookdown report.
#' 
#' This function is only meant to set sensitive
#' defaults for gitbook.\cr
#' \code{\link[bookdown]{gitbook}} can be used instead.
#' @param split_by String, how the reports should be split,
#' (see help of the \link[bookdown]{gitbook} function)
#' @param config List with config parameters,
#' by default: no sharing and collapsed by section.
#' (see help of the \link[bookdown]{gitbook} function)
#' @param extra_dependencies NULL by default
#' @param ... Extra parameters passed to the
#' \link[glpgStyle]{gitbook_report} function.
#' @return R Markdown output format to pass to \code{\link[bookdown]{render_book}}.
#' @importFrom bookdown gitbook
#' @author Laure Cougnaud
#' @family medical monitoring reporting
#' @export
gitbook_medicalMonitoring_report <- function(
	split_by = 'section+number',
	config = list(
		sharing = NULL, 
		toc = list(collapse = 'section')
	), 
	extra_dependencies = NULL,
	...){

	bookdown::gitbook(
		...,
		split_by = split_by,
		config = config,
		extra_dependencies = extra_dependencies
	)
	
}


#' Medical monitoring format for rmarkdown report.
#' 
#' This function only kept for back-compatibility, 
#' \code{\link[rmarkdown]{html_document}}
#' can be used instead.
#' @param extra_dependencies NULL by default.
#' @param ... Extra parameters passed to the
#' \link[glpgStyle]{html_report} function.
#' @return R Markdown output format to pass to \code{\link[rmarkdown]{render}}.
#' @author Laure Cougnaud
#' @family medical monitoring reporting
#' @importFrom rmarkdown html_document
#' @export
html_medicalMonitoring_report <- function(
	extra_dependencies = NULL,
	...){
	
	rmarkdown::html_document(
		...,
		extra_dependencies = extra_dependencies
	)
	
}


