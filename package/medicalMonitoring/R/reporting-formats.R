#' GLPG-theme medical monitoring format for bookdown report.
#' 
#' This format includes also additional javascript libraries
#' required for visualization/tables of a medical monitoring
#' report.
#' @param split_by String, how the reports should be split,
#' (see help of the \link[bookdown]{gitbook} function)
#' @param config List with config parameters,
#' by default: no sharing and collapsed by section.
#' (see help of the \link[bookdown]{gitbook} function)
#' @param extra_dependencies List of \code{\link[htmltools]{htmlDependency}}
#' by default extracted from \code{\link{getJsDepMedicalMonitoring}}.
#' (see help of the \link[rmarkdown]{html_document} function)
#' @param ... Extra parameters passed to the
#' \link[glpgStyle]{gitbook_report} function.
#' @return R Markdown output format to pass to \code{\link[bookdown]{render_book}}.
#' @importFrom glpgStyle gitbook_report
#' @author Laure Cougnaud
#' @family medical monitoring reporting
#' @export
gitbook_medicalMonitoring_report <- function(
	split_by = 'section+number',
	config = list(
		sharing = NULL, 
		toc = list(collapse = 'section')
	), 
	extra_dependencies = getJsDepMedicalMonitoring(),
	...){

	glpgStyle::gitbook_report(
		...,
		split_by = split_by,
		config = config,
		extra_dependencies = extra_dependencies
	)
	
}


#' GLPG-theme medical monitoring format for rmarkdown report.
#' 
#' This format includes also additional javascript libraries
#' required for visualization/tables of a medical monitoring
#' report.
#' @param extra_dependencies List of \code{\link[htmltools]{htmlDependency}}
#' by default extracted from \code{\link{getJsDepMedicalMonitoring}}
#' (see help of the \link[rmarkdown]{html_document} function)
#' @param ... Extra parameters passed to the
#' \link[glpgStyle]{html_report} function.
#' @return R Markdown output format to pass to \code{\link[rmarkdown]{render}}.
#' @importFrom glpgStyle gitbook_report
#' @author Laure Cougnaud
#' @family medical monitoring reporting
#' @export
html_medicalMonitoring_report <- function(
	extra_dependencies = getJsDepMedicalMonitoring(),
	...){
	
	glpgStyle::html_report(
		...,
		extra_dependencies = extra_dependencies
	)
	
}


