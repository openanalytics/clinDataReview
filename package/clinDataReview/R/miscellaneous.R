#' Create link to patient profile
#' 
#' Create a link to a patient profile directory
#' (where the patient profile files are saved) by adding an extra column with the link
#' in the data.
#' The path to the patient profile is built as:
#' [patientProfilePath]/subjectProfile-[subjectID].pdf,
#' where '/' are replaced with '-' in the subject 
#' identifier (\code{subjectVar}).
#' @param data a data.frame
#' @param patientProfilePath string indicating the directory 
#' where the patient profiles are stored.
#' @param subjectVar string indicating which column in the data represents the
#' unique subject identifier, "USUBJID" by default.
#' @param checkExist Logical, if TRUE (by default)
#' the \code{patientProfilePath} is checked for existence,
#' and an error is returned if this directory doesn't exist.
#' @return A data.frame with two extra columns:
#' \code{patientProfilePath} and \code{patientProfileLink} with
#' the path to the patient profile and an hyperlink to it, respectively.
#' @author Michela Pasetto
#' @examples 
#' # Typical CDISC dataset contains universal subject ID (USUBJID)
#' data <- data.frame(USUBJID = c("subj1", "subj2", "subj3"))
#' dataWithPatientProfileVar <- createPatientProfileVar(
#'   data = data, 
#'   patientProfilePath = "pathProfiles", 
#'   checkExist = FALSE
#' )
#' # path and HTML link are included in the output dataset
#' head(dataWithPatientProfileVar[, c("USUBJID", "patientProfilePath", "patientProfileLink")])
#' @export 
createPatientProfileVar <- function(
	data,
	patientProfilePath,
	subjectVar = "USUBJID",
	checkExist = TRUE
) {
	
	#pathToDir <- paste(patientProfilePath, collapse = "/")
	
	fileExist <- file.exists(patientProfilePath)	
	if(checkExist & !fileExist) 
		stop("File path for patient profiles not found.")
	
	if(! subjectVar %in% colnames(data)) {
		
		warning(
			sprintf(
				paste("Unique subject identifier '%s' not available in the data,",
					"so the patient profile variable is not created."
				), subjectVar
			)
		)
		return(data)
		
	}	
	
	data$patientProfilePath <- file.path(
		patientProfilePath, 
		sprintf(
			"subjectProfile-%s.pdf",
			sub("/", "-", data[, subjectVar])
		)
	)
	
	data$patientProfileLink <- sprintf(
		'<a href="%s" target="_blank">%s</a>',
		data$patientProfilePath,
		data[, subjectVar]
	)
	
	return(data)
	
}


#' Format hover text for use in plotly interactive plots.
#' The labels are wrapped to multiple lines if exceed the width of the plotly
#' hover box, e.g. in case labels for points with same x/y coordinates overlap,
#' and corresponding labels are truncated.
#' @param x Vector with hover text information.
#' @param label Label for the variable
#' @param width Integer, number of characters at 
#' which the hover text should be cut at to multiple lines.
#' @return String with formatted hover label.
#' @author Laure Cougnaud
#' @export
formatHoverText <- function(x, label, width = 50){
	formatLongLabel <- function(x)
		vapply(x, function(x1)
			xRF <- paste(strwrap(x1, width = width), collapse = "<br>"),
			FUN.VALUE = character(1)
		)
	formatLongLabel(
		paste0(
			label, ": ",
			paste(unique(as.character(x)), collapse = ", ")
		)
	)
}

#' Get Javascript custom scripts required for specific
#' clinical data functionalities.
#' @param dep (optional) Character vector with names of Javascript dependencies 
#' By default, all dependencies are included.
#' @param type (optional) Character vector with type of dependencies,
#' either: 'collapsibleButton' or 'patientProfiles'.
#' @return List of \code{\link[htmltools]{htmlDependency}}.
#' To include this dependency in a report e.g. generated with rmarkdown,
#' these can be passed to the: \code{extra_dependencies} parameter
#' of the \code{output_format} specific function, e.g.:
#' \code{rmarkdown::render(...,	
#' output_format = rmarkdown::html_document(extra_dependencies = dep))
#' }
#' @importFrom htmltools htmlDependency
#' @importFrom utils packageVersion
#' @author Laure Cougnaud
#' @export
getJsDepClinDataReview <- function(
	type = c("collapsibleButton", "patientProfiles"),
	dep = NULL) {

	type <- match.arg(type, several.ok = TRUE)
	if(!is.null(dep))
		dep <- match.arg(dep, 
			choices = c("FileSaver", 
				"jszip", "jszip-utils", 
				"PatientProfiles", "collapsibleButton"
			),
			several.ok = TRUE
		)
	
	getPackageJSDep <- function(name, version) {
		srcDep <- system.file("js", package = "clinDataReview", name)
		htmltools::htmlDependency(
			name = name,
			version = version,
			src = srcDep,
			script = list.files(srcDep, pattern = "\\.js$"),
			stylesheet = list.files(srcDep, pattern = "\\.css$")
		)
	}
	
	# Some of the dependencies e.g. jszip are also imported
	# by rmd by default (when interactive plots included)
	# it is important that the JS dep 'name' match the ones used in rmd package
	# otherwise dependency with different versions will be included
	# and the rmd version (e.g. older one) prevals on the custom one
	# see ? htmltools::resolveDependencies
	
	htmlDep <- list(
		getPackageJSDep(name = "FileSaver", version = "2.0.2"),
		getPackageJSDep(name = "jszip", version = "3.5.0"),
		getPackageJSDep(name = "jszip-utils", version = "0.1.0"),
		getPackageJSDep(name = "PatientProfiles", version = packageVersion("clinDataReview")),
		getPackageJSDep(name = "collapsibleButton", version = packageVersion("clinDataReview"))
	)
	
	if(!is.null(type)){
		if(!is.null(dep))
			warning("'type' or 'dep' should be specified (not both). 'dep' is considered.")
		dep <- c(
			if("collapsibleButton" %in% type)	"collapsibleButton",
			if("patientProfiles" %in% type)
				c("FileSaver", "jszip", "jszip-utils",
					"PatientProfiles")
		)
	}
	
	if(!is.null(dep)) {
		selectedDep <- which(sapply(htmlDep, '[[', "name") %in% dep)
		htmlDep <- htmlDep[selectedDep]
	}

	return(htmlDep)

}

#' Function to create collapsible HTML content
#' 
#' Please note that the button is of class:
#' 'hideshow', defined in the 'input.hideshow.js' js file
#' included in the package.
#' @param input Object to be collapse, e.g.
#' datatable.
#' @param title String with button title.
#' @return \code{\link[htmltools]{tag}} object
#' @author Laure Cougnaud
#' @importFrom htmltools tags div tagList br
#' @importFrom htmlwidgets prependContent
#' @importFrom clinUtils getColorPalette
#' @export
collapseHtmlContent <- function(
	input, 
	title = "Click to show or hide"
	){
	
	btnStyle <- paste0(
		#"border: none;",
		"background-color: transparent"
	)
		
	btn <- tags$input(
		type = "button", 
		class = "hideshow",
		value = title,
		style = btnStyle
	)

	# set a specific class to be able to set width of embedded DT
	# only for the DT included within a collapsible input
	btnContent <- div(input, 
		style = "height:100%; display: inline-block;", 
		class = "hideshow_cnt"
	)
	
	res <- tagList(
		btn, 
		btnContent, br(), br()
	)
	
	res <- prependContent(res, getJsDepClinDataReview(type = "collapsibleButton"))
	
	return(res)
	
}

#' Get path ('href') property from hyperlink(s).
#' @param x Character vector with hyperlink(s).
#' If multiple, the hyperlinks should be separated by: ', '.
#' @return Character vector of length \code{x}
#' containing only the hyperlinks.
#' @author Laure Cougnaud
#' @export
getPathHyperlink <- function(x){
	
	linksSplit <- strsplit(as.character(x), split = ", ")
	
	paths <- sapply(linksSplit, function(linksAll){
		linksDest <- sapply(linksAll, function(link){
			sub(".+href=\"(.+)\"( |\\>).+", "\\1", link)	
		})
		if(length(linksDest) != length(linksAll))
			stop("Extraction of path from each hyperlink is not correct.")
		toString(linksDest)
	})

	# approach with xml2/rvest packages,
	# but returns error message if path not available
#			sapply(linksAll, function(link){
#				linkHTML <- read_html(link, options = "NOERROR")
#				linkA <- html_nodes(linkHTML, "a")
#				html_attr(x = linkHRef, name = "href")
#			})
#		)

	if(!is.character(paths) || length(paths) != length(x))
		stop("Parsing of paths from all hyperlinks failed.")
	
	return(paths)
	
}

#' Get formula for a specific variable,
#' to be used in aesthetic specification in \code{\link[plotly]{plot_ly}}.
#' @param var Character vector with variable to combine.
#' Otherwise with the '+' operator.
#' @return \code{\link[stats]{as.formula}}
#' @author Laure Cougnaud
#' @export
varToFm <- function(var){
	fm <- as.formula(paste0("~", paste(var, collapse = "+")))
	return(fm)
}

