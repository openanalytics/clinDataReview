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
#' the medical monitoring functionalities.
#' @return list of \code{\link[htmltools]{htmlDependency}}.
#' To include this dependency in a report e.g. generated with rmarkdown,
#' these should be passed to the: \code{extra_dependencies} parameter
#' of the \code{output_format} specific function, e.g.:
#' \code{rmarkdown::render(...,	
#' output_format = rmarkdown::html_document(extra_dependencies = dep))
#' }
#' @importFrom htmltools htmlDependency
#' @importFrom utils packageVersion
#' @author Laure Cougnaud
#' @export
getJsDepMedicalMonitoring <- function(){
	
	
	getPackageJSDep <- function(name, version){
		srcDep <- system.file("js", package = "medicalMonitoring", name)
		htmltools::htmlDependency(
			name = name,
			version = version,
			src = srcDep,
			script = list.files(srcDep)
		)
	}
	
	# Some of the dependencies e.g. jszip are also imported
	# by rmd by default (when interactive plots included)
	# it is important that the JS dep 'name' match the ones used in rmd package
	# otherwise dependency with different versions will be included
	# and the rmd version (e.g. older one) prevals on the custom one
	# see ? htmltools::resolveDependencies
	
	htmlDep <- list(
		getPackageJSDep(name = "FileSaver", version = "1.3.8"),
		getPackageJSDep(name = "jszip", version = "3.2.2"),
		getPackageJSDep(name = "jszip-utils", version = "0.1.0"),
		getPackageJSDep(name = "PatientProfiles", version = packageVersion("medicalMonitoring"))
	)

	return(htmlDep)

}

#' Function to create collapse with button in html output
#' @param input Object to be included within the button.
#' @param id String with button ID. If not specified,
#' a random id, as 'button:x' is used.
#' @param title String with button title.
#' @param color String witn button color.
#' @param borderColor String with button color border.
#' @return \code{\link[htmltools]{HTML}} object
#' @author Kirsten Van Hoorde, Laure Cougnaud
#' @importFrom htmltools tag tagList div br
#' @export
includeInButton <- function(
	input, 
	id = paste0("button:", sample.int(n = 1000, size = 1)), 
	title = "Click to show or hide",  
	color = glpgStyle::glpgColor()["green"], 
	borderColor = glpgStyle::glpgColor()["orange"]){
	
	# create button
	id <- gsub("[[:punct:]]| ", "", id)
	btnStyle <- paste0(
		"color:", color, " !important;",
		"border-color:", borderColor, " !important;",
		"background-color: 'white' !important"
	)
	btn <- tag(
		"button", 
		varArgs = list(
			type = "button", class = "btn",
			style = btnStyle,
			'data-toggle'= "collapse",
			title = title,
			'data-target' = paste0("#", id),
			title
		)
	)
	
	# create content
	btnContent <- div(class = "row", div(id = id, class= "collapse buttonArrow", input))
	
	res <- tagList(btn, btnContent, br(), br(), br(), br())
	
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
	
	linksSplit <- strsplit(x, split = ", ")
	
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
#' @param var String with variable.
#' @return \code{\link[stats]{as.formula}}
#' @author Laure Cougnaud
#' @export
varToFm <- function(var){
	fm <- as.formula(paste0("~", var))
	return(fm)
}