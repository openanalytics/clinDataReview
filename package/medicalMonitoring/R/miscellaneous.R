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
#' @author Laure Cougnaud
#' @export
getJsDepMedicalMonitoring <- function(){
	
	jsPath <- system.file("js", package = "medicalMonitoring")
	jsDepNames <- list.files(jsPath)
	
	htmlDep <- lapply(jsDepNames, function(jsDep){
		srcDep <- file.path(jsPath, jsDep)
		htmltools::htmlDependency(
			name = jsDep,
			version = packageVersion("medicalMonitoring"),
			src = c(file = srcDep),
			script = list.files(srcDep)
		)
	
	})

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
#' @importFrom htmltools tag tagList div 
#' @export
includeInButton <- function(
	input, 
	id, 
	title = "Click to show or hide",  
	color = glpgStyle::glpgColor()["green"], 
	borderColor = glpgStyle::glpgColor()["orange"]){

	if(missing(id)){
		id <- paste0("button:", sample.int(n = 1000, size = 1))
	}
	
	# create button
	id <- gsub(" ", "", id)#[[:punct:]]|
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
	btnContent <- div(id = id, class= "collapse buttonArrow", input)
	
	res <- tagList(btn, btnContent, br(), br(), br())
	
	return(res)
	
}