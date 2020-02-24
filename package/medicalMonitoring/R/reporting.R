#' Get Markdown header, for creation in Rmarkdown.
#' 
#' The depth is extracted:
#' \enumerate{
#' \item{from the \code{settings} if a
#' specified depth is provided in the 'rmd_file_depth' for the
#' current knitted file}
#' \item{\code{depth} parameter otherwise}
#' }
#' @param title String with header title.
#' @param depth Integer of length 1 with header depth/level,
#' 1 by default
#' @param settings List with settings from YAML file, containing:
#' \itemize{
#' \item{'rmd\_files': }{character vector with names of Rmd files}
#' \item{'rmd\_file\_depth': }{integer vector with corresponding file depth}
#' }
#' @return String with Markdown header,
#' to be included in R within \code{cat}.
#' @export
getMdHeader <- function(title, depth = 1, settings = NULL){
	
	if(!is.null(settings)){
		
		idxInput <- which(settings$rmd_files == knitr::current_input())
		if(length(idxInput) > 0){
			depthInput <- settings$rmd_file_depth[idxInput]
			if(!is.null(depthInput) && !is.na(depthInput))
				depth <- depthInput
		}
		
	}
	
	headerST <- paste0("\n",
		paste(rep("#", depth ),collapse =""),
		" ", title, "\n"
	)
	
	return(headerST)
	
}

#' Print \code{medicalMonitoringTable} object in a knitted document
#' (e.g. Rmarkdown document).
#' @param x Object of class \code{medicalMonitoring}
#' @param ... Extra parameters for \code{\link[knitr]{knit_print}},
#' not used by default.
#' @importFrom knitr knit_print
#' @importFrom htmltools tagList knit_print.shiny.tag.list
#' @author Laure Cougnaud
#' @export
knit_print.medicalMonitoring <- function(x, ...){

	# extract plot
	plot <- x$plot
		
	# extract table and include it within a button if required
	table <- x$table
	xMetadata <- attributes(table)$metadata
	if(!is.null(xMetadata$button) && xMetadata$button){
		
		table <- includeInButton(
			input = table, 
			id = xMetadata$buttonId, 
			title = xMetadata$buttonTitle
		)
	
	}
	
	res <- tagList(plot, table)
	
	requireNamespace("htmltools")
	htmltools::knit_print.shiny.tag.list(res)
	
}