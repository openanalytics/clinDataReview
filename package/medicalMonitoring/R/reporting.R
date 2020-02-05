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