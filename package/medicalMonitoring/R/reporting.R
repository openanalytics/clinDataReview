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
getMdHeader <- function(title, level = 1, settings = NULL){
	
	if(!is.null(settings)){
		
		idxInput <- which(settings$rmd_files == knitr::current_input())
		if(length(idxInput) > 0){
			levelInput <- settings$rmd_file_depth[idxInput]
			if(!is.null(levelInput) && !is.na(levelInput))
				level <- levelInput
		}
		
	}
	
	headerST <- paste0("\n",
		paste(rep("#", level ),collapse =""),
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

#' Include output from medical monitoring, or list 
#' of such outputs in a Rmarkdown report.
#' @param list List of medical monitoring plots,
#' potentially nested.
#' If nested, the list should be named by the different elements,
#' separated by \code{sep}, e.g. 
#' \code{list('group1.param1' = .., 'group1.param2' = ...)}.
#' @param sep String with separator used to distinguish 
#' different levels in the labels of the list.
#' e.g. '\\.' by default.
#' @param level Integer with base level for section,
#' 1 by default.
#' @param labelGeneral String with general label for the
#' chunk.
#' @return No returned value, the plots are included in the
#' report.
#' @author Laure Cougnaud
#' @export
knitPrintMedicalMonitoring <- function(
	list, sep = "\\.", level = 1,
	labelGeneral = "medicalMonitoring"){
	
	if(inherits(list, "medicalMonitoring")){
		
		knit_print(list)
		
	}else{
	
		listLabels <- strsplit(names(list), split = sep)
		nLevels <- unique(sapply(listLabels, length))
		if(length(nLevels) != 1)
			stop("Issue in extraction of labels for the visualization.")
		
		if(nLevels == 1){
			
			knitPrintListObjects(
				xList = list, 
				generalLab = labelGeneral,
				titles = names(list), 
				titleLevel = level
			)
		
		}else{
			
			labelLevelCur <- sapply(listLabels, "[[", 1)
			for(label in unique(labelLevelCur)){
				
				# section header
				cat(getMdHeader(title = label, level = level))
				
				# elements for current section
				listEl <- list[which(labelLevelCur == label)]
				names(listEl) <- sub("[^\\.]{1,}\\.(.+)", "\\1", names(listEl))
				
				knitPrintMedicalMonitoring(list = listEl, sep = sep, level = level+1)
				
			}
			
		}
		
	}
	
}
