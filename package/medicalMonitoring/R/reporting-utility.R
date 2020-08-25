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
#' @param level Integer of length 1 with header depth/level,
#' 1 by default
#' @param settings List with settings from YAML file, containing:
#' \itemize{
#' \item{'rmd\_files': }{character vector with names of Rmd files}
#' \item{'rmd\_file\_depth': }{integer vector with corresponding file depth}
#' }
#' @return String with Markdown header,
#' to be included in R within \code{cat}.
#' @family medical monitoring reporting
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
		
		table <- collapseHtmlContent(
			input = table, 
			title = xMetadata$buttonTitle
		)
	
	}
	
	res <- tagList(plot, table)
	
	htmltools::knit_print.shiny.tag.list(res)
	
}

#' Include output from medical monitoring, or list 
#' of such outputs in a Rmarkdown report, with an appropriate title.
#' @param list Named list of medical monitoring plots,
#' the names are used for the section header.
#' If several section header should be created, either:
#' \itemize{
#' \item{a list of level 1 named by the different group elements,
#' separated by \code{sep}, e.g. 
#' \code{list('group1.param1' = .., 'group1.param2' = ...)}.
#' Such list is e.g. created with \code{\link[plyr]{dlply}}.}
#' \item{a nested list, named with the different groups,
#' e.g. created with \code{lapply}}
#' }
#' @param sep String with separator used to distinguish 
#' different levels in the labels of the list.
#' e.g. '.' by default.
#' @param level Integer with base level for section,
#' 1 by default.
#' @param generalLabel String with general label for the
#' chunk(s) including the plots in the Rmarkdown document. 
#' Note: as chunks should have unique labels in a Rmd document,
#' this label should be unique within the same Rmarkdown document 
#' (in case \code{listPlots} with same names are included multiple times
#' in the document).
#' @return No returned value, the plots are included in the
#' report.
#' @author Laure Cougnaud
#' @importFrom glpgUtilityFct knitPrintListObjects
#' @importFrom stringr str_split fixed
#' @family medical monitoring reporting
#' @export
knitPrintMedicalMonitoring <- function(
	list, sep = ".", level = 1,
	generalLabel = "medicalMonitoring"){
	
	classes <- c("medicalMonitoring", "datatables", "plotly")
	if(inherits(list, what = classes)){
		
		knit_print(list)
		
	}else{
	
		if(!is.list(list))	stop("Input object not supported.")
		if(is.null(names(list)))	stop("Input list should be named.")
		
		# base::strsplit remove element which are empty, so use stringr::str_split instead
		listLabels <- stringr::str_split(names(list), pattern = stringr::fixed(sep))
		nLevels <- unique(sapply(listLabels, length))
		if(length(nLevels) != 1)
			stop("Issue in extraction of labels for the visualization.")
		
		if(nLevels == 1){
			
			# list of list 1 with expected objects
			isListObjects <- any(sapply(list, inherits, what = classes))
			if(isListObjects){
		
				# if the list has missing name (e.g. created from plyr::dlply without grouping variable)
				# don't include section header
				noName <- length(list) == 1 && (is.na(names(list)) || names(list) == "NA")
				titles <- if(!noName)	names(list)
				
				# print list of objects
				knitPrintListObjects(
					xList = list, 
					generalLabel = generalLabel,
					titles = titles, 
					titleLevel = level
				)
				
			# nested list
			}else	if(any(sapply(list, inherits, what = "list"))){
				
				list <- unlist(list, recursive = FALSE)
				knitPrintMedicalMonitoring(
					list = list, 
					level = level, 
					generalLabel = generalLabel,
					sep = "."
				)
				
			}else	stop("Objects in the list should be among the classes: ", 
						toString(c("list", classes)), ".")
		
		# list of level 1 with multiple groups:
		}else{
			
			# label for current section level:
			labelLevelCur <- sapply(listLabels, "[[", 1)
			
			for(label in unique(labelLevelCur)){
				
				# section header
				cat(getMdHeader(title = label, level = level))
				
				# extract elements for current section
				idxEl <- which(labelLevelCur == label)
				listEl <- list[idxEl]
				# build labels for subsections
				labelLevelOther <- lapply(listLabels, "[", -1)[idxEl]
				labelLevelOther <- sapply(labelLevelOther, paste, collapse = sep)
				names(listEl) <- labelLevelOther
				
				knitPrintMedicalMonitoring(
					list = listEl, 
					sep = sep, 
					level = level+1,
					generalLabel = label
				)
				
			}
			
		}
		
	}
	
}
