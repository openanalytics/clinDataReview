#' Get Markdown header, for creation in Rmarkdown.
#' 
#' The depth is extracted:
#' \enumerate{
#' \item{from the \code{settings} if a
#' specified depth is provided in the 'rmd_file_depth' for the
#' current knitted file}
#' \item{\code{level} parameter otherwise}
#' }
#' @param title String with header title.
#' @param level Integer of length 1 with header depth/level,
#' 1 by default
# #' @param settings List with settings from YAML file, containing:
# #' \itemize{
# #' \item{'rmd\_files': }{character vector with names of Rmd files}
# #' \item{'rmd\_file\_depth': }{integer vector with corresponding file depth}
# #' }
#' @return String with Markdown header,
#' to be included in R within \code{cat}.
#' @family clinical data reporting
#' @export
getMdHeader <- function(
    title, level = 1
#, settings = NULL
) {
  
#	if(!is.null(settings)){
#		
#		idxInput <- which(settings$rmd_files == knitr::current_input())
#		if(length(idxInput) > 0){
#			levelInput <- settings$rmd_file_depth[idxInput]
#			if(!is.null(levelInput) && !is.na(levelInput))
#				level <- levelInput
#		}
#		
#	}
  
  headerST <- paste0("\n",
      paste(rep("#", level ),collapse =""),
      " ", title, "\n"
  )
  
  return(headerST)
  
}

#' Combine select box(es) and the plot
#' @param x Object of class \code{clinDataReview}
#' @return \code{x} object:
#' \itemize{
#' \item{with the \code{plot} element containing a combination of the 
#' \code{buttons} and the \code{plot}}
#' \item{without the \code{buttons} element}
#' }
#' @importFrom utils hasName
#' @importFrom htmltools div
combineButtonsAndPlot <- function(x){
  
  if(is.list(x) && hasName(x, "buttons")){
  
    btns <- x$buttons
    
    # set the size of the button
    widthBtn <- floor(100/length(btns))
    divBnt <- lapply(btns, htmltools::div, style = paste0("width:", widthBtn, "%;"))
    
    # create a css flexbox with all widgets
    divPlot <- htmltools::div(x$plot, style = "width:100%;")
    argsFlex <- do.call(tagList, c(divBnt, list(divPlot)))
    x$plot <- htmltools::div(argsFlex, style = "display: flex; flex-wrap: wrap;")
    x$buttons <- NULL
    
  }
  
  return(x)
  
}

#' Print a \code{clinDataReview} object in the console
#' @param x Object of class \code{clinDataReview}
#' @param ... Extra parameters for compatibility with \code{\link{print}},
#' not used currently.
#' @return No returned value, the object is printed into the console.
#' @importFrom htmltools browsable
#' @importFrom utils hasName
#' @export
print.clinDataReview <- function(x, ...){
  
  x <- combineButtonsAndPlot(x)
  
  if(!inherits(x$plot, "plotly"))
    x$plot <- browsable(x$plot)
  
  class(x) <- setdiff(class(x), "clinDataReview")
  
  if(!utils::hasName(x, "table"))
    x <- x$plot
  
  print(x)
  
}

#' Print \code{clinDataReviewTable} object in a knitted document
#' (e.g. Rmarkdown document).
#' @param x Object of class \code{clinDataReview}
#' @param ... Extra parameters for compatibility with \code{\link[knitr]{knit_print}},
#' not used currently.
#' @importFrom knitr knit_print
#' @importFrom htmltools tagList knit_print.shiny.tag.list
#' @author Laure Cougnaud
#' @export
knit_print.clinDataReview <- function(x, ...){
  
  # extract plot
  x <- combineButtonsAndPlot(x)
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

#' Include output from clinical data, or list 
#' of such outputs in a Rmarkdown report, with an appropriate title.
#' @param list Named list of clinical data plots,
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
#' @return No returned value, the plots are included in the
#' report.
#' If a element in the list are empty (NULL), these elements
#' (and the associated sections) are not included in the report.
#' @author Laure Cougnaud
#' @importFrom clinUtils knitPrintListObjects
#' @importFrom stringr str_split fixed
#' @family clinical data reporting
#' @export
knitPrintClinDataReview <- function(
  list, sep = ".", level = 1){
  
  classes <- c("clinDataReview", "datatables", "plotly")
  if(is.null(list) || length(list) == 0){
    
    return(invisible())
    
  }else	if(inherits(list, what = classes)){
    
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
        
        # filter empty elements
        isEmpty <- sapply(list, is.null)
        list <- list[!isEmpty]
        titles <- if(!noName)	names(list)
        
#        if(length(list) == 0){
#          return(invisible())
#        }else{
          # print list of objects
          # Note for static plot, we would need the 'generalLabel' option
          knitPrintListObjects(
              xList = list, 
              titles = titles, 
              titleLevel = level
          )
        #}
        
        # nested list
      }else	if(any(sapply(list, inherits, what = "list"))){
        
        list <- unlist(list, recursive = FALSE)
        knitPrintClinDataReview(
            list = list, 
            level = level, 
            sep = "."
        )
        
      }else	stop("Objects in the list should be among the classes: ", 
            toString(c("list", classes)), ".")
      
      # list of level 1 with multiple groups:
    }else{
      
      # label for current section level:
      labelLevelCur <- sapply(listLabels, "[[", 1)
      
      for(label in unique(labelLevelCur)){
        
        # extract elements for current section
        idxEl <- which(labelLevelCur == label)
        listEl <- list[idxEl]
        # build labels for subsections
        labelLevelOther <- lapply(listLabels, "[", -1)[idxEl]
        labelLevelOther <- sapply(labelLevelOther, paste, collapse = sep)
        names(listEl) <- labelLevelOther
        
        # filter empty elements
        isEmpty <- sapply(listEl, is.null)
        listEl <- listEl[!isEmpty]
        
        if(length(listEl) != 0){
          
          # section header
          cat(getMdHeader(title = label, level = level))
          
          knitPrintClinDataReview(
              list = listEl, 
              sep = sep, 
              level = level+1
          )
          
        }
        
      }
      
    }
    
  }
  
}

