#' Format interactive plot, with possibility to download patient profiles
#' on a click event.
#' @param idVar String with variable of \code{data}
#' containing plot element.
#' @param idVarPlot String with variable in the \code{\link[plotly]{plotly}}
#' output containing IDs.
#' @param idFromDataPlot Logical, if TRUE (by default) \code{idVarPlot}
#' is extracted from the data of the plot output object (e.g. if this plot
#' was created from \code{\link[plotly]{ggplotly}}), otherwise
#' directly from the plot object 
#' (if the plot was created from \code{\link[plotly]{plot_ly}} directly).
#' @param highlightOn String with event to turn on the selection
#' (\code{on} parameter of \code{\link[plotly]{highlight}}),
#' 'plotly_click' by default.
#' @param highlightOff String with event to turn off the selection
#' (\code{off} parameter of \code{\link[plotly]{highlight}}),
#' 'plotly_doubleclick' by default.
#' @param pathVar String with variable of \code{data} containing path
#' to a subject-specific report (e.g. patient profiles).
#' @param pathDownload Logical, if TRUE (by default) the subject-specific report(s)
#' are downloaded in a zip compressed file.
#' If FALSE (only available if unique report per \code{idVarPlot}),
#' each report is opened in a new window.
#' @param verbose Logical, if TRUE report progress messages
#' during execution (included in the browser 'Console').
#' @param labelVarPlot String with plotly variable used to
#' extract label to build the file name of the zip compressed
#' file containing patient report.
#' If not specified, the label are extracted based on the \code{idVarPlot}
#' of the selected plot element.
#' @param keyHighlightBox Logical, if TRUE (FALSE by default) a selectize box
#' is included to highlight selected element(s) of the key variable.
#' @inheritParams formatDataForPlotClinData
#' @inheritParams clinDataReview-common-args
#' @return Updated \code{\link[plotly]{plotly}} object.
#' @author Laure Cougnaud
#' @importFrom plotly highlight
#' @importFrom htmlwidgets onRender JS
#' @importFrom htmlwidgets prependContent
#' @importFrom clinUtils getLabelVar
#' @export
formatPlotlyClinData <- function(
	pl, data, 
	idVar = "USUBJID", 
	pathVar = NULL, pathDownload = TRUE,
	idFromDataPlot = FALSE, 
	idVarPlot = "key", labelVarPlot = NULL,
	highlightOn = "plotly_click",
	highlightOff = "plotly_doubleclick",
	id = paste0("plotClinData", sample.int(n = 1000, size = 1)),
	# selection
	selectVars = NULL, selectLab = getLabelVar(selectVars, labelVars = labelVars),
	keyVar = NULL, keyHighlightBox = FALSE, labelVars = NULL,
	verbose = FALSE){

	idVarInit <- idVar

	# change plotly default on click event in the legend
	# single click: selected item is visible
	# double click: select item is hidden
	# plotly default is to display all excepted item when clicked on a legend
	pl <- pl %>% layout(
		legend = 
			list(
				itemclick = "toggleothers",
				itemdoubleclick = "toggle"
			)
	)

	# turn-off selection by double-clicking on the graph
	pl <- pl %>% highlight(on = highlightOn, off = highlightOff, 
	   selectize = keyHighlightBox)
	
	# to check attributes available in the plotly object:
	#	plotly_json(pl)
	
	if(!is.null(pathVar)){
		
		if(!is.null(idVar) && length(idVar) > 1){
			data$key <- do.call(interaction, data[, idVar])
			idVar <- "key"
		}
		
		dataPPDf <- unique(data[, c(idVar, pathVar)])
		
		# in case 'pathVar' is formatted as URL, only extract the path
		dataPPDf[, pathVar] <- getPathHyperlink(dataPPDf[, pathVar])
		
		idxDupl <- which(duplicated(dataPPDf[, idVar]))
		if(length(idxDupl) > 0){
			dataDupl <- merge(dataPPDf, dataPPDf[idxDupl, idVar, drop = FALSE])
			rownames(dataDupl) <- NULL
			stop(paste0("Different ", sQuote(pathVar), " available for specific ", 
				idVarInit, ":\n", 
				paste(capture.output(print(dataDupl)), collapse = "\n")
			))
		}
		dataPP <- dataPPDf[, c(idVar, pathVar)]
		colnames(dataPP) <- c("key", "path")
		
		# Add Js function to the widget
		
		# Important: parameters should be in the same order
		# than specified in function definition 
		# (fct call by parameter doesn't exists natively in Javascript)
		jsPatientProfiles <- JS("function(el, x, data){",
			paste0("getPatientProfilesPlotly(el, x, data,",
				"fromdata=", tolower(idFromDataPlot), ",",
				"idvar=", sQuote(idVarPlot), ",",
				"labelplot=", sQuote(id), ",",
				"labelvar=", ifelse(is.null(labelVarPlot), 'null', sQuote(labelVarPlot)), ",",
				"download=", tolower(pathDownload), ",",
				"verbose=", tolower(verbose), 
				");"
			),
		"}") 
		
		pl <- pl %>% onRender(
			jsCode = jsPatientProfiles, 
			data = dataPP
		)
		
		# and required Js libraries
		prepCntArgs <- c(list(x = pl), getJsDepClinDataReview(type = "patientProfiles"))
		pl <- do.call(prependContent, prepCntArgs)		
	
	}
	
  if(!is.null(selectVars)){
	  
	  res <- addSelectBtn(
	    data = data, pl = pl,
	    selectVars = selectVars, selectLab = selectLab, labelVars = labelVars,
	    keyVar = keyVar,
	    id = id
    )
	  
  }else{res <- pl}

	return(res)
	
}

#' Add selection box(es) to a plotly plot.
#' @param data \code{\link[crosstalk]{SharedData}} object used for the plot.
#' @inheritParams clinDataReview-common-args
#' @return if \code{selectVars} is specified: a \code{\link[htmltools]{browsable}}
#' object combining the select buttons and the \code{plotly} object.\cr
#' Otherwise, the input \code{plotly} object.
#' @importFrom clinUtils getLabelVar
#' @importFrom crosstalk filter_select SharedData
#' @importFrom htmlwidgets JS
#' @importFrom htmltools tags tagList
#' @importFrom stats as.formula
addSelectBtn <- function(
  data, pl,
  selectVars = NULL, selectLab = NULL, labelVars = NULL,
  id = paste0("plotClinData", sample.int(n = 1000, size = 1)),
  keyVar){
    
  # (from doc) limitation: the highlighting variable has to be nested inside filter variable(s)
  xKey <- data[, keyVar]
  if(is.factor(xKey)) xKey <- droplevels(xKey)
  xFilters <- do.call(interaction, data[, selectVars, drop = FALSE])
  isNested <- tapply(xFilters, xKey, function(x) length(unique(x)))
  if(any(isNested > 1)){
    
    warning(paste("The selection variable(s):", toString(shQuote(selectVars)),
      "is/are not nested in the key variable:", shQuote(keyVar),
      "so the select button cannot be created.'"))
    res <- pl
    
  }else{
    
    selectLab <- getLabelVar(selectVars, labelVars = labelVars, label = selectLab)
    
    dataSharedData <- crosstalk::SharedData$new(
      data = unique(data[, c(keyVar, selectVars)]), 
      key = varToFm(keyVar), 
      group = id
    )
    
    filterBtn <- lapply(seq_along(selectVars), function(iVar){
      
      var <- selectVars[iVar]
      idBtn <- paste0(id, iVar)
      btn <- crosstalk::filter_select(
        id = idBtn,
        label = selectLab[var],
        sharedData = dataSharedData, 
        group = as.formula(paste("~", var)), 
        multiple = FALSE
      )
      
      # set default to first element in the vector
      default <- if(is.factor(data[, var])){
        levels(droplevels(data[, var]))[1]
      }else{sort(unique(data[, var]))[1]}
      
      # add custom JS to set default value for the button
      # and remove the 'All' option ('')
      js <- htmlwidgets::JS(
        'function set_btn(){',
        paste0(
          'var btn = document.getElementById("', idBtn, 
          '").getElementsByClassName("selectized")[0].selectize'
        ),
        paste0('btn.setValue("', default, '", false);'),
        'btn.removeOption("")',
        '}',
        '$(document).ready(set_btn);'
      )
      script <- htmltools::tags$script(js)
      btnCustom <- htmltools::tagList(script, btn)
      
    })
    
    res <- list(buttons = filterBtn, plot = pl)
    
  }
  
  return(res)
  
}
