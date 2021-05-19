#' Create a 'clinical data table', associated to a plot.
#' 
#' Interactive table is created, with the possibility to have
#' clickeable link to patient-specific report, and included
#' within a button.
#' @param keyLab String with label for \code{keyVar}.
#' @param tableButton Logical, if TRUE (by default)
#' the table is included within an HTML button.
#' @param tableVars Character vector with variables to be included 
#' in the table.
#' @param tableLab Named character vector with labels
#' for each \code{tableVars}.
#' @param tablePars List with parameters passed to the
#' \code{\link[clinUtils]{getClinDT}} function.
#' @param verbose Logical, if TRUE (FALSE by default) progress messages are printed
#' in the current console.
#' @inheritParams formatDataForPlotClinData
#' @inheritParams clinDataReview-common-args
#' @inheritParams clinDataReview-common-args-summaryStatsVis
#' @return \code{\link[DT]{datatable}}
#' @author Laure Cougnaud
#' @importFrom clinUtils getClinDT getLabelVar
#' @importFrom crosstalk SharedData
#' @importFrom stats as.formula
#' @export
tableClinData <- function(
	data, 
	idVar = "USUBJID", idLab = getLabelVar(idVar, labelVars = labelVars),
	keyVar = NULL, keyLab = getLabelVar(keyVar, labelVars = labelVars),
	pathVar = NULL, pathLab = getLabelVar(pathVar, labelVars = labelVars),
	pathExpand = FALSE,
	tableVars = colnames(data),
	tableLab = getLabelVar(tableVars, labelVars = labelVars),
	tableButton = TRUE, tablePars = list(),
	id = paste0("plotClinData", sample.int(n = 1000, size = 1)),
	labelVars = NULL,
	verbose = FALSE){
	
	tableVarsInit <- tableVars
	
	if(is.null(tablePars))	tablePars <- list()
	
	if(!idVar %in% colnames(data))
		warning(paste(
			"Subject ID variable:", idVar,
			"is not available in the data, so it is ignored."
		))

	# add key/id in variables to display
	# (used for linking plot <-> table and for the path)
	tableVars <- unique(c(
		if(idVar %in% colnames(data))	setdiff(idVar, tableVars), 
		tableVars, 
		keyVar
	))
	tableLab <- c(
		getLabelVar(var = idVar, label = idLab, labelVars = labelVars),
		tableLab, 
		getLabelVar(var = keyVar, label = keyLab, labelVars = labelVars)
	)
	tableLab <- tableLab[!duplicated(names(tableLab))]
	
	usePathVar <- !is.null(pathVar)
	if(usePathVar && !pathVar %in% colnames(data)){
		warning(paste(
			"Variable with path to subject profile:", pathVar,
			"is not available in the data, so it is ignored."
		))
		usePathVar <- FALSE
	}
	
	# add hyperlink in the table:
	if(usePathVar){
		
		tableVars <- c(pathVar, tableVars) # add in variables for DT
		
		# create the hyperlink (if not already created)
		if(pathExpand){
			tableLab[pathVar] <- getLabelVar(var = pathVar, 
				label = pathLab, labelVars = labelVars)
		}
		
	}
	
	# retain only specified variables:
	data <- data[, tableVars, drop = FALSE]
	
	# convert character columns to factor
	colCharacter <- sapply(data, function(x)
		is.character(x) & !is.factor(x)
	)
	data[, colCharacter] <- lapply(data[, colCharacter, drop = FALSE], as.factor)
	
	## set DT options
	
	if(usePathVar){
		
		tablePars <- c(tablePars, 
			# escape column with hyperlink
			if(!is.null(pathVar) & pathExpand)
				c(
					list(escape = -match(pathVar, colnames(data))),
					# expand the variable
					list(expandVar = pathVar)
				)
		)
	}

	# ID column non visible:
	# if not specified in input columns
	# or added in the pathVar column
	colsNonVisibleExtra <- c(
		# remove pathVar if saved in pathVar column
		if(usePathVar & !pathExpand)	pathVar,
		# key-columns not displayed
		if(!all(keyVar %in% tableVarsInit))	setdiff(keyVar, tableVarsInit)
	)
	if(length(colsNonVisibleExtra) > 0){
		tablePars$nonVisibleVar <- unique(c(tablePars$nonVisibleVar, colsNonVisibleExtra))
	}
	tablePars$colnames <- setNames(names(tableLab), tableLab)
	
	# use idVar column in DT (for filter/sortering), and display it with hyperlink
	if(usePathVar & !pathExpand){
		
		iPath <- match(pathVar, tableVars)-1
	
		filterHyperlinkJS <- JS("function(data, type, row){",
			 "  if(type === 'display'){",
			 paste0("var link = '<a href=\"' + row[", iPath, 
				"] + '\" target=\"_blank\">' + data + '</a>';"),
			 "    return link;",
			 "  } else {",
			 "    return data;",
			 "  }",
			 "}"
		)
		
		tablePars$options$columnDefs <- c(tablePars$options$columnDefs,
			list(list(targets = match(idVar, tableVars)-1, render = filterHyperlinkJS))
		)
		
	}
	
	# build shared data
	dataTableSharedData <- crosstalk::SharedData$new(
		data = data, 
		key = if(!is.null(keyVar))	varToFm(keyVar), 
		group = id
	)
	
	# create table
	argsClinDT <- c(list(data = dataTableSharedData), tablePars)
	table <- do.call(getClinDT, argsClinDT)
	
	if(tableButton){
		
		attributes(table)$metadata <- list(
			button = tableButton, 
			buttonTitle = "Click to show or hide the data associated to the plot"
		)
		
	}
	
	return(table)
	
}