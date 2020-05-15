#' Create a 'monitoring table', associated to a plot.
#' 
#' Interactive table is created, with the possibility to have
#' clickeable link to patient-specific report, and included
#' within a button.
#' @param keyLab String with label for \code{keyVar}.
#' @param tableButton Logical, if TRUE (by default)
#' the table is included within an HTML button.
#' @param tableVars,tableLab Character vector with variables to be included 
#' in the table; and associated labels.
#' @param tablePars List with parameters passed to the
#' \code{\link[glpgUtilityFct]{toDTGLPG}} function.
#' @param verbose Logical, if TRUE (FALSE by default) progress messages are printed
#' in the current console.
#' @inheritParams formatDataForPlotMonitoring
#' @inheritParams medicalMonitoring-common-args
#' @inheritParams medicalMonitoring-common-args-summaryStatsVis
#' @return \code{\link[DT]{datatable}}
#' @author Laure Cougnaud
#' @importFrom glpgUtilityFct toDTGLPG getLabelVar
#' @importFrom plotly highlight_key
#' @importFrom stats as.formula
#' @export
tableMonitoring <- function(
	data, 
	idVar = "USUBJID", idLab = getLabelVar(idVar, labelVars = labelVars),
	keyVar = NULL, keyLab = getLabelVar(keyVar, labelVars = labelVars),
	pathVar = NULL, pathLab = getLabelVar(pathVar, labelVars = labelVars),
	pathExpand = FALSE,
	tableVars,
	tableLab = getLabelVar(tableVars, labelVars = labelVars),
	tableButton = TRUE, tablePars = list(),
	id = paste0("plotMonitoring", sample.int(n = 1000, size = 1)),
	labelVars = NULL,
	verbose = FALSE){
	
	tableVarsInit <- tableVars

	# add key/id in variables to display
	# (used for linking plot <-> table and for the path)
	tableVars <- unique(c(tableVars, idVar, keyVar))
	tableLab <- c(tableLab, idLab[idVar], keyLab[keyVar])
	tableLab <- tableLab[!duplicated(names(tableLab))]
	
	# add hyperlink in the table:
	if(!is.null(pathVar)){
		
		tableVars <- c(pathVar, tableVars) # add in variables for DT
		
		# create the hyperlink (if not already created)
		if(!pathExpand){
			
			data[, pathVar] <- paste0(
				'<a href="', data[, pathVar], 
				'" target="_blank">', data[, idVar], '</a>'
			)
			
			# include the label from ID var
			tableLab[pathVar] <- idLab
			
		}else{
			tableLab[pathVar] <- pathLab
		}
	}
	
	# retain only specified variables:
	data <- data[, tableVars, drop = FALSE]
	
	# convert character columns to factor
	colCharacter <- sapply(data, function(x)
		is.character(x) & !is.factor(x)
	)
	data[, colCharacter] <- lapply(data[, colCharacter, drop = FALSE], as.factor)
	
	if(!is.null(pathVar)){
		
#		downloadButton <- paste0(
#			'<a title="Download all patient profiles"',
#			'onclick="getPatientProfilesDT(this,',
#			tolower(verbose),
#			');">All</a>'
#		)
#		data[, pathVar] <- paste(downloadButton, data[, pathVar], sep = ", ")
		
		tablePars <- c(tablePars, 
			# escape column with hyperlink
			list(escape = -match(pathVar, colnames(data))),
			# expand the variable
			if(pathExpand)	list(expandVar = pathVar)
		)
	}

	# ID column non visible:
	# if not specified in input columns
	# or added in the pathVar column
	colsNonVisibleExtra <- c(
		# remove idVar if saved in pathVar column
		if(!is.null(pathVar) & !pathExpand)	idVar,
		# key-columns not displayed
		if(!all(keyVar %in% tableVarsInit))	setdiff(keyVar, tableVarsInit)
	)
	if(length(colsNonVisibleExtra) > 0){
		jNonVisible <- which(colnames(data) %in% colsNonVisibleExtra)-1
		tablePars$nonVisible <- unique(c(tablePars$nonVisible, jNonVisible))
	}
	tablePars$colnames <- setNames(names(tableLab), tableLab)
	
	# build shared data
	dataTableSharedData <- highlight_key(
		data = data, 
		key = if(!is.null(keyVar))	varToFm(keyVar), 
		group = id
	)
	
	# create table
	argsToDTGLPG <- c(list(data = dataTableSharedData), tablePars)
	table <- do.call(toDTGLPG, argsToDTGLPG)
	
	if(tableButton){
		
		attributes(table)$metadata <- list(
			button = tableButton, 
			buttonTitle = "Click to show or hide the data associated to the plot"
		)
		
	}
	
	return(table)
	
}