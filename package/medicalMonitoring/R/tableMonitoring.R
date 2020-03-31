#' Create a 'monitoring table', associated to a plot.
#' 
#' Interactive table is created, with the possibility to have
#' clickeable link to patient-specific report, and included
#' within a button.
#' @param tableButton Logical, if TRUE (by default)
#' the table is included within an HTML button.
#' @param tableVars,tableLab Character vector with variables to be included 
#' in the table; and associated labels.
#' @param tablePars List with parameters passed to the
#' \code{\link[glpgUtilityFct]{toDTGLPG}} function.
#' @param verbose Logical, if TRUE (FALSE by default) progress messages are printed
#' in the current console.
#' @inheritParams medicalMonitoring-common-args
#' @return \code{\link[DT]{datatable}}
#' @author Laure Cougnaud
#' @importFrom glpgUtilityFct toDTGLPG getLabelVar
#' @importFrom plotly highlight_key
#' @importFrom stats as.formula
#' @export
tableMonitoring <- function(
	data, 
	idVar = "USUBJID", idLab = getLabelVar(idVar, labelVars = labelVars),
	pathVar = NULL, pathLab = getLabelVar(pathVar, labelVars = labelVars),
	pathExpand = FALSE,
	tableVars,
	tableLab = getLabelVar(tableVars, labelVars = labelVars),
	tableButton = TRUE, tablePars = list(),
	id = paste0("plotMonitoring", sample.int(n = 1000, size = 1)),
	labelVars = NULL,
	verbose = FALSE){
	
	tableVarsInit <- tableVars

	# add idVar in variables to display (used for linking plot <-> table)
	if(!all(idVar %in% tableVars)){
		tableVars <- c(idVar, tableVars)
		tableLab <- c(setNames(idLab, idVar), tableLab)
	}
	
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
	if(!all(idVar %in% tableVarsInit) | (!is.null(pathVar) & !pathExpand)){
		tablePars$nonVisible <- which(colnames(data) %in% idVar)-1
	}
	
	tablePars$colnames <- setNames(names(tableLab), tableLab)
	
	# build shared data
	dataTableSharedData <- highlight_key(
		data = data, 
		key = varToFm(idVar), 
		group = id
	)
	
	# create table
	argsToDTGLPG <- c(list(data = dataTableSharedData), tablePars)
	table <- do.call(toDTGLPG, argsToDTGLPG)
	
	if(tableButton){
		
		idButton <- paste0("button:", id)
#		class(table) <- c("medicalMonitoringTable", class(table))
		
		attributes(table)$metadata <- list(
			button = tableButton, 
			buttonId = idButton,
			buttonTitle = "Click to show or hide the data associated to the plot"
		)
		
	}
	
	return(table)
	
}