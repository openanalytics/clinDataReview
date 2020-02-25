#' Create a 'monitoring table', associated to a plot.
#' 
#' Interactive table is created, with the possibility to have
#' clickeable link to patient-specific report, and included
#' within a button.
#' @param tableButton Logical, if TRUE (by default)
#' the table is included within an HTML button.
#' @param tableVars Character vector with variables to be included 
#' in the table. By default, ID, x, y and any aesthetic variables
#' are included.
#' @param tablePars List with parameters passed to the
#' \code{\link[glpgUtilityFct]{toDTGLPG}} function.
#' @param id String with general id for the table:
#' \itemize{
#' \item{'SharedData:[id]' is used as \code{group} for the \code{\link[crosstalk]{SharedData}}}
#' \item{'button:[id]' is used as button ID}
#' }
#' If not specified, a random id, as 'tableMonitoringX' is used.
#' @inheritParams medicalMonitoring-common-args
#' @return \code{\link[DT]{datatable}}
#' @author Laure Cougnaud
#' @importFrom glpgUtilityFct toDTGLPG getLabelVar
#' @importFrom plotly highlight_key
#' @importFrom stats as.formula
#' @export
tableMonitoring <- function(data, 
	idVar = "USUBJID", idLab = getLabelVar(idVar, labelVars = labelVars),
	pathVar = NULL,
	tableVars,
	tableLab = getLabelVar(tableVars, labelVars = labelVars),
	tableButton = TRUE, tablePars = list(),
	id){

	if(missing(id))
		id <- paste0("tableMonitoring", sample.int(n = 1000, size = 1))
	
	# add idVar in variables to display (used for linking plot <-> table)
	if(!idVar %in% tableVars){
		tableVars <- c(idVar, tableVars)
		tableLab <- c(setNames(idLab, idVar), tableLab)
	}
	
	# add hyperlink in the table:
	if(!is.null(pathVar)){
		
		data[, "linkVar"] <- paste0(
			'<a href="', data[, pathVar], 
			'" target="_blank">', data[, idVar], '</a>'
		)
		
		tableVars <- c("linkVar", tableVars) # add in variables to display
		tableLab["linkVar"] <- tableLab[idVar] # add label
		
	}
	
	# retain only specified variables:
	data <- data[, tableVars, drop = FALSE]
	
	# escape column with hyperlink
	idxUrlVar <- which(colnames(data) == "linkVar")
	if(!is.null(pathVar)){
		# escape column with URL
		tablePars <- c(tablePars, list(escape = c(tablePars$escape, -idxUrlVar)))
		# ID column non visible (used for the link)
		tablePars$nonVisible <- which(colnames(data) == idVar)-1
	}
	
	tablePars$colnames <- setNames(names(tableLab), tableLab)
	
	# build shared data
	keyFm <- as.formula(paste("~", idVar))
	group <- paste0("SharedData:", id)
	dataTableSharedData <- highlight_key(data = data, key = keyFm, group = group)
	
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