#' Extract variables displayed in the attached table, for
#' each available plotting function of the medical monitoring package.
#' 
#' This function is used in each plotting function of the package 
#' to extract the variable(s) displayed in the table associated to the
#' plot and their associated labels.\cr
#' This can also be used in the template reports, e.g. to extract
#' reference variable(s) for the comparison table functionality
#' in the plot template report.\cr
#' The following framework is used:
#' \itemize{
#' \item{if variables to be displayed in the table (\code{tableVars})
#' are not specified:\cr}{all variables displayed in the plot
#' are selected, based on the plot arguments.\cr
#'  For example: the variables
#' displayed in the x and y axis and for coloring are extracted
#' for the \code{\link{scatterplotMonitoring}} plotting function.\cr
#' Label for these variable(s) are extracted from the associated parameter 
#' (e.g. \code{xLab} for \code{xVar} and so on) or the general
#' parameter for the variable labels (\code{labelVars}) if not specified.
#' }
#' \item{if variables to be displayed in the table (\code{tableVars})
#' are specified:\cr}{these variable(s) are returned.\cr
#' The associated label(s) are extracted from the associated 
#' parameter (\code{tableLab}) or the general
#' parameter for the variable labels (\code{labelVars}) if not specified.}
#' }
#' @param plotFunction String with name of the plotting function,
#' be available in the \code{medicalMonitoring package}.
#' @param plotArgs List with parameters passed to the plotting function.
#' @return Character vector with variable to include in the table,
#' with extra attributes: 'tableLab'/'tablePars' containing the associated
#' labels and the table parameters (passed to \code{\link{tableMonitoring}}).
#' @author Laure Cougnaud
#' @export
getPlotTableVars <- function(plotFunction, plotArgs){

	# in case only a subset of the pars are provided
	# add default values of parameters
	plotFctArgsDef <- formals(plotFunction)
	plotFctArgsNotProvided <- plotFctArgsDef[setdiff(names(plotFctArgsDef), names(plotArgs))]
	plotArgs <- c(plotArgs, plotFctArgsNotProvided)
	
	# extract pars of interest:
	tableVars <- plotArgs$tableVars
	tableLab <- plotArgs$tableLab
	tablePars <- plotArgs$tablePars
	
	# check if par is specified or not
	# 'missing' when used withing plotting fct
	# 'null' when specified via template report
	tableVarsNotSpec <- missing(tableVars) || is.null(tableVars)
			
	getPlotArgsAsVect <- function(elNames){
		elList <- do.call(c, plotArgs[elNames]) # to deal with list(A = list(b = ...), B = list())
		el <- unique(unlist(elList, use.names = FALSE))
		return(el)
	}
	
	if(plotFunction == "scatterplotMonitoring"){
			
		if(tableVarsNotSpec){
			
			aesVar <- getPlotArgsAsVect(c("aesPointVar", "aesLineVar"))
			tableVars <- unique(c(getPlotArgsAsVect(c("idVar", "xVar", "yVar")), aesVar))
			tableLab <- with(plotArgs, 
				c(
					getLabelVar(var = idVar, label = if(!missing(idLab))	idLab, labelVars = labelVars),
					getLabelVar(var = xVar, label = if(!missing(xLab))	xLab, labelVars = labelVars),
					getLabelVar(var = yVar, label = if(!missing(yLab))	yLab, labelVars = labelVars),
					getLabelVar(var = aesVar, label = if(!missing(aesLab))	aesLab, labelVars = labelVars)
				)
			)
		}else{
			tableLab <- with(plotArgs, 
				getLabelVar(tableVars, labelVars = labelVars, 
					label = if(!missing(tableLab))	tableLab
				)
			)
		}
			
	}else	if(plotFunction == "barplotMonitoring"){
			
		if(tableVarsNotSpec){
			tableVars <- with(plotArgs, c(xVar, colorVar, yVar))
			tableLab <- with(plotArgs, 
				c(
					getLabelVar(var = xVar, label = if(!missing(xLab))	xLab, labelVars = labelVars),
					getLabelVar(var = yVar, label = if(!missing(yLab))	yLab, labelVars = labelVars),
					getLabelVar(var = colorVar, label = if(!missing(colorLab))	colorLab, labelVars = labelVars)
				)
			)
		}else{
			tableLab <- with(plotArgs, 
				getLabelVar(tableVars, labelVars = labelVars, 
					label = if(!missing(tableLab))	tableLab
				)
			)
		}
		
	}else	if(plotFunction %in% c("plotCountMonitoring", "treemapMonitoring", "sunburstMonitoring")){
			
		valueVar <- plotArgs$valueVar
		if(tableVarsNotSpec){
			
			tableVars <- with(plotArgs, c(vars, valueVar, colorVar))
			tableLab <- with(plotArgs,  
				c(
					getLabelVar(var = vars, label = varsLab, labelVars = labelVars),
					getLabelVar(var = valueVar, label = valueLab, labelVars = labelVars),
					getLabelVar(var = colorVar, label = colorLab, labelVars = labelVars)
				)
			)
			
		}else{
			
			tableLab <- with(plotArgs, 
				getLabelVar(tableVars, labelVars = labelVars, 
					label = if(!missing(tableLab))	tableLab
				)
			)
			
			if(!valueVar %in% tableVars){
				
				tableVars <- c(tableVars, valueVar)
				tablePars$nonVisibleVar <- c(
					tablePars$nonVisibleVar,
					valueVar
				)
				plotArgs$valueLab <- valueLab <- getLabelVar(valueVar, labelVars = labelVars)
                tableLab <- c(tableLab, setNames(valueLab, valueVar))
			}
			
		}
		
		if("barVar" %in% names(tablePars))
			tablePars <- c(tablePars, list(barVar = valueVar))
		
	}else if(plotFunction == "timeProfileIntervalPlot"){
		
		if(tableVarsNotSpec){
			
			tableVars <- with(plotArgs, c(
				paramGroupVar, paramVar, timeStartVar, timeEndVar, 
				colorVar, timeStartShapeVar, timeEndShapeVar
			))
			tableLab <- with(plotArgs, 
				c(
					getLabelVar(var = paramGroupVar, labelVars = labelVars),
					getLabelVar(var = paramVar, label = paramLab, labelVars = labelVars),
					getLabelVar(var = timeStartVar, label = timeStartLab, labelVars = labelVars),
					getLabelVar(var = timeEndVar, label = timeEndLab, labelVars = labelVars),
					getLabelVar(var = colorVar, label = colorLab, labelVars = labelVars),
					getLabelVar(var = timeStartVar, label = timeStartShapeLab, labelVars = labelVars),
					getLabelVar(var = timeEndShapeVar, label = timeEndShapeLab, labelVars = labelVars)
				)
			)
			
		}else{
			tableLab <- with(plotArgs, 
				getLabelVar(tableVars, labelVars = labelVars, 
					label = if(!missing(tableLab))	tableLab
				)
			)
		}
		
	}else{
		
		stop(paste(
			"Extraction of table parameters for function:", plotFunction,
			"is not yet implemented."
		))
			
	}
			
	tableVars <- unique(tableVars)
	tableLab <- tableLab[tableVars]
	
	res <- tableVars
	attr(res, "tableLab") <- tableLab
	attr(res, "tablePars") <- tablePars
	
	return(res)
	
}
