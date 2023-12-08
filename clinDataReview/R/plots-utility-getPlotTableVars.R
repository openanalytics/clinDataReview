#' Extract variables displayed in the attached table, for
#' each available plotting function of the clinDataReview package.
#' 
#' This function is used in each plotting function of the package 
#' to extract the variable(s) displayed in the table associated to the
#' plot and their associated labels.\cr
#' This can also be used in the template reports, e.g. to extract
#' reference variable(s) for the comparison table functionality
#' in the plot template report.\cr
#' The following framework is used:
#' \itemize{
#' \item if variables to be displayed in the table (\code{tableVars})
#' are not specified:\cr all variables displayed in the plot
#' are selected, based on the plot arguments.\cr
#'  For example: the variables
#' displayed in the x and y axis and for coloring are extracted
#' for the \code{\link{scatterplotClinData}} plotting function.\cr
#' Label for these variable(s) are extracted from the associated parameter 
#' (e.g. \code{xLab} for \code{xVar} and so on) or the general
#' parameter for the variable labels (\code{labelVars}) if not specified.
#' \item if variables to be displayed in the table (\code{tableVars})
#' are specified:\cr these variable(s) are returned.\cr
#' The associated label(s) are extracted from the associated 
#' parameter (\code{tableLab}) or the general
#' parameter for the variable labels (\code{labelVars}) if not specified.
#' }
#' For the functions: \code{\link{plotCountClinData}}, 
#' \code{\link{treemapClinData}}, \code{\link{sunburstClinData}}:
#' value to represent are included in the table and colored with a bar.
#' @param plotFunction String with name of the plotting function,
#' be available in the \code{clinDataReview package}.
#' @param plotArgs List with parameters passed to the plotting function.
#' @return Character vector with variable to include in the table,
#' with extra attributes (passed to \code{\link{tableClinData}}): 
#' \itemize{
#' \item 'tableLab': Named character vector with labels 
#' for the table variables
#' \item 'tablePars' : extra table parameters, only included if specified as 
#' input or specified internally.
#' }
#' labels and the table parameters .
#' @author Laure Cougnaud
#' @export
getPlotTableVars <- function(plotFunction, plotArgs){

	# exception: default args for treemap/sunburst extracted from plotCountClinData
	if(plotFunction %in% c("treemapClinData", "sunburstClinData"))
		plotFunction <- "plotCountClinData"
	
	# in case only a subset of the pars are provided
	# add default values of parameters
	plotFctArgsDef <- formals(plotFunction)
	plotFctArgsNotProvided <- plotFctArgsDef[setdiff(names(plotFctArgsDef), names(plotArgs))]
	plotArgs <- c(plotArgs, plotFctArgsNotProvided)
	
	# extract pars of interest:
	tableVars <- plotArgs$tableVars
	tableLab <- plotArgs$tableLab
	
	# convert object of mode call -> to list
	tablePars <- eval(plotArgs$tablePars)
	
	# check if par is specified or not
	# 'missing' when used withing plotting fct
	# 'null' when specified via template report
	tableVarsNotSpec <- missing(tableVars) || is.null(tableVars)
			
	getPlotArgsAsVect <- function(elNames){
		elList <- do.call(c, plotArgs[elNames]) # to deal with list(A = list(b = ...), B = list())
		el <- unique(unlist(elList, use.names = FALSE))
		return(el)
	}
	
	if(plotFunction == "scatterplotClinData"){
			
		if(tableVarsNotSpec){
			
			aesVar <- getPlotArgsAsVect(c("aesPointVar", "aesLineVar"))
			selectVars <- getPlotArgsAsVect("selectVars")
			tableVars <- unique(c(getPlotArgsAsVect(c("idVar", "xVar", "yVar")), aesVar, selectVars))
			tableLab <- with(plotArgs, 
				c(
					getLabelVar(var = idVar, label = if(!missing(idLab))	idLab, labelVars = labelVars),
					getLabelVar(var = xVar, label = if(!missing(xLab))	xLab, labelVars = labelVars),
					getLabelVar(var = yVar, label = if(!missing(yLab))	yLab, labelVars = labelVars),
					getLabelVar(var = aesVar, label = if(!missing(aesLab))	aesLab, labelVars = labelVars),
					getLabelVar(var = selectVars, label = selectLab, labelVars = labelVars)
				)
			)
		}else{
			tableLab <- with(plotArgs, 
				getLabelVar(tableVars, labelVars = labelVars, 
					label = if(!missing(tableLab))	tableLab
				)
			)
		}
			
	}else	if(plotFunction == "barplotClinData"){
			
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
		
	}else if(plotFunction == "errorbarClinData"){
		
		if(tableVarsNotSpec){
			tableVars <- with(plotArgs, c(
				if(!is.null(xErrorVar))	c(yVar, xVar, xErrorVar),
				if(!is.null(yErrorVar))	c(xVar, yVar, yErrorVar),
				colorVar, selectVars
			))
			tableLab <- with(plotArgs, 
				c(
					getLabelVar(var = xVar, label = if(!missing(xLab))	xLab, labelVars = labelVars),
					getLabelVar(var = xErrorVar, label = if(!missing(xErrorLab))	xErrorLab, labelVars = labelVars),
					getLabelVar(var = yVar, label = if(!missing(yLab))	yLab, labelVars = labelVars),
					getLabelVar(var = yErrorVar, label = if(!missing(yErrorLab))	yErrorLab, labelVars = labelVars),
					getLabelVar(var = colorVar, label = if(!missing(colorLab))	colorLab, labelVars = labelVars),
					getLabelVar(var = selectVars, label = selectLab, labelVars = labelVars)
				)
			)
		}else{
			tableLab <- with(plotArgs, 
				getLabelVar(tableVars, labelVars = labelVars, 
					label = if(!missing(tableLab))	tableLab
				)
			)
		}
				
	}else	if(plotFunction == "boxplotClinData"){
		
		if(tableVarsNotSpec){
			tableVars <- with(plotArgs, c(idVar, xVar, yVar, colorVar, facetVar))
			tableLab <- with(plotArgs, 
				c(
					getLabelVar(var = idVar, label = if(!missing(idLab))	idLab, labelVars = labelVars),
					getLabelVar(var = xVar, label = if(!missing(xLab))	xLab, labelVars = labelVars),
					getLabelVar(var = yVar, label = if(!missing(yLab))	yLab, labelVars = labelVars),
					getLabelVar(var = colorVar, label = if(!missing(colorLab)) colorLab, labelVars = labelVars),
					getLabelVar(var = facetVar, label = if(!missing(facetVar)) facetLab, labelVars = labelVars)
				)
			)
		}else{
			tableLab <- with(plotArgs, 
				getLabelVar(tableVars, labelVars = labelVars, 
					label = if(!missing(tableLab))	tableLab
				)
			)
		}
		
	}else	if(plotFunction %in% c("plotCountClinData", "treemapClinData", "sunburstClinData")){
			
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
				valueLab <- with(plotArgs, getLabelVar(valueVar, labelVars = labelVars))
                tableLab <- c(tableLab, setNames(valueLab, valueVar))
			}
			
		}
				
	}else if(plotFunction == "timeProfileIntervalPlot"){
		
		if(tableVarsNotSpec){
			
			tableVars <- with(plotArgs, c(
				paramGroupVar, paramVar, timeStartVar, timeEndVar, 
				colorVar, timeStartShapeVar, timeEndShapeVar,
				selectVars
			))
			tableLab <- with(plotArgs, 
				c(
					getLabelVar(var = paramGroupVar, labelVars = labelVars),
					getLabelVar(var = paramVar, label = paramLab, labelVars = labelVars),
					getLabelVar(var = timeStartVar, label = timeStartLab, labelVars = labelVars),
					getLabelVar(var = timeEndVar, label = timeEndLab, labelVars = labelVars),
					getLabelVar(var = colorVar, label = colorLab, labelVars = labelVars),
					getLabelVar(var = timeStartShapeVar, label = timeStartShapeLab, labelVars = labelVars),
					getLabelVar(var = timeEndShapeVar, label = timeEndShapeLab, labelVars = labelVars),
					getLabelVar(var = selectVars, label = selectLab, labelVars = labelVars)
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
	if(length(tablePars) > 0)
		attr(res, "tablePars") <- tablePars
	
	return(res)
	
}
