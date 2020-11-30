#' Extract variables displayed in the attached table, for
#' each available plotting function of the medical monitoring package.
#' @param plotFunction String with name of the plotting function.
#' @param plotArgs List with parameters passed to the plotting function.
#' @return Character vector with variable to include in the table,
#' with extra attribute: 'tableLab'/'tablePars' containing variable labels.
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
	tableLabNotSpec <- missing(tableLab) || is.null(tableLab)
			
	if(plotFunction == "scatterplotMonitoring"){
			
		if(tableVarsNotSpec){
			aesVar <- with(plotArgs, unlist(c(aesPointVar, aesLineVar)))
			tableVars <- with(plotArgs, unique(c(idVar, xVar, yVar, aesVar)))
			tableLab <- with(plotArgs, 
				c(
					getLabelVar(var = idVar, label = xLab, labelVars = labelVars),
					getLabelVar(var = xVar, label = xLab, labelVars = labelVars),
					getLabelVar(var = yVar, label = yLab, labelVars = labelVars),
					getLabelVar(var = aesVar, label = aesLab, labelVars = labelVars)
				)
			)
		}else	if(tableLabNotSpec){
			tableLab <- with(plotArgs, getLabelVar(tableVars, labelVars = labelVars))
		}
			
	}else	if(plotFunction == "barplotMonitoring"){
			
		if(tableVarsNotSpec){
			tableVars <- with(plotArgs,  c(xVar, colorVar, yVar))
			tableLab <- setNames(with(plotArgs, c(xLab, colorLab, yLab)), tableVars)
		}else	if(tableLabNotSpec){
			tableLab <- with(plotArgs, getLabelVar(tableVars, labelVars = labelVars))
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
			
			if(tableLabNotSpec)
				tableLab <- with(plotArgs, glpgUtilityFct::getLabelVar(tableVars, labelVars = labelVars))
			
			if(!valueVar %in% tableVars){
				tableVars <- c(tableVars, valueVar)
				tablePars$nonVisibleVar <- c(
					tablePars$nonVisibleVar,
					valueVar
				)
                tableLabOrig <- tableLab
                tableLab <- c(tableLab, valueVar)
                idxNames <- which(names(tableLab) %in% names(tableLabOrig))
                names(tableLab) <- c(names(tableLab)[idxNames], valueVar)
                plotArgs$valueLab <- valueVar
				tableLab[valueVar] <- with(plotArgs, glpgUtilityFct::getLabelVar(valueLab, labelVars = labelVars))
			}
			
		}
		
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
			
		}else	if(tableLabNotSpec){
			tableLab <- with(plotArgs, getLabelVar(tableVars, labelVars = labelVars))
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
