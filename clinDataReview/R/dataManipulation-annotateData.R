#' Annotate a dataset.
#' 
#' Standard annotation variables are available via the 
#' parameter \code{annotType}. Custom dataset/variables of interest
#' are specified via the \code{annotDataset}/\code{annotVar} parameters.
#' @param data Data.frame with input data to annotate.
#' @param dataPath String with path to the data.
#' @param annotations Annotations (or list of those) either as a:
#' \itemize{
#' \item string with standard annotation type, among:
#' \itemize{
#' \item demographics: standard variables from the demographics data (DM or ADSL) are extracted
#' \item exposed_subjects: a logical variable: \code{EXFL} is added to \code{data},
#' identifying exposed subjects, i.e. subjects included in the exposure dataset (EX/ADEX)
#' dataset and with non empty and non missing start date ('EXSTDTC', 'STDY' or 'ASTDY')
#' \item functional_groups_lab: a character variable: 'LBFCTGRP' is added to \code{data}
#' based on standard naming of the parameter code ('PARAMCD' or 'LBTESTCD' variable)
#' }
#' \item list of custom annotation, with:
#' \itemize{
#' \item (optional) annotation dataset, either:
#' \itemize{
#' \item 'dataset': String with name of the annotation dataset,
#'  e.g. 'ex' to import data from the file: '[dataset].sas7bdat'in \code{dataPath}
#' \item 'data': Data.frame with annotation dataset
#' }
#' The input \code{data} is used if 'data' and 'dataset' are not specified.
#' \item 'vars': Either:
#' \itemize{
#' \item Character vector with variables of interest from annotation dataset.
#' If not specified, all variables of the dataset are considered.
#' \item String with new variable name computed from \code{varFct}
#' }
#' \item 'varFct': (optional) Either:
#' \itemize{
#' \item function of \code{data} or string containing such function (e.g. 'function(data) ...')
#' \item string containing manipulations from column names of \code{data} (e.g. 'col1 + col2')
#' }
#'  used to create a new variable specified in \code{vars}.
#' \item 'filters': (optional) Filters for the \strong{annotation dataset}, 
#' see \code{filters} parameter of \code{\link{filterData}}.\cr
#' The annotation dataset is first filtered, before being combined to the 
#' input \code{data}, such as only the records retained in the annotation dataset
#' will be annotated in the output \code{data}. Other records will 
#' have missing values in the annotated variables.
#' \item 'varLabel': (optional) label for new variable in case \code{varFct} is specified.
#' \item 'varsBy': (optional) Character vector with variables used to merge input data and
#' the annotation dataset. If not specified:
#' \itemize{
#' \item if an external dataset (\code{dataset}/\code{data}) is specified:
#' \code{subjectVar} is used
#' \item otherwise: annotation dataset and input data are merged by rows IDs
#' }
#' }
#' }
#' @param subjectVar String with subject ID variable, 'USUBJID' by default.
#' @param labelVars Named character vector containing variable labels of \code{data}.
#' This will be updated with the labels of the extra annotation variables
#' (in \code{attr(output, 'labelVars')}).
#' @param labelData (optional) String with label for input \code{data},
#' that will be included in progress messages.
#' @inheritParams clinDataReview-common-args
#' @return Annotated \code{data}.
#' If \code{labelVars} is specified, the output contains an 
#' extra attribute: 'labelVars'
#' containing updated \code{labelVars} (accessible via: in \code{attr(output, 'labelVars')}).
#' @example inst/examples/annotateData-example.R
#' @importFrom clinUtils loadDataADaMSDTM getLabelVar simpleCap getLabelVars
#' @importFrom tools file_path_sans_ext
#' @export
annotateData <- function(
	data, 
	dataPath = ".", 
	annotations,
	subjectVar = "USUBJID",
	verbose = FALSE,
	labelVars = NULL,
	labelData = "data") {

	if(!is.null(annotations)){

		# if multiple annotations are specified: nested call
		isNest <- ifelse(
			is.list(annotations), 
			is.null(names(annotations)),
			length(annotations) > 1
		)
		if(isNest){
			for(par in annotations){	
				data <- annotateData(
					data = data, 
					dataPath = dataPath, 
					annotations = par,
					subjectVar = subjectVar,
					verbose = verbose,
					labelVars = labelVars
				)
				if(!is.null(labelVars))
					labelVars <- attr(data, "labelVars")
			}
			return(data)
		}
		
		# custom 'left-join' function without ordering of rows and columns in x
		leftJoinBase <- function(x, y, by, ...){
			if(any(duplicated(y[, by])))
				warning("Duplicated records in annotation dataset for: ", 
					toString(sQuote(by)), ", this might create replicated rows in the ",
						"input data."
				)
			res <- merge(
				x = x, y = y, 
				all.x = TRUE, all.y = FALSE, # left join
				by = by,
				sort = FALSE, # doesn't sort rows
				...)
			colsX <- colnames(x)
			cols <- c(colsX, setdiff(colnames(res), colsX))
			res <- res[, cols, drop = FALSE]
			return(res)
		}
		
		if(is.character(annotations)){
			
			switch(annotations,
					
				'demographics' = {
			
					## Requirements: data needs to have a column with "USUBJID", dataPath needs to be specified and must contain dm.sas7bdat
					dm <- c("dm", "adsl")
					dataDemoPath <- list.files(path = dataPath, pattern = "^(dm|adsl)\\..+$", ignore.case = TRUE, full.names = TRUE)
					if(length(dataDemoPath) == 0){
						warning(paste(simpleCap(labelData), "is not annotated with demographics data because no such data (ADSL or DM) is available."))
					}else{
						
						dataDemoPath <- dataDemoPath[1]
						dataAnnotAll <- loadDataADaMSDTM(dataDemoPath, verbose = FALSE)
						labelVarsDM <- attr(dataAnnotAll, "labelVars")
						dataAnnot <- dataAnnotAll[[1]]
						
						varsDM <- c("AGE","SEX","RACE","ETHNIC","COUNTRY","CNTRY", "SITEID","RFSTDTC","RFENDTC","RFXSTDTC","RFXENDTC")
						# ADaM-like: 'A[variable]', e.g. ASEX, AAGE, ...
						varsDM <- c(varsDM, paste0("A", varsDM))
						varsDM <- intersect(varsDM, colnames(dataAnnot))
						if(!subjectVar %in% colnames(dataAnnot))
							stop(simpleCap(labelData), " is not annotated with demographics data because doesn't contain variable: ", subjectVar, ".")
						dataAnnot <- unique(dataAnnot[, c(subjectVar, varsDM), drop = FALSE])
						data <- leftJoinBase(x = data, y = dataAnnot, by = subjectVar)
						
						msgDemo <- paste0(
							simpleCap(labelData), " is annotated with demographics data: ", 
							toString(paste0(getLabelVar(var = varsDM, data = dataAnnot, labelVars = labelVarsDM), " (", sQuote(varsDM), ")")),
								", extracted from the ", sQuote(file_path_sans_ext(basename(dataDemoPath))), " dataset."
						)
						if(verbose)	message(msgDemo)
						
						if(!is.null(labelVars))	labelVars <- c(labelVars, labelVarsDM[varsDM])
						
					}
				
				},
				
				'exposed_subjects' = {
			
					## Requirements: data needs to have a column with "USUBJID", dataPath needs to be specified and must contain ex.sas7bdat
					## Requirements2: expects that non-exposed individuals have an empty ("") value for EXSTDTC ==> check that only dates and empty values are present
					dataExPath <- list.files(path = dataPath, pattern = "^(ex|adex)\\..+$", ignore.case = TRUE, full.names = TRUE)
					if(length(dataExPath) == 0){
						warning(paste(simpleCap(labelData), "is not annotated with exposure data, because no such data (ADEX or EX) is available."))
					}else{
						
						dataExPath <- dataExPath[1]
						dataAnnotAll <- loadDataADaMSDTM(dataExPath, verbose = FALSE)
						labelVarsEX <- attr(dataAnnotAll, "labelVars")
						dataAnnot <- dataAnnotAll[[1]]
						if(!subjectVar %in% colnames(dataAnnot))
							stop(simpleCap(labelData), " is not annotated with ",
								"exposure data because doesn't contain the subject variable: ", 
								shQuote(subjectVar), ".")
						
						startVar <- c("EXSTDTC", "STDY", "ASTDY")
						startVar <- intersect(startVar, colnames(dataAnnot))
						
						if(length(startVar) == 0)
							stop(simpleCap(labelData), " is not annotated with exposure",
								" data because doesn't contain a start time variable.")
						
						idxWithST <- which(dataAnnot[, startVar] != "" & !is.na(dataAnnot[, startVar]))
						dataAnnot <- dataAnnot[idxWithST, ]
						
						data$EXFL <- data[, subjectVar] %in% dataAnnot[, subjectVar]
						
						msgEx <- paste0(
							simpleCap(labelData), " is annotated with exposed subjects, ",
							"extracted based on subjects with non-missing ", 
							getLabelVar(var = startVar, data = dataAnnot, labelVars = labelVarsEX), " (", sQuote(startVar), ")",
							" in the ", sQuote(file_path_sans_ext(basename(dataExPath))), " dataset."
						)
						if(verbose)	message(msgEx)
						
						if(!is.null(labelVars))	labelVars <- c(labelVars, EXFL = "Exposed subjects")
						
					}
					
				},
				
				'functional_groups_lab' = {
					
					varParam <- c("PARAMCD", "LBTESTCD")
					varParam <- intersect(varParam, colnames(data))
					
					if(length(varParam) == 0) {
						
						warning(paste(simpleCap(labelData), "is not annotated with functional groups,",
							"because no variable with laboratory parameter code is available in the data.")
					)
					
					} else {
					
						labGroups <- list(
							"Renal function" = c("UREA","CREAT","CA","URATE","GFR"),
							"Electrolytes" = c("SODIUM","K","BICARB","CL"),
							"Liver function" = c("ALT","AST","ALP","CPK","BILDIR","BILI","PROT","ALB","HGB"),
							"Lipids" = c("CHOL","HDL","LDL","TRIG"),
							"Haematology" = c("BASO", "EOS", "HCT", "HGB", "LYM", "MCH", "MCHC", "MCV", "MONO", "NEUT", "PLAT", "RBC", "WBC")
						)
						labCodeToGroup <- setNames(
							rep(names(labGroups), times = sapply(labGroups, length)), 
							unlist(labGroups)
						)
						labGroupData <- labCodeToGroup[as.character(data[, varParam])]
						labGroupData[is.na(labGroupData)] <- "Other"
						
						labGroupsInData <- intersect(c(names(labGroups), "Other"), unique(labGroupData))
						data$LBFCTGRP <- factor(labGroupData, levels = labGroupsInData)
						
						msgAnnot <- paste0(
							simpleCap(labelData), " is annotated with functional groups, ",
							"based on standard naming of the ",
							getLabelVar(var = varParam, data = data, labelVars = labelVars), " (", 
							sQuote(varParam), ").")
						if(verbose)	message(msgAnnot)
						
						if(!is.null(labelVars))	labelVars <- c(labelVars, LBFCTGRP = "Functional group")
					
					}
					
				},
				
				stop(
					simpleCap(labelData), " is not annotated, because ",
					"'annotations' should be one of: ", 
					toString(sQuote(c("demographics", "exposed_subjects", "functional_groups_lab"))),
					" or custom: contains a 'data'/'dataset' element."
				)
		
			)
			
		}else{
			
			# annotation by:?
			varsBy <- annotations[["varsBy"]]
			
			annotData <- NULL
			
			# extract annotation data
			if("dataset" %in% names(annotations)){
				
				annotDataset <- annotations[["dataset"]]
				annotDataPath <- list.files(
					path = dataPath, 
					pattern = paste0("^(", annotDataset, ")\\..+$"), 
					ignore.case = TRUE, full.names = TRUE
				)
				if(length(annotDataPath) == 0){
					warning(paste(simpleCap(labelData), "is not annotated with",
						"the specified", shQuote(annotDataset), "dataset, because",
						"it is not available in:", dataPath
					))
				}else{
					annotDataAll <- loadDataADaMSDTM(annotDataPath, verbose = FALSE)
					annotData <- annotDataAll[[1]]
					labelVarsAnnot <- attr(annotDataAll, "labelVars")
					
					if(is.null(varsBy))	varsBy <- subjectVar
				}
				
			}else if("data" %in% names(annotations)){
				
				annotDataset <- "custom"
				annotData <- annotations$data
				labelVarsAnnot <- getLabelVars(data = annotData)
				
				if(is.null(varsBy))	varsBy <- subjectVar
				
			}else{
				
				annotDataset <- "current"
				data$idAnnot <- seq_len(nrow(data))
				annotData <- data
				labelVarsAnnot <- labelVars
	
				if(is.null(varsBy))	varsBy <- "idAnnot"
				
			}
			
			if(!is.null(annotData)){
			
				# filter if required:
				annotFilter <- annotations$filters
				if(!is.null(annotFilter)){
					annotData <- filterData(
						data = annotData, 
						filters = annotFilter, 
						returnAll = FALSE,
						labelVars = labelVarsAnnot
					)
					labelVarsAnnot <- attr(annotData, "labelVars")
					msgFilter <- attr(annotData, "msg")
				}
				
				# function to apply to extract new variables
				varFct <- annotations$varFct
				if(!is.null(varFct)){
					
					if(is.null(annotations$vars) || length(annotations$vars) != 1)
						stop("'vars' should be specified and of length 1 for 'varFct':\n", 
							capture.output(varFct))
					
					varNew <- annotations$vars
					if(is.character(varFct)){
						
						varFctToFct <- try(eval(expr = parse(text = varFct)), silent = TRUE)
						if(!inherits(varFctToFct, "try-error")){
							annotData[[varNew]] <- varFctToFct(annotData)
						}else{				
							annotData[[varNew]] <- eval(
								expr = parse(text = varFct), 
								envir = annotData
							)
						}
						msgVarFct <- varFct
						
					}else	if(is.function(varFct)){
						
						annotData[[varNew]] <- varFct(annotData)
						msgVarFct <- getFctCode(varFct)
						
					}else	stop("'varFct' should be a character or a function.")
					
					# set label:
					labelNew <- annotations$varLabel
					if(is.null(labelNew))	labelNew <- msgVarFct
					labelVarsAnnot[varNew] <- labelNew
					
					msgVarFct <- paste("based on:", msgVarFct)
					
					# remove variable in data if already present
					data[[varNew]] <- NULL
					
				}
				
				# if 'vars' not specified, all columns of the annotation dataset are retained:
				annotVar <- annotations$vars
				if(is.null(annotVar))	annotVar <- colnames(annotData)
				annotVar <- setdiff(annotVar, varsBy)
				isAnnotInData <- annotVar %in% colnames(data)
				if(any(isAnnotInData) & is.null(varFct)){
					varAnnotInDataText <- toString(paste0(
						getLabelVar(
							var = annotVar[isAnnotInData], 
							data = annotData, 
							labelVars = labelVarsAnnot
						), 
						" (", 
						sQuote(annotVar[isAnnotInData]), 
						")"
					))
					warning(
						simpleCap(labelData), " is not annotated with variable(s): ", 
						varAnnotInDataText,
						" from the ", sQuote(annotDataset), " dataset",
						" because they are already available in data."
					)
					annotVar <- annotVar[!isAnnotInData]
				}
				if(length(annotVar) > 0){
					
					varsByNotInData <- setdiff(varsBy, colnames(annotData))
					if(length(varsByNotInData) > 0)
						stop(simpleCap(labelData), " is not annotated with ", sQuote(annotDataset), ", because doesn't contain variable:", toString(sQuote(varsByNotInData)), ".")
					
					annotData <- unique(annotData[, unique(c(varsBy, annotVar)), drop = FALSE])
		
					data <- leftJoinBase(x = data, y = annotData, by = varsBy)
										
					msgAnnot <- paste0(
						simpleCap(labelData), " annotated with variable(s): ", 
						toString(paste0(
							getLabelVar(var = annotVar, data = annotData, labelVars = labelVarsAnnot), 
							" (", sQuote(annotVar), ")"
						)),
						" from the ", sQuote(annotDataset), " dataset",
						if(!is.null(annotations$varFct))	paste0(" ", msgVarFct),
						if(!is.null(annotFilter))	paste(" whose", msgFilter),
						if(annotDataset != "current"){
							paste0(
								" based on the variable(s):	", 
								getLabelVar(var = varsBy, data = annotData, labelVars = labelVarsAnnot), 
								" (", sQuote(varsBy), ")"
							)
						},
						"."
					)
					if(verbose)	message(msgAnnot)
					
					if(!is.null(labelVars))	labelVars <- c(labelVars, labelVarsAnnot[annotVar])
					
				}
				
			}
			
			if(annotDataset == "current")
				data[["idAnnot"]] <- NULL
			
		}
		
	}
	
	if(!is.null(labelVars))	
		attr(data, "labelVars") <- labelVars
	
	return(data)
	
}
