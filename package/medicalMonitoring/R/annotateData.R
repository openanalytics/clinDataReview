#' Annotate a SDTM dataset.
#' 
#' Standard annotation variables are available via the 
#' parameter \code{annotType}. Custom dataset/variables of interest
#' are specified via the \code{annotDataset}/\code{annotVar} parameters.
#' @param data Data.frame with input data to annotate.
#' @param dataPath String with path to the data.
#' @param annotations Annotations (or list of those) either as a:
#' \itemize{
#' \item{string with standard annotation type, among:
#' \itemize{
#' \item{demographics: }{standard variables from the demographics data (DM or ADSL) are extracted}
#' \item{exposed_subjects: }{a logical variable: \code{EXFL} is added to \code{data},
#' identifying exposed subjects, i.e. subjects included in the exposure dataset (EX/ADEX)
#' dataset and with non empty and non missing start date ('EXSTDTC', 'STDY' or 'ASTDY')}
#' \item{functional_groups_lab: }{a character variable: 'PARCATFCT' is added to \code{data}
#' based on standard naming of the parameter code ('PARAMCD' or 'LBTESTCD' variable)
#' }
#' }
#' }
#' \item{list of custom annotation, with:
#' \itemize{
#' \item{'dataset' (option 1): }{String with name of the annotation dataset,
#'  e.g. 'ex' to import data from the file: '[dataset].sas7bdat'in \code{dataPath}}
#' \item{'data' (option 2): }{Data.frame with annotation dataset}
#' \item{'vars': }{Character vector with variables of interest from annotation dataset.
#' If not specified, all variables of the dataset are considered.}
#' \item{'filters': }{Filters for the annotation dataset, 
#' see \code{filters} parameter of \code{\link{filterData}}
#' }
#' }
#' }
#' }
#' @param subjectVar String with subject ID variable, 'USUBJID' by default.
#' @param labelVars Named character vector containing variable labels of \code{data}.
#' This will be updated with the labels of the extra annotation variables
#' (in \code{attr(output, 'labelVars')}).
#' @param verbose Logical, if TRUE (FALSE by default) progress messages are printed
#' in the current console.
#' @inheritParams medicalMonitoring-common-args
#' @return Annotated \code{data}.
#' If \code{labelVars} is specified, the output contains an 
#' extra attribute: 'labelVars'
#' containing updated \code{labelVars} (accessible via: in \code{attr(output, 'labelVars')}).
#' @example inst/examples/annotateData-example.R
#' @importFrom glpgUtilityFct loadDataADaMSDTM getLabelVar
#' @importFrom tools file_path_sans_ext
#' @export
annotateData <- function(
	data, 
	dataPath = ".", 
	annotations,
	subjectVar = "USUBJID",
	verbose = FALSE,
	labelVars = NULL) {

	# if multiple annotations are specified: nested call
	isNest <- ifelse(
		is.list(annotations), 
		!any(c("data", "dataset") %in% names(annotations)),
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
	leftJoinBase <- function(x, y, ...){
		res <- merge(
			x = x, y = y, 
			all.x = TRUE, all.y = FALSE, # left join
			sort = FALSE, # doesn't sort rows
			...)
		colsX <- colnames(x)
		cols <- c(colsX, setdiff(colnames(res), colsX))
		res <- res[, cols, drop = FALSE]
		return(res)
	}
	
	if(is.character(annotations)){
	
		annotations <- match.arg(annotations, choices = c("demographics", "exposed_subjects"))
		
		switch(annotations,
				
			'demographics' = {
		
				## Requirements: data needs to have a column with "USUBJID", dataPath needs to be specified and must contain dm.sas7bdat
				dm <- c("dm", "adsl")
				dataDemoPath <- list.files(path = dataPath, pattern = "^(dm|adsl)\\..+$", ignore.case = TRUE, full.names = TRUE)
				if(length(dataDemoPath) == 0){
					warning("Demographics annotation not included, because no data (ADSL or DM) is available.")
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
						stop("Demographics data not imported because doesn't contain variable:", subjectVar, ".")
					dataAnnot <- dataAnnot[, c(subjectVar, varsDM)]
					data <- leftJoinBase(data, dataAnnot, by = subjectVar)
					
					msgDemo <- paste0("Demographics data: ", 
						toString(paste0(getLabelVar(var = varsDM, data = dataAnnot, labelVars = labelVarsDM), " (", sQuote(varsDM), ")")),
							" is extracted from the ", sQuote(file_path_sans_ext(basename(dataDemoPath))), " dataset."
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
					warning("Exposure annotation not included, because no data (ADEX or EX) is available.")
				}else{
					
					dataExPath <- dataExPath[1]
					dataAnnotAll <- loadDataADaMSDTM(dataExPath, verbose = FALSE)
					labelVarsEX <- attr(dataAnnotAll, "labelVars")
					dataAnnot <- dataAnnotAll[[1]]
					if(!subjectVar %in% colnames(dataAnnot))
						stop("Exposure data not imported because doesn't contain variable:", subjectVar, ".")
					
					startVar <- c("EXSTDTC", "STDY", "ASTDY")
					startVar <- intersect(startVar, colnames(dataAnnot))
					idxWithST <- which(dataAnnot[, startVar] != "" & !is.na(dataAnnot[, startVar]))
					dataAnnot <- dataAnnot[idxWithST, ]
					
					data$EXFL <- data[, subjectVar] %in% dataAnnot[, subjectVar]
					
					msgEx <- paste0("Exposed subjects extracted based on subjects with non-missing ", 
						getLabelVar(var = startVar, data = dataAnnot, labelVars = labelVarsEX), " (", sQuote(startVar), ")",
						" in the ", sQuote(file_path_sans_ext(basename(dataExPath))), " dataset."
					)
					if(verbose)	message(msgEx)
					
					if(!is.null(labelVars))	labelVars <- c(labelVars, EXFL = "Exposed subjects")
					
				}
				
			},
			
			'functional_groups_lab' = {
				
				varParam <- c("PARAMCD", "LBTESTCD")
				varParam <- setdiff(varParam, colnames(data))
				
				if(length(varParam) == 0)
					stop("Functional lab annotation not added because no variable with laboratory parameter code is available in the data.")
				varParam <- varParam[1]
				
				labGroups <- list(
					"Renal function" = c("UREA","CREAT","CA","URATE","GFR"),
					"Electrolytes" = c("SODIUM","K","BICARB","CL"),
					"Liver function" = c("ALT","AST","ALP","CPK","BILDIR","BILI","PROT","ALB","HGB"),
					"Lipids" = c("CHOL","HDL","LDL","TRIG"),
					"Haematology" = c("BASO", "EOS", "HCT", "HGB", "LYM", "MCH", "MCHC", "MCV", "MONO", "NEUT", "PLAT", "RBC", "WBC")
				)
				labCodeToGroup <- setNames(rep(names(labGroups), times = sapply(labGroups, length)), unlist(labGroups))
				labGroupData <- varParam[data[, varParam]]
				labGroupData[is.na(labGroupData)] <- "Other"
				
				data$PARCATFCT <- factor(labGroupData)
				
				msgAnnot <- paste0("Data annotated with variable(s): ", 
						getLabelVar(var = annotVar, data = annotData, labelVars = labelVarsAnnot), " (", sQuote(annotVar), ")",
						" in the ", sQuote(annotDataset), " dataset."
				)
				if(verbose)	
					message(
						paste("Functional group is extracted based on standard naming of the ",
							getLabelVar(var = varParam, data = annotData, labelVars = labelVarsAnnot), " (", sQuote(varParam), ").")
					)
				
				if(!is.null(labelVars))	labelVars <- c(labelVars, PARCATFCT = "Functional group")
				
				
			}
	
		)
		
	}else{
		
		# extract annotation data
		annotData <- annotations[["data"]]
		annotDataset <- annotations[["dataset"]]
		if(is.null(annotData)){
			if(!is.null(annotDataset)){
				annotDataPath <- file.path(dataPath, paste0(annotDataset, ".sas7bdat"))
				annotDataAll <- loadDataADaMSDTM(annotDataPath, verbose = FALSE)
				labelVarsAnnot <- attr(annotDataAll, "labelVars")
				annotData <- annotDataAll[[1]]
			}else	stop(
						"Custom annotation data should be specified via the 'data' or 'dataset' element",
						" within the 'annotation' list."
					)
			
		}else{
			annotDataset <- "custom"
			labelVarsAnnot <- getLabelVars(data = annotData)
		}

		if(!subjectVar %in% colnames(annotData))
			stop(sQuote(annotDataset), "data not imported because doesn't contain variable:", subjectVar, ".")
		
		# filter if required:
		annotFilter <- annotations$filters
		if(!is.null(annotFilter))
			annotData <- filterData(annotData, filters = annotFilter)
		
		# if 'vars' not specified, all columns of the annotation dataset are retained:
		annotVar <- annotations$vars
		if(is.null(annotVar))	annotVar <- colnames(annotData)
		isAnnotInData <- annotVar %in% colnames(data)
		if(any(isAnnotInData)){
			warning(paste(toString(annotVar[isAnnotInData]), "are already available in data,",
				"so these are not considered."
			))
			annotVar <- annotVar[!isAnnotInData]
		}
		if(length(annotVar) > 0){
			
			annotVar <- unique(c(subjectVar, annotVar))	
			annotData <- annotData[, annotVar]

			data <- leftJoinBase(data, annotData, by = subjectVar)
			
			msgAnnot <- paste0("Data annotated with variable(s): ", 
				getLabelVar(var = annotVar, data = annotData, labelVars = labelVarsAnnot), " (", sQuote(annotVar), ")",
				" in the ", sQuote(annotDataset), " dataset."
			)
			if(verbose)	message(msgAnnot)
			
			if(!is.null(labelVars))	labelVars <- c(labelVars, labelVarsAnnot[annotVar])
			
		}
		
	}
	
	if(!is.null(labelVars))	
		attr(data, "labelVars") <- labelVars
	
	return(data)
	
}
