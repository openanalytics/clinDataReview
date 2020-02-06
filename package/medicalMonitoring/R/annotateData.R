#' Annotate a SDTM dataset.
#' 
#' Standard annotation variables are available via the 
#' parameter \code{annotType}. Custom dataset/variables of interest
#' are specified via the \code{annotDataset}/\code{annotVar} parameters.
#' @param data Data.frame with input data to annotate.
#' @param annotations Annotations (or list of those) either as a:
#' \itemize{
#' \item{string with standard annotation type, among:
#' \itemize{
#' \item{demographics: }{standard variables from the DM data are extracted}
#' \item{exposed_subjects: }{a logical variable: \code{EXFL} is added to \code{data},
#' identifying exposed subjects, i.e. subjects included in the 'EX'
#' dataset and with non empty and non missing: 'EXSTDTC' are considered}
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
#' @return Annotated \code{data}
#' @export
annotateData <- function(
	data, 
	dataPath = ".", 
	annotations,
	subjectVar = "USUBJID") {

	# if multiple annotations are specified: nested call
	isNest <- ifelse(
		is.list(annotations), 
		!any(c("data", "dataset") %in% names(annotations)),
		length(annotations) > 1
	)
	if(isNest){
		for(par in annotations){	
			data <- annotateData(data = data, dataPath = dataPath, annotations = par)
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
				annotDataPath <- file.path(dataPath, "dm.sas7bdat")
				custom_df <- loadDataADaMSDTM(annotDataPath)[[1]]
				varsDM <- c(subjectVar,"AGE","SEX","RACE","ETHNIC","COUNTRY","SITEID","RFSTDTC","RFENDTC","RFXSTDTC","RFXENDTC")
				varsDM <- intersect(varsDM, colnames(custom_df))
				if(!subjectVar %in% varsDM)
					stop("Demographics data not imported because doesn't contain variable:", subjectVar, ".")
				custom_df <- custom_df[, varsDM]
				data <- leftJoinBase(data, custom_df, by = subjectVar)
			
			},
			
			'exposed_subjects' = {
		
				## Requirements: data needs to have a column with "USUBJID", dataPath needs to be specified and must contain ex.sas7bdat
				## Requirements2: expects that non-exposed individuals have an empty ("") value for EXSTDTC ==> check that only dates and empty values are present
				annotDataPath <- file.path(dataPath, "ex.sas7bdat")
				custom_df <- loadDataADaMSDTM(annotDataPath)[[1]]
				if(!subjectVar %in% colnames(custom_df))
					stop("Exposure data not imported because doesn't contain variable:", subjectVar, ".")
				custom_df <- subset(custom_df, EXSTDTC != "" & !is.na(EXSTDTC))
				data$EXFL <- data[, subjectVar] %in% custom_df[, subjectVar]
				
			}
	
		)
		
	}else{
		
		# extract annotation data
		annotData <- annotations[["data"]]
		annotDataset <- annotations[["dataset"]]
		if(is.null(annotData)){
			if(!is.null(annotDataset)){
				annotDataPath <- file.path(dataPath, paste0(annotDataset, ".sas7bdat"))
				annotData <- loadDataADaMSDTM(annotDataPath)[[1]]
			}else	stop(
						"Custom annotation data should be specified via the 'data' or 'dataset' element",
						" within the 'annotation' list."
					)
			
		}else	annotDataset <- "custom"

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
		}
		
	}
	
	return(data)
	
}
