
#' Compare tables
#' 
#' Compare tables (data frames) by showing rows that have been added or removed or changed.
#' 
#' @param newData data.frame object representing the new data
#' @param oldData data.frame object representing the old data
#' @param referenceVars character vector of the columns in the data that are the used as reference for the comparison.
#' The \code{referenceVars} need to have the same name in both the new and old data sets.
#' @param changeableVars character vector of the columns in the data for which you want to assess the change,
#' e.g. variables that might have changed from the old to the new data.
#' The \code{changeableVars} need to have the same name in both the new and old data sets.
#' @param labelVars named character vector for renaming the columns of the output table.
#' If NULL, the column names of the data frame are used.
#' @param patientProfilePath string indicating the path to the directory
#' where the patient profiles are stored.
#' @param subjectIDvar string indicating which column in the data represents the unique subject identifier.
#' It is "USUBJID" by default.
#' @return Return an interactive table showing the changes.
#' Additions and removals are green or red colored, respectively.
#' @author Michela Pasetto
#' @importFrom glpgUtilityFct toDTGLPG
#' @importFrom DT formatStyle styleEqual
#' @export 
compareTables <- function(
		newData,
		oldData,
		referenceVars = NULL,
		changeableVars = NULL,
		labelVars = NULL,
		patientProfilePath = NULL,
		subjectIDvar = "USUBJID"
) {
	
	if(length(referenceVars) == 0) stop("Provide variables for the comparison, 'referenceVars' is empty.")
	if(length(changeableVars) == 0) stop("Provide variables to evaluate, 'changeableVars' is empty.")
	
	# Extract parameters
	varsToUse <- c(referenceVars, changeableVars)
	
	if(is.null(labelVars)) {		
		warning("No 'labelVars' specified. The 'referenceVars' and 'changeableVars' names will be used instead.")
		labelVars <- varsToUse
	} else labelVars <- labelVars
	
	if(! subjectIDvar %in% colnames(newData)) {
		stop(sprintf("Unique subject identifier '%s' not available in the new data", subjectIDvar))
	}
	if(! subjectIDvar %in% colnames(oldData)) {
		stop(sprintf("Unique subject identifier '%s' not available in the old data", subjectIDvar))
	}
	
	# Extract data
	oldDataToUse <- oldData[, getColumnsIdx(oldData, varsToUse)]
	newDataToUse <- newData[, getColumnsIdx(newData, varsToUse)]
	
	# Get comparison table
	comparisonDF <- getComparisonDF(
			newData = newDataToUse,
			oldData = oldDataToUse,
			referenceVars = referenceVars
	)
	comparisonTable <- formatComparisonDF(
			comparisonDF,
			labelVars = labelVars,
			patientProfilePath = patientProfilePath,
			subjectIDvar = subjectIDvar
	)
	
	notDisplayVars <- colnames(comparisonTable)[grepl("diff", colnames(comparisonTable))]
	notDiffVars <- colnames(comparisonTable)[! grepl("diff", colnames(comparisonTable))]
	coloringVector <- c("grp_diff", notDisplayVars)	
	
	# Print comparison table
	# use datatable or 'toDTGLPG' ?
	toDTGLPG(
					comparisonTable,
					searchBox = TRUE,
					escape = getJavaScriptColumnsIdx(comparisonTable, "`Unique Subject Identifier`"),
					width = "100%",
					pageLength = 50,
					nonVisible = getJavaScriptColumnsIdx(comparisonTable, notDisplayVars),
					options = list(
							fixedHeader = TRUE
					)
			) %>%			
			
			formatStyle(
					columns = getColumnsIdx(comparisonTable, notDiffVars),
					valueColumns = coloringVector,
					backgroundColor = styleEqual(c("+", "-"), c('lightgreen', 'red'))
			) %>%			
			formatStyle(
					"Version",
					target = "row",
					'font-style' = styleEqual("Previous", "italic"))
	
}


#' Get comparison table
#' 
#' Extract a comparison table (data frame) by showing rows that have been added or removed or changed.
#' @param newData data.frame object representing the new data
#' @param oldData data.frame object representing the old data
#' @param referenceVars character vector of the columns in the data that are the used as reference for the comparison.
#' The \code{referenceVars} need to have the same name in both the new and old data sets.
#' @return A data frame from binding the \code{comparison_df} and \code{comparison_table_diff} from the \code{compareDF} package.
#' @importFrom compareDF compare_df
#' @author Michela Pasetto
#' @export 
getComparisonDF <- function(newData, oldData, referenceVars) {
	
	outputComparison <- compare_df(
			df_new = newData,
			df_old = oldData,
			group_col = referenceVars
	)
	comparedDF <- outputComparison$comparison_df
	comparedDiff <- outputComparison$comparison_table_diff
	colnames(comparedDiff) <- sprintf("%s_diff", colnames(comparedDiff))
	
	comparisonDF <- cbind(comparedDF, comparedDiff)
	
	return(comparisonDF)
	
}

#' Format comparison table
#' 
#' Format a comparison table (data frame) by adding 'Type' and 'Version' columns.
#' @param comparisonDF data.frame object output from getComparisonDF
#' @param labelVars named character vector for renaming the columns of the output table.
#' If NULL, the column names of the data frame are used.
#' @return A data frame formated to be used for the interactive table in \code{compareTables}.
#' The 'Type' column indicates what kind of change has occured in the data whereas
#' the 'Version' column indicates whether the change happens in the old (Previous) or new (Current) data.
#' @importFrom glpgUtilityFct getLabelVar
#' @author Michela Pasetto
#' @export 
formatComparisonDF <- function(
		comparisonDF,
		labelVars,
		patientProfilePath,
		subjectIDvar
) {
	
	comparisonDF$Type <- gsub("[+]", "Addition", gsub("=", "Change", gsub("[-]", "Removal", comparisonDF$grp_diff)))	
	comparisonDF$Version <- gsub("[+]", "Current", gsub("[-]", "Previous", comparisonDF$chng_type))
	
	comparisonDF <- createPatientProfileVar(
			data = comparisonDF,
			patientProfilePath = patientProfilePath,
			subjectIDvar = subjectIDvar
	)
	comparisonDF[, subjectIDvar] <- comparisonDF$patientProfileLink
	
	columnsToRemove <- c("grp", "chng_type", "chng_type_diff", "patientProfilePath", "patientProfileLink")
	columnsToKeep <- colnames(comparisonDF)[- which(colnames(comparisonDF) %in% columnsToRemove)]
	
	# Make comparison table and rename columns
	comparisonTable <- comparisonDF[, columnsToKeep]	
	comparisonTable <- reorderColumns(
			comparisonTable, c(
					"Type" = 1,
					"Version" = 2)
	)	
	colnames(comparisonTable) <- getLabelVar(var = colnames(comparisonTable), labelVars = labelVars)
	
	return(comparisonTable)
}


