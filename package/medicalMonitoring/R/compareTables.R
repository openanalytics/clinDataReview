
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
#' @param patientProfilePath
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
		patientProfilePath = NULL
) {
	
	if(length(referenceVars) == 0) stop("Provide variables for the comparison, 'referenceVars' is empty.")
	if(length(changeableVars) == 0) stop("Provide variables to evaluate, 'changeableVars' is empty.")
	
	# Extract parameters
	varsToUse <- c(referenceVars, changeableVars)
	
	if(is.null(labelVars)) {		
		warning("No 'labelVars' specified. The 'referenceVars' and 'changeableVars' names will be used instead.")
		labelVars <- varsToUse
	} else {
		labelVars <- labelVars
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
			patientProfilePath = patientProfilePath
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
#' @author Michela Pasetto
#' @export 
formatComparisonDF <- function(comparisonDF, labelVars, patientProfilePath) {
	
	comparisonDF$Type <- gsub("[+]", "Addition", gsub("=", "Change", gsub("[-]", "Removal", comparisonDF$grp_diff)))	
	comparisonDF$Version <- gsub("[+]", "Current", gsub("[-]", "Previous", comparisonDF$chng_type))
	
	comparisonDF$patientProfilePath <- paste0(
			patientProfilePath,
			"subjectProfile-", 
			sub("/", "-", comparisonDF$USUBJID), ".pdf"
	)
	
	comparisonDF$USUBJID <- with(
			comparisonDF,
			paste0(
					'<a href="', patientProfilePath, 
					'" target="_blank">', USUBJID, '</a>'
			)
	)
	
	columnsToRemove <- c("grp", "chng_type", "chng_type_diff", "Type", "Version", "patientProfilePath")
	
	# Make comparison table and rename columns
	comparisonTable <- comparisonDF[,
			c(
					"Type", "Version",
					colnames(comparisonDF)[
							- which(colnames(comparisonDF) %in% columnsToRemove)
					]
			)
	]
	colnamesVars <- labelVars[colnames(comparisonTable)]	
	
	colnames(comparisonTable) <- c(
			"Type", "Version",
			colnamesVars[!is.na(colnamesVars)],
			colnames(comparisonTable)[grepl("diff", colnames(comparisonTable))]
	)
	
	return(comparisonTable)
}


#' Extract position indices of columns in a data frame
#' 
#' @param df data frame
#' @param vars character vector of columns of the data frame
#' @author Michela Pasetto
#' @export 
getColumnsIdx <- function(df, vars) which(colnames(df) %in% vars)


#' Extract position indices of columns in a data frame and subtract 1
#' 
#' This function is used for correspondance between R and javascript column indices 
#' @inheritParams getColumnsIdx
#' @author Michela Pasetto
#' @export 
getJavaScriptColumnsIdx <- function(...) getColumnsIdx(...) - 1



