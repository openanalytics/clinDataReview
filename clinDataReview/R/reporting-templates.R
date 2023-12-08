#' Check a configuration file (in _YAML_ format)
#' based on a requirement file in JSON Schema format.
#' @param configSpecFile String with path to the file
#' containing requirements in JSON Schema format.
#' @param configFile path to the config file
#' @inheritParams clinDataReview-common-args-report
#' @return No returned value, an error message is printed
#' in the console if the configuration file doesn't comply
#' to the specified specifications.
#' @author Laure Cougnaud
#' @importFrom yaml read_yaml
#' @importFrom jsonlite toJSON
#' @importFrom jsonvalidate json_validator
#' @export
checkConfigFile <- function(configFile, configSpecFile, configDir = "./config"){
	
	# import parameters from specified config file
	params <- read_yaml(file = file.path(configDir, configFile))
	
	# convert to JSON file
	paramsJSON <- jsonlite::toJSON(params, auto_unbox = TRUE)

	# import JSON schema file
	# str(jsonlite::fromJSON(configSpecFile))
	jsonSchV <- jsonvalidate::json_validator(configSpecFile, engine = "ajv" )

	# validate output
	resValidate <- jsonSchV(paramsJSON, verbose = TRUE, error = TRUE)
	
}

#' Get path of template clinical data report
#' @param file String with name of the template Rmd document
#' @param package String, which package the template should be extracted from,
#' by default the \code{clinDataReview} package.
#' @return String with path to the template in the installed \code{clinDataReview}
#' package
#' @author Laure Cougnaud
#' @examples
#' \dontrun{
#' pathDivisionTemplate <- getPathTemplate("divisionTemplate.Rmd") # get path template in the package
#' file.copy(from = pathDivisionTemplate, to = ".") # copy to current directlory
#' rmarkdown::render(pathDivisionTemplate) # run file 
#' }
#' @export
getPathTemplate <- function(file, package = "clinDataReview"){  
	pathTemplate <- system.file(file.path("template", file), package = package)
	if(!file.exists(pathTemplate))
		warning(paste("Template file:", sQuote(file), "not available in the", 
			sQuote(package), "package."))
	return(pathTemplate)
}

#' Create documentation for clinical data template reports
#' available in the 'template' folder of the package.
#' 
#' If a JSON schema file available, the information relative
#' to the template is extracted from this file with the function
#' \code{JSONSchToRd}.
#' @param templatePath string with path where the template Rmd reports
#' and associated JSON schema files are stored,
#' by default path of the installed version of the package.
#' This parameter is only for expert use of the package.
#' @return Character vector with Rd code containing description
#' for all template documents.
#' @importFrom tools file_path_sans_ext
#' @references \href{https://json-schema.org/understanding-json-schema/}{JSON schema specification}
#' @author Laure Cougnaud
createTemplateDoc <- function(
	templatePath = system.file("template", package = "clinDataReview")){
		
	getBaseName <- function(path, type){
		bn <- file_path_sans_ext(basename(path))
		if(any(duplicated(bn)))	stop(paste("Duplicated", type, "files."))
		return(bn)
	}
	
	# get template names
	if(length(templatePath) == 1 && templatePath== ""){
		return("")	
	}
	
	templateFiles <- list.files(pattern = "*.Rmd$", path = templatePath)
	names(templateFiles) <- getBaseName(templateFiles, type = "template")
	
	# and param specification file
	templateSpecFilePaths <- list.files(pattern = "*.json$", path = templatePath, full.names = TRUE)
	names(templateSpecFilePaths) <- getBaseName(templateSpecFilePaths, type = "spec file")
	
	docRox2 <- lapply(names(templateFiles), function(template){
		fileSpecPath <- templateSpecFilePaths[template]
		if(!is.na(fileSpecPath)){
			
			# template general parameters
			templateSpec <- jsonlite::fromJSON(fileSpecPath)
			JSONSchToRd(JSONSch = templateSpec, title = paste0(": ", template))

		}else{
			paste0("\\section{", template, "}")
		}
	})

	docRox2All <- do.call(c, docRox2)
	
	docRoxParType <- paste0(
		"\\section{Parameter type}{Please note that the type mentioned below ",
		"corresponds to the type in the config file (in YAML/JSON format).\\cr\n",
		"The mapping to R data type is as followed:",
		"\\itemize{",
		"\\item string: character vector of length 1",
		"\\item integer: integer vector of length 1",
		"\\item array: vector/list without names",
		"\\item object: list with names",
		"}}"
	)
	docRox2All <- c(docRoxParType, docRox2All)
#	cat(docRox2All)
	
	return(docRox2All)
	
}


#' Get R Documentation from a JSON schema.
#' 
#' Note: this function doesn't support the full JSON schema
#' specification, currently only the functionalities
#' required by the templates of the package are implemented.
#' 
#' @section Supported JSON schema tags: 
#' \itemize{
#' \item 'title' is used as Rd section header
#' \item 'description' is included in the text
#' \item parameters are extracted from the following 'properties' tag: 
#' \itemize{
#' \item 'type': object type
#' \item 'doc': documentation for the parameter (custom JSON schema tag).
#' This can contain any Roxygen tags, e.g.: \verb{\link[package]{function}}.
#' \item 'pattern' (optional): required value for the parameter
#' \item 'items' (optional): JSON schema for the different elements of an 
#' 'object'
#' \item 'minItems'/'maxItems' (optional): minimum/maximum number of elements 
#' in an 'array'
#' \item 'enum' (optional): set of possible values
#' \item 'const' (optional): fixed value for the parameter (a.k.a 'constant')
#' }
#' If a parameter is required, it should be listed in the 'required'
#' tag of the schema (outside of the 'properties' tag).
#' }
#' @param JSONSch List with JSON schema, as returned by \code{\link[jsonlite]{fromJSON}}.
#' @param title (optional) String with title.
#' This will combined with the JSON schema 'title' tag if this is specified.
#' is not available.
#' @return Character vector with R documentation for the specified JSON schema.
#' @importFrom utils hasName
#' @author Laure Cougnaud
JSONSchToRd <- function(JSONSch, title = NULL){
	
	paramsReq <- JSONSch$req
	
	getItem <- function(x, name = NULL){
		itemsRox2Start <- paste0("\\item{", 
			if(!is.null(name))	paste0(name, ": ")
		)
		items <- paste0(itemsRox2Start, x, "}")
		return(items)
	}
	getItemize <- function(x){
		return(c("\\itemize{", x, "}"))
	}
	
	getElement <- function(param){
	  
		param <- param[!sapply(param, function(x) 
			is.null(x) || (length(x) == 1 && is.na(x)))
		]
	  
		pDocVect <- c()
	
		# type(s)
		if(hasName(param, "type"))
 			pDocVect <- c(pDocVect, paste(param$type, collapse = " or "))
  	
		# constant
		if(hasName(param, "const")){
			pDocVect <- c(pDocVect, paste("string set to:", shQuote(param$const)))
		}
  	
		# for list ('object'): type of elements in the list
		if(hasName(param, "items")){
			items <- param[["items"]]
			if(hasName(items, "type"))
				pDocVect <- c(pDocVect, paste0("of ", items$type, "(s)"))
			if(hasName(items, "enum"))
				pDocVect <- c(pDocVect, paste("among: \\emph{", 
					toString(shQuote(trimws(items$enum))), "}"))
			# for array of object, might be nested
			pDocVect <- c(pDocVect, JSONSchToRd(JSONSch = items))
		}

		# for array: min/max number of items
		if(any(c("minItems", "maxItems") %in% names(param))){
			itemSize <- paste(
				"of length:", toString(c(
					if(!is.null(param$minItems))	paste("minimum", param$minItems),
					if(!is.null(param$maxItems))	paste("maximum", param$maxItems)
				))
			)
			pDocVect <- c(pDocVect, itemSize)
		}
		
		# for integer/numeric: min/max
		if(any(c("minimum", "maximum") %in% names(param))){
			itemSize <- paste(
				"of length:", toString(c(
				if(!is.null(param$minimum))	paste("minimum", param$minimum),
				if(!is.null(param$maximum))	paste("maximum", param$maximum)
				))
			)
			pDocVect <- c(pDocVect, itemSize)
		}
		
		# pattern (for fixed parameter)
		if(hasName(param, "pattern"))
			pDocVect <- c(pDocVect, paste("with value as:\\emph{", 
				shQuote(param$pattern), "}"))

		if(hasName(param, "enum"))
			pDocVect <- c(pDocVect, paste("among: \\emph{", 
				toString(shQuote(trimws(param$enum))), "}"))

		return(pDocVect)
		
	}
	
	# build doc for each parameter
	if(hasName(JSONSch, "properties")){
		
		paramsDocList <- lapply(names(JSONSch$properties), function(param){
					
			jsonSchPropParam <- JSONSch$properties[[param]] 
					
			pDocVect <- c()
			
			# required/optional
			isRequired <- (param %in% paramsReq)
			if(!isRequired)	pDocVect <- c("(optional)", pDocVect)
			
			if(hasName(jsonSchPropParam, "oneOf")){
				pDocElVect <- apply(jsonSchPropParam$oneOf, 1, function(x){
					docElVect <- getElement(param = as.list(x))
					paste(docElVect, collapse = " ")
				})
				pDocEl <- paste(pDocElVect, collapse = " or ")
			}else{
				pDocEl <- getElement(param = jsonSchPropParam)
			}
			pDocVect <- c(pDocVect, pDocEl)
			
			# combine all elements to build the doc
			pDocText <- paste(pDocVect, collapse = " ")
			
			# Rd doc
			if(hasName(jsonSchPropParam, "doc"))	
				pDocText <- paste0(pDocText, ", ", jsonSchPropParam$doc)
					
			pDocName <- paste0(
				if(isRequired)	"\\strong{",
				"\\code{", 
				param, 
				"}",
				if(isRequired)	"}"
			)
			getItem(x = pDocText, name = pDocName)
			
		})
		
		# combine across params
		paramsDoc <- getItemize(do.call(c, paramsDocList))
		paramsDoc <- paste(paramsDoc, collapse = "\n")
		
	}else	paramsDoc <- NULL
	
	title <- paste(c(JSONSch$title, title), collapse = " ")
	desc <- JSONSch$description
	
	res <- c(
		if(title != "")	paste0("\\section{", title, "}{"),
		if(!is.null(desc))	desc,
		if(!is.null(paramsDoc))	
			paste("\\cr The following parameters are available:", paramsDoc), 
		if(title != "")	"}"
	)
	
	return(res)
	
}

#' Rmarkdown templates for clinical data
#' 
#' Template reports with standard visualizations/tables
#' available in the package are described here.
#' 
#' For each template, required parameters are indicated in \strong{bold}. 
#' @evalRd createTemplateDoc()
#' 
#' @name clinDataReview-templates
#' @return No return value, used for the documentation of 
#' the Rmarkdown template reports contained in the package.
NULL