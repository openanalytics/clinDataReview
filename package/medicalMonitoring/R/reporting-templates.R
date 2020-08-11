#' Check a configuration file (in _YAML_ format)
#' based on a requirement file in JSON Schema format.
#' @param configFile String with name of the config
#' file of interest in YAML format.
#' @param configSpecFile String with name of the config file
#' containing requirements in JSON Schema format.
#' @return No returned value, an error message is printed
#' in the console if the configuration file doesn't comply
#' to the specified specifications.
#' @author Laure Cougnaud
#' @importFrom yaml read_yaml
#' @importFrom jsonlite toJSON
#' @importFrom jsonvalidate json_validator
#' @export
checkConfigFile <- function(configFile, configSpecFile){
	
	# import parameters from specified config file
	params <- read_yaml(file = configFile)
	
	# convert to JSON file
	paramsJSON <- jsonlite::toJSON(params, auto_unbox = TRUE)

	# import JSON schema file
	# str(jsonlite::fromJSON(configSpecFile))
	jsonSchV <- jsonvalidate::json_validator(configSpecFile)

	# validate output
	resValidate <- jsonSchV(paramsJSON, verbose = TRUE, error = TRUE)
	
}

#' Get path of template medical monitoring report
#' @param file String with name of the template Rmd document
#' @param package String, which package the template should be extracted from,
#' by default the \code{medicalMonitoring} package.
#' @return String with path to the template in the installed \code{medicalMonitoring}
#' package
#' @author Laure Cougnaud
#' @examples
#' \dontrun{
#' pathDivisionTemplate <- getPathTemplate("divisionTemplate.Rmd") # get path template in the package
#' file.copy(from = pathDivisionTemplate, to = ".") # copy to current directlory
#' rmarkdown::render(pathDivisionTemplate) # run file 
#' }
#' @export
getPathTemplate <- function(file, package = "medicalMonitoring"){  
	pathTemplate <- system.file(file.path("template", file), package = package)
	if(!file.exists(pathTemplate))
		warning(paste("Template file:", sQuote(file), "not available in the", 
			sQuote(package), "package."))
	return(pathTemplate)
}

#' Create documentation for medical monitoring template reports
#' available in the 'template' folder of the package.
#' 
#' If a JSON schema file available, the information relative
#' to the template is extracted from this file with the function
#' \code{JSONSchToRd}.
#' @return Character vector with Rd code containing description
#' for all template documents.
#' @references \href{JSON schema specification}{https://json-schema.org/understanding-json-schema/}
#' @author Laure Cougnaud
createTemplateDoc <- function(){
		
	getBaseName <- function(path, type){
		bn <- basename(path)
		if(any(duplicated(bn)))	stop(paste("Duplicated", type, "files."))
		return(bn)
	}
	
	# get template names
	dirTemplatePackage <- system.file("inst", "template", package = "medicalMonitoring")
	
	if(length(dirTemplatePackage) == 1 && dirTemplatePackage == ""){
		return("")	
	}
	
	templateFiles <- list.files(pattern = "*.Rmd$", path = dirTemplatePackage)
	names(templateFiles) <- getBaseName(templateFiles, type = "template")
	# and param specification file
	templateSpecFilePaths <- list.files(pattern = "*.json$", path = dirTemplatePackage, full.names = TRUE)
	names(templateSpecFilePaths) <- getBaseName(templateSpecFilePaths, type = "spec file")
	
	getItem <- function(x, name = NULL){
		itemsRox2Start <- paste0("\\item", 
			if(!is.null(name))	paste0("{", name, ": }"), 
			"{"
		)
		items <- if(length(x) > 1){
			c(itemsRox2Start, x, "}")
		}else	paste0(itemsRox2Start, x, "}")
		return(items)
	}
	getItemize <- function(x){
		return(c("\\itemize{", x, "}"))
	}
	
	docRox2 <- lapply(names(templateFiles), function(template){
				
		fileSpecPath <- templateSpecFilePaths[template]
		if(!is.na(fileSpecPath)){
			
			# template general parameters
			templateSpec <- jsonlite::fromJSON(fileSpecPath)
			JSONSchToRd(jsonSch = templateSpec, title = template)

		}else{
			getItem(template)
		}
	})

	docRox2All <- do.call(c, docRox2)
	
	docRoxParType <- paste0(
		"\\section{Parameter type}{Please note that the type mentioned below ",
		"corresponds to the type in the config file (in YAML/JSON format).",
		"The mapping to R data type is as followed:",
		"\\itemize{",
		"\\item{string: }{character vector of length 1}",
		"\\item{integer: }{integer vector of length 1}",
		"\\item{array: }{vector/list without names}",
		"\\item{object: }{list with names}",
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
#' \item{'title' is used as Rd section header}
#' \item{'description' is included in the text}
#' \item{parameters are extracted from the following 'properties' tag: }{
#' \itemize{
#' \item{'type': }{object type}
#' \item{'doc': }{documentation for the parameter (custom JSON schema tag).
#' This can contain any Roxygen tags, e.g.: \code{\link[package]{function}}}.
#' \item{'pattern' (optional): }{required value for the parameter}
#' \item{'items' (optional): }{JSON schema for the different elements of an 'object'}
#' \item{'minItems'/'maxItems' (optional): }{minimum/maximum number of elements in an 'array'}
#' }
#' If a parameter is required, it should be listed in the 'required'
#' tag of the schema (outside of the 'properties' tag).
#' }
#' }
#' @param jsonSch List with JSON schema, as returned by \code{\link[jsonlite]{fromJSON}}.
#' @param title (optional) String with title, only used if the JSON schema 'title' tag
#' is not available.
#' @return Character vector with R documentation for the specified JSON schema.
#' @author Laure Cougnaud
JSONSchToRd <- function(jsonSch, title = NULL){
	
	paramsReq <- jsonSch$req
	
	# build doc for each parameter
	paramsDocList <- lapply(names(jsonSch$properties), function(param){
				
		jsonSchPropParam <- jsonSch$properties[[param]] 
				
		pDocVect <- c()
		
		# required/optional
		isRequired <- (!param %in% paramsReq)
		if(isRequired)	pDocVect <- c("(optional)", pDocVect)
		
		# type(s)
		pDocVect <- c(pDocVect, paste(jsonSchPropParam$type, collapse = " or "))
		
		# for list ('object'): type of elements in the list
		if(!is.null(jsonSchPropParam$items))
			pDocVect <- c(pDocVect, paste("of", jsonSchPropParam$items$type))
		
		# for array of object, might be nested
		if("items" %in% names(jsonSchPropParam)){
			
			items <- jsonSchPropParam[["items"]]
			pDocItems <- JSONSchToRd(jsonSch = items)
			pDocVect <- c(pDocVect, pDocItems)
			
		}
		
		# for array: min/max number of items
		if(any(c("minItems", "maxItems") %in% names(jsonSchPropParam))){
			itemSize <- paste(
				"of length:", toString(c(
					if(!is.null(jsonSchPropParam$minItems))	paste("at least", jsonSchPropParam$minItems),
					if(!is.null(jsonSchPropParam$maxItems))	paste(jsonSchPropParam$maxItems, "at most")
				))
			)
			pDocVect <- c(pDocVect, itemSize)
		}
		
		# pattern (for fixed parameter)
		if(!is.null(jsonSchPropParam$pattern))
			pDocVect <- c(pDocVect, paste("with value as:\\emph{", sQuote(jsonSchPropParam$pattern), "}"))
		
		# Rd doc
		if(!is.null(jsonSchPropParam$doc))	
			pDocVect <- c(pDocVect, paste("containing", jsonSchPropParam$doc))
		
		# combine all elements to build the doc
		pDocText <- paste(pDocVect, collapse = " ")
				
		pDocName <- paste0(
			"\\code{", 
			if(isRequired)	"\\strong{",
			param, 
			if(isRequired)	"}",
			"}"
		)
		getItem(pDocText, name = pDocName)
		
	})
	
	# combine across params
	paramsDoc <- getItemize(do.call(c, paramsDocList))
	
	if(is.null(title))	title <- jsonSch$title
	desc <- jsonSch$description
	
	res <- c(
		if(!is.null(title))	paste0("\\section{", title, "}{"),
		if(!is.null(desc))	desc,
		"\\cr The following parameters are available:", paramsDoc, 
		if(!is.null(title))	"}"
	)
	
	return(res)
	
}

#' Rmarkdown templates for medical monitoring
#' 
#' Template reports with standard visualizations/tables
#' available in the package are described here.
#' 
#' For each template, required parameters are indicated in \strong{bold}. 
#' @evalRd createTemplateDoc()
#' 
#' @name medicalMonitoring-templates
NULL