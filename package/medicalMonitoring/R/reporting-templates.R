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
#' to the template is extracted from this file:
#' \itemize{
#' \item{'title' is used as Rd section header}
#' \item{'description' is included in the text}
#' \item{parameters are extracted from the 'properties' tag: }{
#' \itemize{
#' \item{'type': }{object type}
#' \item{'enum' (optional): }{required value for the parameter}
#' \item{'doc' (optional): }{documentation for the parameter}
#' }
#' If a parameter is required, it should be listed in the 'required'
#' tag of the schema (outside of the 'properties' tag).
#' }
#' }
#' @return Character vector with Rd code containing description
#' for all template documents.
#' @importFrom tools file_path_sans_ext
#' @references \href{JSON schema specification}{https://json-schema.org/understanding-json-schema/}
#' @author Laure Cougnaud
createTemplateDoc <- function(){
		
	getBaseName <- function(path, type){
		bn <- file_path_sans_ext(basename(path))
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
			templateSpec <- jsonlite::fromJSON(fileSpecPath)
			title <- templateSpec$title
			desc <- templateSpec$description
			reqParams <- templateSpec$req
			paramsDocList <- lapply(names(templateSpec$properties), function(param){
				pDoc <- templateSpec$properties[[param]]
				pDocVect <- c(
					if(!param %in% reqParams)	"(optional)",
					pDoc$type,
					if(!is.null(pDoc$enum))	paste("with value:", sQuote(pDoc$enum)),
					if(!is.null(pDoc$doc))	pDoc$doc
				)
				pDocText <- paste(pDocVect, collapse = " ")
				getItem(pDocText, name = paste0("\\code{", param, "}"))
			})
			paramsDoc <- getItemize(do.call(c, paramsDocList))
			c(
				paste0("\\section{", ifelse(!is.null(title), title, template), "}{"),
				desc,
				"The following parameters are available:",
				paramsDoc,
				"}"
			)
		}else{
			getItem(template)
		}
	})

	docRox2All <- do.call(c, docRox2)
#	cat(docRox2All)
	
	return(docRox2All)
	
}

#' Rmarkdown templates for medical monitoring
#' 
#' Template reports with standard visualizations/tables
#' available in the package are described here. 
#' 
#' @evalRd createTemplateDoc()
#' 
#' @name medicalMonitoring-templates
NULL