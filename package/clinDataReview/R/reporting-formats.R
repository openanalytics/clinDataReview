#' Clinical data format for bookdown report.
#' 
#' This function is only meant to set sensitive
#' defaults for gitbook.\cr
#' \code{\link[bookdown]{gitbook}} can be used instead.
#' @param logo String, path to the logo. No logo is printed by default.
#' @param logoHeight String, indicating the logo height; 60px height by default.
#' @param split_by String, how the reports should be split,
#' (see help of the \link[bookdown]{gitbook} function)
#' @param config List with config parameters,
#' by default: no sharing and collapsed by section.
#' (see help of the \link[bookdown]{gitbook} function)
#' @param extra_dependencies NULL by default
#' @param css String, path to the css.
#' @param ... Extra parameters passed to the
#' \link[bookdown]{gitbook} function.
#' @return R Markdown output format to pass to \code{\link[bookdown]{render_book}}.
#' @importFrom bookdown gitbook
#' @author Laure Cougnaud
#' @family clinical data reporting
#' @export
gitbook_clinDataReview_report <- function(
    logo = NULL,
    logoHeight = '60px',
    split_by = 'section+number',
    config = list(
        sharing = NULL, 
        toc = list(collapse = 'section')
    ), 
    extra_dependencies = NULL,
    css = NULL,
    ...)
{
  
  if(!is.null(logo)) includes <- addLogoGitbook(
        logo = logo, logoHeight = logoHeight
  ) else includes <- list()
  
  file.copy(
      from = system.file(
          "css", "gitbook.css", package = "clinDataReview"
      ),
      to = "./gitbook.css",
      overwrite = TRUE
  )
  
  bookdown::gitbook(
      ...,
      css = c("./gitbook.css", css),
      split_by = split_by,
      config = config,
      extra_dependencies = extra_dependencies,
      includes = includes
  )
  
}

#' @importFrom htmltools img
#' @importFrom knitr image_uri
addLogoGitbook <- function(logo = NULL, logoHeight = '60px') {
  
  height <- sprintf("height:%s;", logoHeight)
  
  logoStyle <- paste0(
      height,
      "position:absolute;top:50px; right:1%; padding:10px;z-index:200;"
  )
  
  # Create the external file
  logoText <- img(
      src = image_uri(logo), 
      alt = 'logo', 
      style = logoStyle
  )
  
  logoText <- paste0('<script>document.write(\'<div class="logos">', 
      logoText, '</div>\')</script>'
  )
  
  logoHtml <- tempfile(fileext = ".html")
  cat(logoText, file = logoHtml)
  
  includes <- list()
  includes$in_header <- logoHtml
  
  return(includes)
  
}


#' Clinical data format for rmarkdown report.
#' 
#' This function only kept for back-compatibility, 
#' \code{\link[rmarkdown]{html_document}}
#' can be used instead.
#' @param extra_dependencies NULL by default.
#' @param ... Extra parameters passed to the
#' \link[rmarkdown]{html_document} function.
#' @return R Markdown output format to pass to \code{\link[rmarkdown]{render}}.
#' @author Laure Cougnaud
#' @family clinical data reporting
#' @importFrom rmarkdown html_document
#' @export
html_clinDataReview_report <- function(
    extra_dependencies = NULL,
    ...){
  
  rmarkdown::html_document(
      ...,
      extra_dependencies = extra_dependencies
  )
  
}


