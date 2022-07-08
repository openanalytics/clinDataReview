#' Get function code
#' @param fct a R function
#' @return String with function code
getFctCode <- function(fct){
  
  if(is.primitive(fct)){
    fctCode <- capture.output(print(fct))
    fctCode <- sub(".*\\.Primitive\\((.+)\\).*", "\\1", fctCode)
    fctCode <- sub('^"(.+)"$', "\\1", fctCode)
  }else{
    fctCode <- paste(as.character(body(fct)), collapse = "")
  }
  
  return(fctCode)
  
}