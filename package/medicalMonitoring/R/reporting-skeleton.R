
reportSkeleton <- function(dirName) {
  
  if(! dir.exists(dirName)) stop("Directory '", dirName, "' does not exist.")
  if(length(list.files(dirName)) > 0) warning("'", dirName, "' is not empty. Files might be overwritten.")
  
  # create data folder
  dirData <- file.path(dirName, "data")
  if(! grepl("data", list.files(dirName))) dir.create(dirData)
  
  # move data from clinUtils in xpt format
  
  # create metadata in data folder
  createExampleMetadata(dirData)
  
  
}


#' @importFrom yaml write_yaml
createExampleMetadata <- function(dirName) {
  
  fileName <- file.path(dirName, "metadata.yml")
  
  write_yaml(
      list(
          pathSDTMs = dirName,
          dateTimeSDTMcreation = "20210101",
          datasetInfo = list(
              list(
                  dataset = "ex.xpt",
                  datetime = "20210101",
                  status = "Checking ongoing"
              ),
              list(
                  dataset = "sl.xpt",
                  datetime = "20210101",
                  status = "Checked",
                  comments = "Nothing to report"
              )
          )
      ),
      fileName
  )
  
}