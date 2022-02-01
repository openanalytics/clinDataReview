# Create temporary yaml file
tmpdir <- tempdir()
library(yaml)

tmpYamlFile <- tempfile(
    pattern = "file", tmpdir = tempdir(), fileext = ".yml"
)
listArgs <- list(
    pathSDTMs = "path/To/SDTM",
    pathSDTMReformat = "path/To/SDTMReformat",
    dateTime = "20200101",
    datasetInfo = list(
        list(
            column1 = "ex.xpt",
            column2 = "20200101"
        ),
        list(
            column1 = "sl.xpt",
            column2 = "20200101",
            column3 = "OK"
        )
    )
)
write_yaml(
    listArgs,
    file = tmpYamlFile
)

# Run metadata
# Note: the 'datasetInfo' can also contain empty elements
getMetadata(filePath = tmpYamlFile)
