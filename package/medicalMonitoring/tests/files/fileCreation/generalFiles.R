###################################
## Create files used for testing ##
##                               ##
## Author: Michela Pasetto       ##
###################################

###########################
## Create bookdown file ###
###########################

testPathBase <- normalizePath(path = "../files")
bookdownFile <- file.path(testPathBase, "_bookdown.yml")
file.create(bookdownFile)
write_yaml(
    list(
        SAS_dir = "path/to/data",
        patient_profile_path = "./patient_profiles/"
    ),
    bookdownFile
)
