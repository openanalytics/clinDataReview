##########################################################
## Create data sets for testing 'annotateData' function ##
##########################################################

library(glpgUtilityFct)
library(haven)

pathTestData <- "~/git/GLPGMedicalMonitoring/package/medicalMonitoring/tests/dataTesting"

###############
## Load data ##
data(SDTMDataPelican)
dataDM <- SDTMDataPelican$DM
dataEX <- SDTMDataPelican$EX
labelVars <- attr(SDTMDataPelican, "labelVars")

##########################
## Subset exposure data ##
dataEXsub <- subset(dataEX, grepl("4902-02|4903-03", dataEX$USUBJID) & EXDOSE == 75)

if(! dir.exists(pathTestData)) dir.create(pathTestData, recursive = TRUE)
write_xpt(dataEXsub, file.path(pathTestData, "adex.xpt"))
