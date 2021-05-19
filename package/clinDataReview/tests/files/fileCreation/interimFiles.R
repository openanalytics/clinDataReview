###################################
## Create files used for testing ##
##                               ##
## Author: Michela Pasetto       ##
###################################

######################
## Create rds files ##
######################
testPathBase <- normalizePath(path = "../files")
testPathInterim <- file.path(testPathBase, "interim")

names <- c(
    "adverseEvents-division",
    "index"
)

rdsFiles <- sprintf("%s.rds", names)
file.create(file.path(testPathInterim, rdsFiles))
sessList <- list(knitMeta = sessionInfo())
for(i in 1 : length(rdsFiles)) {
  saveRDS(sessList, file.path(testPathInterim, rdsFiles[i]), version = 2)  
}

#####################
## Create md files ##
#####################
mdFiles <- sprintf("%s.md", names)    
file.create(file.path(intermediateDir, mdFiles))

textIndex <- "---
title: 'Study Name'
date: '2020-12-25'
documentclass: book
output: clinDataReview::gitbook_clinDataReview_report
editor_options:
  chunk_output_type: console
---    
\n\n# Introduction \n\nSome introduction.
"
testConfig <- "# Adverse events \n\nThis is an empty chapter."

testList <- list(
    testConfig = testConfig,
    textIndex = textIndex
)

for(i in 1 : length(mdFiles)) {
  
  writeLines(
      text = testList[[i]],
      con = file(file.path(intermediateDir, mdFiles[i]))
  )
  
}


