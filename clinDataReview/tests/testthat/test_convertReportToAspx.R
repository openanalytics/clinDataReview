context("Convert clinical data review report from html to aspx")

 

test_that("Html extensions in filenames and links are modified to .aspx extension", {
  
  # minimal example data
  tmpDir <- tempfile()
  dir.create(tmpDir)
  tmpSubDir <- file.path(tmpDir,'reportA')
  dir.create(tmpSubDir)
  exFile <- file.path(tmpDir,'chapter1.html')
  file.create(exFile)
  writeLines(text= c('Introduction', 'link to html or aspx report reportA/file.chapter1.1.html'), con=exFile)
  file.create(file.path(tmpSubDir, 'chapter1.1.html'))
  file.create(file.path(tmpSubDir, 'table.csv'))
  
  
  convertReportToAspx(reportDir = tmpDir)
  
  
  expect_equal(
    list.files(tmpDir, recursive=TRUE),
    c("chapter1.aspx",  "reportA/chapter1.1.aspx", "reportA/table.csv" ), 
    'Extensions are modified to .aspx')
  
  expect_equal(
    readLines(file.path(file.path(tmpDir,'chapter1.aspx')))[2],
    "link to html or aspx report reportA/file.chapter1.1.aspx", 
    'Links in .html files are modified to .aspx'
  )
  
})


test_that("An error is generated when the report directory is not a directory", {
  expect_error( convertReportToAspx( reportDir = 'noDirectory'),
                "'reportDir' does not exist",
                label = 'Directory should exist')
})


test_that("A warning is generated when the reporting directory does not contain any .html files", {
  
  tmpDir <- tempfile()
  dir.create(tmpDir)
  
  expect_warning( 
    convertReportToAspx( reportDir = tmpDir),
    'No html files present in directory',
    label = 'Directory should contain .html files'
                  )
  
})
