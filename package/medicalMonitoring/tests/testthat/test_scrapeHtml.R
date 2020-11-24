context("Test scrape html functions")

test_that("Test saving of scraped tables", {
      
      df <- data.frame(
          A = c(1, 2, 3), B = c(4, 5, 6)
      )
      tmpdir <- tempdir()
            
#      mockr::with_mock(
#          saveScrapeTable = function(table, pathHtml) {
#            
#            pathScrapeTableTxt <- sprintf("%s.txt", pathHtml)
#            write.table(
#                x = table, file = pathScrapeTableTxt,
#                quote = FALSE, sep = '\t', row.names = FALSE
#            )
#          },
#          {
#            saveScrapeTable(
#                table = df, pathHtml = file.path(tmpdir, "ciao")
#            )
#            expect_true(file.exists(file.path(tmpdir, "ciao.txt")))
#          }
#      )
      
      medicalMonitoring:::saveScrapeTable(table = df, pathHtml = file.path(tmpdir, "ciao"))
      files <- list.files(file.path(tmpdir, "scrapedTables"), full.names = TRUE)
      expect_true(file.exists(files[1]))
      expect_true(file.exists(files[2]))
           
    })
