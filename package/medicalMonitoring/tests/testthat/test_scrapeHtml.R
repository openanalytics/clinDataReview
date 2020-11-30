context("Test scrape html functions")

basePath <- normalizePath(path = "../files")
pathHtml <- list.files(file.path(basePath, "htmlScraping"), full.names = TRUE)

pathHtml <- "~/git/GLPGMedicalMonitoring/package/medicalMonitoring/tests/files/htmlScraping/3-3-demographics.html"

test_that("Errors of scraping html tables", {
      
      expect_error(
          scrapeHtml(),
          "No path to html specified."
      )
      
      expect_error(
          scrapeHtml("pathToDir"),
          "File at .* does not exist."
      )
      
    })

test_that("Test scraping of html table", {
      
      tmpdir <- tempdir()
      expect_silent(
          res <- scrapeHtml(
              pathHtml = pathHtml,
              pathToExportDir = tmpdir,
              fileNameExport = "scrapedTable"
          )
      )
      expect_equal(class(res), "matrix")
      files <- list.files(file.path(tmpdir, "scrapedTables"), full.names = TRUE)
      expect_true(file.exists(files[1]))
      expect_true(file.exists(files[2]))
      
      
    })

test_that("Test reading html files", {
      
      expect_silent(
          res <- readHtml(pathHtml)
      )
      expect_equal(class(res), "list")
      expect_equal(class(res$x), "list")
      expect_equal(class(res$x$data), "matrix")
      expect_equal(class(res$x$container), "character")
      
    })


test_that("Test parsing of JSON object", {
      
      jsonObjList <- readHtml(pathHtml)
      
      expect_silent(
          res <- parseScrapeTable(jsonObj = jsonObjList)
      )
      expect_equal(class(res), "matrix")
      
    })

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
      
      saveScrapeTable(table = df, pathToDir = tmpdir, fileName = "ciao")
      files <- list.files(file.path(tmpdir, "scrapedTables"), full.names = TRUE)
      expect_true(file.exists(files[1]))
      expect_true(file.exists(files[2]))
      
    })

