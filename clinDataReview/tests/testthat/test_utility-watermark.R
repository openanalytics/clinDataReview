context("Create a watermark")

test_that("A file with a watermark is successfully created", {
			
  file <- tempfile(pattern = "watermark", fileext = ".png")
  getWatermark(file = file)
  expect_true(file.exists(file))
	
})

test_that("A watermark is successfully added to a plotly object", {
			
  file <- tempfile(pattern = "watermark", fileext = ".png")
  getWatermark(file = file)
  pl <- plotly::plot_ly(type = "scatter", mode = "marker")
  pl <- addWatermark(pl = pl, watermark = file)
  
  # check that an image has been included below the plot
  plBuild <- plotly::plotly_build(pl)
  expect_equal(
    object = sapply(plBuild$x$layout$images, `[[`, "layer"),
    expected = "below"
  )
			
})