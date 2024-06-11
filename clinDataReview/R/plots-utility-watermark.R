#' Get a watermark, to be included in a clinical visualization
#' @param label String with label for the watermark, 'EXPLORATORY' by default.
#' @param file (optional) String with path to a file where the watermark will be
#' exported to.
#' @param color String with color for the watermark, 'lightgrey' by default.
#' @param fontsize Integer with fontsize for the watermark, 20 by default.
#' @inheritParams clinDataReview-common-args
#' @return The \code{file} with the watermark (invisibly).
#' @export
#' @importFrom ggplot2 annotation_custom ggplot theme_void ggsave
#' @importFrom grid textGrob gpar
#' @author Laure Cougnaud, Kirsten van Hoorde
#' @family watermark helpers
#' @examples
#' # export a watermark to a file
#' watermark <- tempfile(pattern = "watermark", fileext = ".png")
#' getWatermark(file = watermark)
getWatermark <- function(
  label = "EXPLORATORY", color = "lightgrey", fontsize = 20,
  file = NULL, width = 800, height = 500){
  
  watermark <- ggplot2::annotation_custom(
    xmin = -Inf, ymin = -Inf, xmax = Inf, ymax = Inf,
    grob = grid::textGrob(
      label = label,
      gp = grid::gpar(col = color, fontsize = fontsize),
      hjust = 0.5, vjust = 0.5, rot = 45
    )
  )
  gg <- ggplot2::ggplot() + ggplot2::theme_void() + watermark
  
  if(!is.null(file)){
    ggplot2::ggsave(
      filename = file, plot = gg, 
      width = width, height = height, units = "px", 
      bg = "transparent"
    )
  }
  
  return(invisible(file))
  
}

#' Add a watermark to a \code{plotly} object.
#' @inheritParams clinDataReview-common-args
#' @return \code{plotly} object with a watermark (if specified)
#' @importFrom plotly layout
#' @importFrom base64enc dataURI
#' @export 
#' @family watermark helpers
#' @examples
#' watermark <- tempfile(pattern = "watermark", fileext = ".png")
#' getWatermark(file = watermark)
#' addWatermark(pl = plotly::plot_ly(), watermark = watermark)
addWatermark <- function(pl, watermark = NULL){
  
  if(!is.null(watermark)){
    pl <- plotly::layout(
      p = pl, 
      images = list(
        source = base64enc::dataURI(file = watermark),
        layer = "below",
        xref = "paper", x = 0.5, xanchor = "center", sizex = 1, 
        yref = "paper", y = 0.5, yanchor = "middle", sizey = 1, sizing = "contain"
      )
    )
  }
  
  return(pl)
  
}