#' Border conversion
#'
#' Takes images to predict cells along a predetermined border in order
#' to count cells along the predetermined border. This will be used to help
#' determine those cells to "remove" them from the count function "countCells".
#'
#' @param x An image file
#' @param y Dimming variable (using the 'dim' function from EBImage)
#' @return Analyzing the cells on the border of \code{x}
#' @examples
#' img0 <- list.files("data/Anabena 2021/UV", pattern = "tif", full.name = TRUE)
#' img1 <- lapply(img0, readTIFF)
#' img2 <- lapply(img1, greyscale)
#' img3 <- lapply(img2, mapped)
#' img4 <- image_convert(img3)
#' dims <- dim(img4)
#' border(img4, dims)
border <- function(x, y) {
  c(x[1:y[1], 1], x[1:y[1], y[2]], x[1, 1:y[2]], x[y[1], 1:y[2]])
}
