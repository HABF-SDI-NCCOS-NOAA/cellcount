#' Border conversion
#'
#' Takes images to predict cells along a predetermined border in order
#' to count cells along the predetermined border. This will be used to help
#' determine those cells to "remove" them from the count function "countCells".
#'
#' @param x An image file
#' @param y Dimming variable (using the 'dim' function from EBImage)
#' @return Analyzing the cells on the border of \code{x}

border <- function(x, y) {
  c(x[1:y[1], 1], x[1:y[1], y[2]], x[1, 1:y[2]], x[y[1], 1:y[2]])
}
