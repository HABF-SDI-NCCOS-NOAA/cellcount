#' Matrix mapping conversion
#'
#' Takes a grey-scale/8-bit image to convert to a data matrix,
#' where data under the numerical pixel intensity threshold is removed.
#' This function is needed for the use of the "image_convert" function.
#'
#' @param x a matrix mapping conversion function
#' @param threshold a numerical value of pixel intensity
#'
#' @examples

mapped <- function(x, threshold = 0.3) {
  x <- as.matrix(x)
  x[x < threshold] <- 0
  return(x)
}
