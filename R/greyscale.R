#' Grey-scale conversion
#'
#' Takes images to adjust the contrast and converts them to grey-scale/8-bit.
#' This function is needed to use the "mapping" function to properly
#' convert images to data-frames.
#'
#' @param x an image variable
#' @param contrast a numerical value for contrast adjustment
#' 
#' @return An image variable gets converted from RGB scale to grey scale
#' @export
#'
#' @examples images <- list.files("C:/Users/Desktop/Test_Images/",pattern = "tif", full.name = T)
#' @examples greyscale(images,contrast=2)

greyscale <- function(x, contrast = 2) {
  x <- contrast * x
  x <- x[, , 1] + x[, , 2] + x[, , 3]
  x <- x / max(x)
  x <- normalize(x, inputRange = c(0.1, 0.75))
  return(x)
}
