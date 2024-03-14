#' Matrix mapping conversion
#'
#' Takes a grey-scale/8-bit image to convert to a data matrix,
#' where data under the numerical pixel intensity threshold is removed.
#' This function is needed for the use of the "image_convert" function.
#'
#' @param x a grey scale image variable
#' @param threshold a numerical value of pixel intensity
#'
#' @return Converts an grey scale image to a data matrix, in a pixel by
#' @return pixel format
#' @export
#' 
#' @examples images <- list.files("C:/Users/Desktop/Test_Images/",pattern = "tif", full.name = T)
#' @examples grey_images <- greyscale(images,contrast=2)
#' @examples mapped(grey_images,threshold=0.3)

mapped <- function(x, threshold = 0.3) {
  x <- as.matrix(x)
  x[x < threshold] <- 0
  return(x)
}
