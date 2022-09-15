#' Outline conversion
#'
#' Takes grey-scale images to convert to an outline format for the 'propagation'
#' function. Needed for the "multi_image" function.
#' @param x normalized image variable (see example)
#' @examples
#' image_greyF2 <- readImage(paste0(grey_pathF2, grey_filesF2[f]))
#' ImgF2 <- normalize(image_greyF2, inputRange = c(0, 0.75))
#' F2_lab <- outline(ImgF2)
outline <- function(x) {
  x <- thresh(x, w = 15, h = 15, offset = 0.05)
  x <- opening(x, makeBrush(5, shape = "disc"))
  x <- fillHull(x)
  x <- bwlabel(x)
  return(x)
}
