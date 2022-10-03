#' Count Images
#'
#' Displays converted microscopy images of distinct cell counts subsequently from the image_convert function.
#' Can use the "display" function from EBImage to automatically generate the
#' produced image from this function.
#'
#' @param image A tif image
#' @param normalize if TRUE normalizes the resulting color image
#' @param removeEdgeCells automatically removes cells from along the image
#' border
#'
#' @examples

countImages <- function(image, normalize = TRUE, removeEdgeCells = TRUE) {
  if (removeEdgeCells) {
    dims <- dim(image)
    border1 <- border(image, dims)
    ids <- unique(border1[which(border1 != 0)])
    inner <- rmObjects(image, ids)
    EBImage::colorLabels(inner, normalize = normalize)
  } else {
    EBImage::colorLabels(image, normalize = normalize)
  }
}
