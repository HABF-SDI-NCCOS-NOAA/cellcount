#' Image convert
#'
#' Takes a mapped image (i.e., data matrix) and counts individual cells within an image.
#' This is to be used with images with a single image that contains a single
#' fluorescent dimension of cells. Best used for single-cell type images.
#'
#' @param x an image variable that has been mapped using the "mapped" function
#' @param w width of the moving rectangular window (see EBImage)
#' @param h heigth of the moving rectangular window (see EBImage)
#' @param offset a thresholding offset
#' @param areathresh a numerical value of individual pixel areas of cells - this sets a threshold for what items are removed
#' @param tolerance The minimum height of the object in the units of image intensity between its highest point and the point where it contacts another object (see EBImage)
#' @param ext Radius of the neighborhood in pixels for the detection of neighboring objects. Higher value smooths out small objects (see EBImage)
#'
#' @examples

image_convert <- function(x, w = 17, h = 17, offset = 0.001, areathresh = 50, tolerance= 0.5, ext = 1) {
  image <- thresh(x, w = w, h = h, offset = offset)
  image1 <- fillHull(image)
  image2 <- watershed(distmap(image1), tolerance = tolerance, ext = ext)
  nf <- computeFeatures.shape(image2)
  nr <- which(nf[, "s.area"] < areathresh)
  image3 <- rmObjects(image2, nr)
  return(image3)
}
