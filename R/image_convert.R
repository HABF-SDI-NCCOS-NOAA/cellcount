#' Image convert
#'
#' Takes a mapped image (i.e., data matrix) and counts individual cells within an image.
#' This is to be used with images with a single image that contains a single
#' fluorescent dimension of cells. Used for single-cell type images.
#'
#' @param x an image variable that has been mapped using the "mapped" function
#' @param w width of the moving rectangular window (see EBImage)
#' @param h heigth of the moving rectangular window (see EBImage)
#' @param offset a thresholding offset
#' @param areathresh a numerical value of individual pixel areas of cells - this sets a threshold for what items are removed
#'
#' @examples
#' img <- list.files("data/Karlodinium", pattern = "tif", full.name = T)
#' img1 <- lapply(img[[1]], readTIFF)
#' img2 <- lapply(img1, greyscale, contrast = 2)
#' img3 <- lapply(img2, mapped, threshold = 0.3)
#' img4 <- image_convert(img3, w = 15, h = 25, offset = 0.001, areathresh = 150)
image_convert <- function(x, w = 17, h = 17, offset = 0.001, areathresh = 50, tolerance= 0.5, ext = 1) {
  image <- thresh(x, w = w, h = h, offset = offset)
  image1 <- fillHull(image)
  image2 <- watershed(distmap(image1), tolerance = tolerance, ext = ext)
  nf <- computeFeatures.shape(image2)
  nr <- which(nf[, "s.area"] < areathresh)
  image3 <- rmObjects(image2, nr)
  return(image3)
}
