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
#' img <- list.files("data/Karlodinium", pattern = "tif", full.name = T)
#' img1 <- lapply(img[[1]], readTIFF)
#' img2 <- lapply(img1, greyscale, contrast = 2)
#' img3 <- lapply(img2, mapped, threshold = 0.3)
#' img4 <- image_convert(img3, w = 15, h = 25, offset = 0.01, areathresh = 150)
#' final_img <- countImages(img4, normalize = T, removeEdgeCells = T)
#' display(final_img)
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
