#' Image count
#'
#' Takes information from a converted image and counts cells within the image.
#' This uses the "border" function to remove cells along the image border and
#' only count cells within the image. This creates a more accurate cell count.
#'
#' @param x converted image variable
#' @return Counting cells within \code{x}
#'
#' @examples
#' img <- list.files("data/Karlodinium", pattern = "tif", full.name = T)
#' img1 <- lapply(img[[1]], readTIFF)
#' img2 <- lapply(img1, greyscale, contrast = 2)
#' img3 <- lapply(img2, mapped, threshold = 0.3)
#' img4 <- image_convert(img3, w = 15, h = 25, offset = 0.01, areathresh = 150)
#' countCells(img4)
countCells <- function(x) {
  countAll <- max(x)
  dims <- dim(x)
  border1 <- border(x, dims)
  ids <- unique(border1[which(border1 != 0)])
  countInner <- countAll - length(ids)
  return(countInner)
}
