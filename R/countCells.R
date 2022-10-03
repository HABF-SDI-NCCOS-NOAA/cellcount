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

countCells <- function(x) {
  countAll <- max(x)
  dims <- dim(x)
  border1 <- border(x, dims)
  ids <- unique(border1[which(border1 != 0)])
  countInner <- countAll - length(ids)
  return(countInner)
}
