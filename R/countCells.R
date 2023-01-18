#' Image count
#'
#' Takes information from a converted image and counts cells within the image.
#' This remove cells along the image border and only count cells within the image
#' in order to produce a more accurate cell count.
#'
#' @param x converted image variable using the "image_convert" function
#' 
#' @return Counts cells within an processed image
#' @export
#'
#' @examples images <- list.files("C:/Users/Desktop/Test_Images/",pattern = "tif", full.name = T)
#' @examples grey_images <- greyscale(images,contrast=2)
#' @examples images_mapped <- mapped(grey_images,threshold=0.3)
#' @examples imagesConverted <- image_convert(imagesMapped[[1]],w=10,h=10,offset=0.001,areathresh=50,tolerance=0.8,ext=1)
#' @examples countCells(imagesConverted)

countCells <- function(x) {
  countAll <- max(x)
  dims <- dim(x)
  border1 <- c(x[1:dims[1], 1], x[1:dims[1], dims[2]], x[1, 1:dims[2]], x[dims[1], 1:dims[2]])
  ids <- unique(border1[which(border1 != 0)])
  countInner <- countAll - length(ids)
  return(countInner)
}
