#' Count Images
#'
#' Displays converted microscopy images of distinct cell counts subsequently from the "image_convert" function.
#'
#' @param image A processed image variable from the "image_convert" function
#' @param normalize if TRUE, normalizes the resulting color image
#' @param removeEdgeCells automatically removes cells from along the image
#' border using a true (T) or false (F) input
#' 
#' @return Produces a unique color-label image. Users can use the "display" function to view the image (see EBImage for more details) as well as save the image using the "writeImage" function (see readTIFF for more details).
#' @export
#'
#' @examples images <- list.files("C:/Users/Desktop/Test_Images/",pattern = "tif", full.name = T)
#' @examples grey_images <- greyscale(images,contrast=2)
#' @examples images_mapped <- mapped(grey_images,threshold=0.3)
#' @examples imagesConverted <- image_convert(imagesMapped[[1]],w=10,h=10,offset=0.001,areathresh=50,tolerance=0.8,ext=1)
#' @examples count_images(imagesConverted, normalize = T, removeEdgeCells = T)

count_images <- function(image, normalize = TRUE, removeEdgeCells = TRUE) {
  if (removeEdgeCells) {
    dims <- dim(image)
    border1 <- c(image[1:dims[1], 1], image[1:dims[1], dims[2]], image[1, 1:dims[2]], image[dims[1], 1:dims[2]])
    ids <- unique(border1[which(border1 != 0)])
    inner <- rmObjects(image, ids)
    EBImage::colorLabels(inner, normalize = normalize)
  } else {
    EBImage::colorLabels(image, normalize = normalize)
  }
}
