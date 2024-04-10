#' Filamentous convert
#'
#' Takes a mapped image (i.e., data matrix) and counts individual cells within an image.
#' This is to be used with images that contains a single
#' fluorescent dimension of cells. Best used for filamentous/colonial-cell type images.
#' images.
#'
#' @param x an image variable that has been mapped using the "mapped" function
#' @param w width of the moving rectangular window (see EBImage)
#' @param h heigth of the moving rectangular window (see EBImage)
#' @param offset a thresholding offset
#' @param areathresh a numerical value regarding the individual pixel areas of cells within each image - this sets a threshold for what items are removed from images
#' @param tolerance The minimum height of the object in the units of image intensity between its highest point and the point where it contacts another object (see EBImage)
#' @param ext Radius of the neighborhood in pixels for the detection of neighboring objects. Higher values smooth out small objects (see EBImage for more details)
#'
#' @return Produces the proper data format for the function "countImages" to produce color-label images.
#' @export 
#'
#' @examples images <- list.files("C:/Users/Desktop/Test_Images/",pattern = "tif", full.name = T)
#' @examples grey_images <- greyscale(images,contrast=2)
#' @examples images_mapped <- mapped(grey_images,threshold=0.3)
#' @examples filamentous_convert(imagesMapped[[1]],w=10,h=10,offset=0.001,areathresh=50,tolerance=0.8,ext=1)


filamentous_convert <- function(x, w = 17, h = 17, offset = 0.001, areathresh = 50, tolerance= 0.5, ext = 1) {
  image <- thresh(x, w = w, h = h, offset = offset)
  image1 <- watershed(distmap(image), tolerance = tolerance, ext = ext)
  image2 <- fillHull(image1)
  nf <- computeFeatures.shape(image2)
  nr <- which(nf[, "s.area"] < areathresh)
  image3 <- rmObjects(image2, nr)
  return(image3)
}
