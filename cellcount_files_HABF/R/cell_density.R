#' Cell density
#'
#' Takes count results from processed images to calculate cell density based
#' on camera field of view settings (inputted by end-user), number of images, filter area, volumes,
#' and amount of cells filtered that the image is visualizing.
#'
#' @param x count results from processed image
#' @param FOV field of view from camera specs in square millimeters
#' @param images total number of fields
#' @param filtration.area filtration area in square millimeters
#' @param volume volume filtered per field in mL
#' @param total.volume total sample volume in mL
#'
#' @return Calculates the density of cells within your original sample, based on the inputted data
#' @export
#'
#' @examples Cell.Count <- data.frame(Image_File_Name = character(0), Cell_Count = numeric(0))
#' @examples images <- list.files("C:/Users/Desktop/Test_Images/",pattern = "tif", full.name = T)
#' @examples images_names <- list.files("C:/Users/Desktop/Test_Images/",pattern = "tif", full.name = F)
#' @examples grey_images <- greyscale(images,contrast=2)
#' @examples images_mapped <- mapped(grey_images,threshold=0.3)
#' @examples imagesConverted <- image_convert(imagesMapped[[1]],w=10,h=10,offset=0.001,areathresh=50,tolerance=0.8,ext=1)
#' @examples count <- countCells(imagesConverted)
#' @examples Cell.Count[nrow(Cell.Count) + 1, ] <- c(imgNames[[1]], count)
#' @examples Cell.Count$Cell_Count<-as.numeric(Cell.Count$Cell_Count)
#' @examples cell.total <- as.numeric(Cell.Count$Cell_Count)
#' @examples cell.total <- sum(cell.total)
#' @examples cell_density(cell.total,FOV = 0.00084,images = 10,filtration.area = 213.8,volume = 1,total.volume = 2)
#' 

cell_density <- function(x, FOV = 0.0726, images = 10, filtration.area = 213.8, volume = 1, total.volume = 5) {
  MCF <- filtration.area / FOV
  N <- x / images
  cyano <- (MCF * N) / volume
  cyano_total <- cyano * total.volume
  return(cyano_total)
}
