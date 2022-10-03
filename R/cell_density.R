#' Cell density
#'
#' Takes count results from processed images to calculate cell density based
#' on camera field of view settings (inputted by end-user) and amount of cells
#' filtered that the image is visualizing.
#'
#' @param x count results from processed image
#' @param FOV field of view from camera specs in mm2
#' @param images total number of fields
#' @param filtration.area filtration area in mm2
#' @param volume volume filtered per field in mL
#' @param total.volume total sample volume in mL
#'
#' @examples

cell_density <- function(x, FOV = 0.0726, images = 10, filtration.area = 213.8, volume = 1, total.volume = 5) {
  MCF <- filtration.area / FOV
  N <- x / images
  cyano <- (MCF * N) / volume
  cyano_total <- cyano * total.volume
  return(cyano_total)
}
