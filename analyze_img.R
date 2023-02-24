library(EBImage)
library(tiff)
library(pixmap)
library(raster)
library(beepr)

border <- function(x, y) {
  c(x[1:y[1], 1], x[1:y[1], y[2]], x[1, 1:y[2]], x[y[1], 1:y[2]])
}
cell_density <- function(x, FOV = 0.0726, images = 10, filtration.area = 213.8, volume = 1, total.volume = 5) {
  MCF <- filtration.area / FOV
  N <- x / images
  cyano <- (MCF * N) / volume
  cyano_total <- cyano * total.volume
  return(cyano_total)
}
countCells <- function(x) {
  countAll <- max(x)
  dims <- dim(x)
  border1 <- border(x, dims)
  ids <- unique(border1[which(border1 != 0)])
  countInner <- countAll - length(ids)
  return(countInner)
}
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
greyscale <- function(x, contrast = 2) {
  x <- contrast * x
  x <- x[, , 1] + x[, , 2] + x[, , 3]
  x <- x / max(x)
  x <- normalize(x, inputRange = c(0.1, 0.75))
  return(x)
}
image_convert <- function(x, w = 17, h = 17, offset = 0.001, areathresh = 50, tolerance= 0.5, ext = 1) {
  image <- thresh(x, w = w, h = h, offset = offset)
  image1 <- fillHull(image)
  image2 <- watershed(distmap(image1), tolerance = tolerance, ext = ext)
  nf <- computeFeatures.shape(image2)
  nr <- which(nf[, "s.area"] < areathresh)
  image3 <- rmObjects(image2, nr)
  return(image3)
}
mapped <- function(x, threshold = 0.3) {
  x <- as.matrix(x)
  x[x < threshold] <- 0
  return(x)
}


read_images <- lapply(images, readTIFF)
img_transposed <- lapply(read_images,aperm,c(2,1,3))
grey_images <- lapply(img_transposed, greyscale, contrast = contrast_adj2)
imagesMapped <- lapply(grey_images, mapped, threshold = thresh_adj2)
for (z in 1:length(images)) {
  imagesConverted <- image_convert(imagesMapped[[z]], w = 30, h = 50, offset = 0.001, areathresh = areathresh2, tolerance = tolerance2, ext = ext2)
  final_img <- countImages(imagesConverted, normalize = T, removeEdgeCells = T)
  count <- countCells(imagesConverted)
  Cell.Count[nrow(Cell.Count) + 1, ] <- c(imgNames[[z]], count)
  analyzed_image <- paste0(sub(".tif", replacement = "", x = imgNames[z]), "_analyzed.tiff")
  writeImage(final_img, files = paste0(image_file, analyzed_image),compression=c("LZW"))
}
cell.total <<- as.numeric(Cell.Count$Cell_Count)
cell.total <<- sum(cell.total)
Cell.Count[nrow(Cell.Count) + 1, ] <- c("Cell Total", cell.total)
cell.den <<- cell_density(cell.total, FOV = FOV2, images = image_num2, filtration.area = filter_area2, volume = filter_vol2, total.volume = total_vol2)
Cell.Count[nrow(Cell.Count) + 1, ] <- c("Cell Density", cell.den)
cell.av<<-(cell.total/image_num2)
Cell.Count[nrow(Cell.Count) + 1, ] <- c("Imaging Average",cell.av)
cell.mL<<-(cell.den/total_vol2)
Cell.Count[nrow(Cell.Count) + 1, ] <- c("Total Volume Cell per mL",cell.mL)
write.csv(Cell.Count, paste0(csv_file, csv_file2))
beepr::beep(sound=3)