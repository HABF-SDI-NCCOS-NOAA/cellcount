library(EBImage)
library(tiff)
library(pixmap)
library(raster)
library(beepr)

mapped_avg <- function(x) {
  x <- as.matrix(x)
  return(x)
}


if(analysis_type == 'single-cell'){
  read_images <- lapply(images, readTIFF)
  img_transposed <- lapply(read_images,aperm,c(2,1,3))
  grey_images <- lapply(img_transposed, greyscale, contrast = contrast_adj2)
  img1<-lapply(grey_images,mapped_avg)
  for (z in 1:length(images)) {
    adj1<-median(img1[[z]])
    adj2<-adj1+thresh_adj2
    imagesMapped <- lapply(grey_images, mapped, threshold = adj2) #background intensity threshold adjustment
    imagesConverted <- single_cell_convert(imagesMapped[[z]], w = rec_width2, h = rec_height2, offset = 0.001, areathresh = areathresh2, tolerance = tolerance2, ext = ext2)
    final_img <- count_images(imagesConverted, normalize = T, removeEdgeCells = T)
    count <- count_cells(imagesConverted)
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
} else{
  read_images <- lapply(images, readTIFF)
  img_transposed <- lapply(read_images,aperm,c(2,1,3))
  grey_images <- lapply(img_transposed, greyscale, contrast = contrast_adj2)
  img1<-lapply(grey_images,mapped_avg)
  for (z in 1:length(images)) {
    adj1<-median(img1[[z]])
    adj2<-adj1+thresh_adj2
    imagesMapped <- lapply(grey_images, mapped, threshold = adj2) #background intensity threshold adjustment
    imagesConverted <- filamentous_convert(imagesMapped[[z]], w = rec_width2, h = rec_height2, offset = 0.001, areathresh = areathresh2, tolerance = tolerance2, ext = ext2)
    final_img <- count_images(imagesConverted, normalize = T, removeEdgeCells = T)
    count <- count_cells(imagesConverted)
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
}