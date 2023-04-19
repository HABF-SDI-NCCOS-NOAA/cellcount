library(EBImage)
library(tiff)
library(pixmap)
library(raster)
library(beepr)
library(outliers)
library(cellcount)

mapped_avg <- function(x) {
  x <- as.matrix(x)
  return(x)
}

# clear environment and free unused memory
gc()

Cell.Count <- data.frame(Image_File_Name = character(0), Cell_Count = numeric(0))

file_name<-paste0("F192_") #Change the name of species analyzed here

#Change directories here
savdir <- ("C:/Users/Tyler.Harman/Desktop/cellcount_work/cellcount_data/CSV_data/")
image_savdir <- ("C:/Users/Tyler.Harman/Desktop/cellcount_work/cellcount_data/Convert_Images/Cyano F192_40x/Rep 1/Conc_E/")
images <- list.files("C:/Users/Tyler.Harman/Desktop/cellcount_work/quantitative_images/Cyano F192_40x/Rep 1/Conc_E/"
                     , pattern = "tif", full.name = T)
images_names <- list.files("C:/Users/Tyler.Harman/Desktop/cellcount_work/quantitative_images/Cyano F192_40x/Rep 1/Conc_E/"
                           , pattern = "tif", full.name = F)

imgNames <- paste0(file_name, images_names)
read_images <- lapply(images, readTIFF)
img_transposed <- lapply(read_images,aperm,c(2,1,3))
names(images) <- imgNames

grey_images <- lapply(img_transposed, greyscale, contrast = 4)
display(grey_images[[2]]) #visualize contrast/brightness adjustment here

img1<-lapply(grey_images,mapped_avg)

for (j in 1:length(images)) {
  adj1<-median(img1[[j]])
  adj2<-adj1+0.2
  imagesMapped <- lapply(grey_images, mapped, threshold = adj2) #background intensity threshold adjustment
  imagesConverted <- single_cell_convert(imagesMapped[[j]], w = 17, h = 17, offset = 0.001, areathresh = 250, tolerance = 1, ext = 1)
  final_img <- countImages(imagesConverted, normalize = T, removeEdgeCells = T)
  count <- countCells(imagesConverted)
  Cell.Count[nrow(Cell.Count) + 1, ] <- c(imgNames[[j]], count)
  analyzed_image <- paste0(sub(".tif", replacement = "", x = imgNames[j]), "_analyzed.tiff")
  writeImage(final_img, files = paste0(image_savdir, analyzed_image),compression=c("LZW"))
}

Cell.Count$Cell_Count<-as.numeric(Cell.Count$Cell_Count)
outlier<-function(){
  mean<-median(Cell.Count$Cell_Count)
  sd<-sd(Cell.Count$Cell_Count)*2
  sd_neg<-(mean-sd)
  sd_pos<-(mean+sd)
  SD_range_detection<-ifelse(sd_pos>Cell.Count$Cell_Count,Cell.Count$Cell_Count,NA)&
    ifelse(sd_neg<Cell.Count$Cell_Count,Cell.Count$Cell_Count,NA)
  return(SD_range_detection)
}
Cell.Count$SD_range<-outlier()

cell.total <- as.numeric(Cell.Count$Cell_Count)
cell.total <- sum(cell.total)

Cell.Count[nrow(Cell.Count) + 1, ] <- c("Cell Total", cell.total,"null")

#Change variables outlined below
#cell.den <- cell_density(cell.total, FOV = 0.00084, images = 10, filtration.area = 213.8, volume = 1, total.volume = 2)
#Cell.Count[nrow(Cell.Count) + 1, ] <- c("Cell Density", cell.den,"null")

cell.av<-(cell.total/10) #change this on number of images
Cell.Count[nrow(Cell.Count) + 1, ] <- c("Imaging Average",cell.av,"null")

#cell.mL<-(cell.den/2) #change this via total volume
#Cell.Count[nrow(Cell.Count) + 1, ] <- c("Total Volume Cell per mL",cell.mL,"null")

write.csv(Cell.Count, paste0(savdir, "/F192_Conc_1E counts.csv")) #Change this CSV file name

beepr::beep(sound=2) #analysis complete
