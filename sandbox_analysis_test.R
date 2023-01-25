library(EBImage)
library(tiff)
library(pixmap)
library(raster)
library(beepr)
library(outliers)
library(cellcount)

# clear environment and free unused memory
gc()

Cell.Count <- data.frame(Image_File_Name = character(0), Cell_Count = numeric(0))

file_name<-paste0("Dolichospermum_") #Change the name of species analyzed here

#Change directories here
savdir <- ("C:/Users/Tyler.Harman/Desktop/cellcount_work/cellcount_data/CSV_data/")
image_savdir <- ("C:/Users/Tyler.Harman/Desktop/cellcount_work/cellcount_data/Convert_Images/Cyano F271_40x/Med/Rep 1/")
images <- list.files("C:/Users/Tyler.Harman/Desktop/cellcount_work/quantitative_images/F271_40x/Med/Rep 1/"
                     , pattern = "tif", full.name = T)
images_names <- list.files("C:/Users/Tyler.Harman/Desktop/cellcount_work/quantitative_images/F271_40x/Med/Rep 1/"
                           , pattern = "tif", full.name = F)

imgNames <- paste0(file_name, images_names)
read_images <- lapply(images, readTIFF)
img_transposed <- lapply(read_images,aperm,c(2,1,3))
names(images) <- imgNames

grey_images <- lapply(img_transposed, greyscale, contrast = 1)
display(grey_images[[5]]) #visualize contrast/brightness adjustment here

binary<-function(x, adj = 0.5) {
  binary_img<- x > adj
  return(binary_img)
}
binary_imgs<-lapply(grey_images, binary, adj = 0.32)
display(binary_imgs[[5]])

imagesMapped <- lapply(grey_images, mapped, threshold = 0.32) #background intensity threshold adjustment


#IMAGE TESTING - do you need to make variable adjustments?

imagesConverted <- image_convert2(imagesMapped[[5]], w = 50, h = 50, offset = 0.001, areathresh = 250, tolerance = 0.8, ext = 4)
#Change variables above depending on species analyzed
final_img <- countImages(imagesConverted, normalize = T, removeEdgeCells = T)
display(final_img)

for (z in 1:length(images)) {
  imagesConverted <- image_convert2(imagesMapped[[z]], w = 50, h = 50, offset = 0.001, areathresh = 250, tolerance = 0.8, ext = 4)
  final_img <- countImages(imagesConverted, normalize = T, removeEdgeCells = T)
  count <- countCells(imagesConverted)
  Cell.Count[nrow(Cell.Count) + 1, ] <- c(imgNames[[z]], count)
  analyzed_image <- paste0(sub(".tif", replacement = "", x = imgNames[z]), "_analyzed.tiff")
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

write.csv(Cell.Count, paste0(savdir, "/Dolichospermum_Med1  counts.csv")) #Change this CSV file name

beepr::beep(sound=2) #analysis complete
