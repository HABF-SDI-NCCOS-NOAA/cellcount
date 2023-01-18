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

file_name<-paste0("D6_Treatment_") #Change the name of species analyzed here

#Change directories here
savdir <- ("C:/Users/Tyler.Harman/Desktop/DinoSHIELD/Total_Bacteria/CSV_data/Day_6/")
image_savdir <- ("C:/Users/Tyler.Harman/Desktop/DinoSHIELD/Total_Bacteria/Convert_Images/Day_6/Treatment/4_mesoJ/")
images <- list.files("C:/Users/Tyler.Harman/Desktop/DinoSHIELD/Total_Bacteria/Quantitative_Images/Day_6/Treatment/4_mesoJ/"
                     , pattern = "tif", full.name = T)
images_names <- list.files("C:/Users/Tyler.Harman/Desktop/DinoSHIELD/Total_Bacteria/Quantitative_Images/Day_6/Treatment/4_mesoJ/"
                           , pattern = "tif", full.name = F)

imgNames <- paste0(file_name, images_names)
read_images <- lapply(images, readTIFF)
img_transposed <- lapply(read_images,aperm,c(2,1,3))
names(images) <- imgNames

grey_images <- lapply(img_transposed, greyscale, contrast = 1)
display(grey_images[[5]]) #visualize contrast/brightness adjustment here

imagesMapped <- lapply(grey_images, mapped, threshold = 0.2) #background intensity threshold adjustment


#IMAGE TESTING - do you need to make variable adjustments?

imagesConverted <- image_convert(imagesMapped[[5]], w = 10, h = 10, offset = 0.001, areathresh = 50, tolerance = 0.8, ext = 1)
#Change variables above depending on species analyzed
final_img <- countImages(imagesConverted, normalize = T, removeEdgeCells = T)
display(final_img)

for (z in 1:length(images)) {
  imagesConverted <- image_convert(imagesMapped[[z]], w = 10, h = 10, offset = 0.001, areathresh = 50, tolerance = 0.8, ext = 1)
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
#Cell.Count$SD_range<-outlier()

cell.total <- as.numeric(Cell.Count$Cell_Count)
cell.total <- sum(cell.total)

Cell.Count[nrow(Cell.Count) + 1, ] <- c("Cell Total", cell.total,"null")

#Change variables outlined below
cell.den <- cell_density(cell.total, FOV = 0.00084, images = 10, filtration.area = 213.8, volume = 1, total.volume = 2)
Cell.Count[nrow(Cell.Count) + 1, ] <- c("Cell Density", cell.den,"null")

cell.av<-(cell.total/10) #change this on number of images
Cell.Count[nrow(Cell.Count) + 1, ] <- c("Imaging Average",cell.av,"null")

cell.mL<-(cell.den/2) #change this via total volume
Cell.Count[nrow(Cell.Count) + 1, ] <- c("Total Volume Cell per mL",cell.mL,"null")

write.csv(Cell.Count, paste0(savdir, "/D6_Treatment4_mesoJ counts.csv")) #Change this CSV file name

beepr::beep(sound=2) #analysis complete
