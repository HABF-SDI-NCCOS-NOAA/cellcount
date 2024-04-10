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

file_name<-paste0("E.coli_") #Change the name of species analyzed here

#Change directories here
savdir <- ("./CSV_data/") #Change save directory for .csv file data here
image_savdir <- ("./Convert_Images/E.coli_100x_qPCR/Rep 3/Con_E/") #Change the save directory for analyzed images

images <- list.files("./quantitative_images/E.coli_100x_qPCR/Rep 3/Con_E/"
                     , pattern = "tif", full.name = T) #List the location of the raw images here
images_names <- list.files("./quantitative_images/E.coli_100x_qPCR/Rep 3/Con_E/"
                           , pattern = "tif", full.name = F) #List the location of the raw images here

imgNames <- paste0(file_name, images_names)
read_images <- lapply(images, readTIFF)
img_transposed <- lapply(read_images,aperm,c(2,1,3))
names(images) <- imgNames

grey_images <- lapply(img_transposed, greyscale, contrast = 4)
display(grey_images[[2]]) #visualize contrast/brightness adjustment here

img1<-lapply(grey_images,mapped_avg)

for (j in 1:length(images)) {
  adj1<-median(img1[[j]])
  adj2<-adj1+0.15
  imagesMapped <- lapply(grey_images, mapped, threshold = adj2) #background intensity threshold adjustment
  imagesConverted <- filamentous_convert(imagesMapped[[j]], w = 17, h = 17, offset = 0.001, areathresh = 50, tolerance = 4, ext = 2)
  final_img <- count_images(imagesConverted, normalize = T, removeEdgeCells = T)
  display(final_img)
  count <- count_cells(imagesConverted)
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

Cell.Count$within_outlier_range<-outlier()

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

write.csv(Cell.Count,"E.coli_qPCR_Rep3E.csv") #Change this CSV file name

beepr::beep(sound=2) #analysis complete
