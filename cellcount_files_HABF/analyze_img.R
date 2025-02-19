library(EBImage)
library(tiff)
library(pixmap)
library(raster)
library(beepr)

mapped_avg <- function(x) {
  x <- as.matrix(x)
  return(x)
}


if(analysis_type == 'single-cell'){ # Check if the analysis type is 'single-cell'
  # Attempt to read TIFF images into a list
  read_images <- lapply(images, readTIFF)
  
  # Check if images were successfully read
  if (length(read_images) > 0 && all(sapply(read_images, is.array))) {
    # If condition is true, images were successfully read
    
    # Print success message to console
    cat("Images successfully read into the environment.\n")
    
    # Print the number of images read
    print(paste("Number of images read:", length(read_images)))
  } else {
    # If condition is false, there was an error reading the images
    
    # Print error message to console
    cat("Error: Failed to read one or more images.\n")
    
    # Stop script execution and display error message
    stop("Image reading failed. Please check your image files and path.")
  }
  img_transposed <- lapply(read_images,aperm,c(2,1,3)) # Transpose images
  grey_images <- lapply(img_transposed, greyscale, contrast = contrast_adj2) # Convert to greyscale with contrast adjustment
  img1<-lapply(grey_images,mapped_avg) # Apply mapped_avg function to grey images
  for (z in 1:length(images)) { # Loop through each image
    adj1<-median(img1[[z]]) # Calculate median of mapped average
    adj2<-adj1+thresh_adj2 # Adjust threshold
    imagesMapped <- lapply(grey_images, mapped, threshold = adj2) # Apply mapping with adjusted threshold
    imagesConverted <- single_cell_convert(imagesMapped[[z]], w = rec_width2, h = rec_height2, offset = 0.001, areathresh = areathresh2, tolerance = tolerance2, ext = ext2) # Convert single cell images
    final_img <- count_images(imagesConverted, normalize = T, removeEdgeCells = T) # Count and process images
    count <- count_cells(imagesConverted) # Count cells in converted images
    Cell.Count[nrow(Cell.Count) + 1, ] <- c(imgNames[[z]], count) # Add cell count to results
    analyzed_image <- paste0(sub(".tif", replacement = "", x = imgNames[z]), "_analyzed.tiff") # Create analyzed image filename
    writeImage(final_img, files = paste0(image_file, analyzed_image),compression=c("LZW")) # Write analyzed image to file
  }
  #cell.total <<- as.numeric(Cell.Count$Cell_Count) # Convert cell count to numeric
  #cell.total <<- sum(cell.total) # Calculate total cell count
  Cell.Count[nrow(Cell.Count) + 1, ] <- c("Cell Total", sum(as.numeric(Cell.Count$Cell_Count))) # Add total cell count to results
  cell.den <<- cell_density(cell.total, FOV = FOV2, images = image_num2, filtration.area = filter_area2, volume = filter_vol2, total.volume = total_vol2) # Calculate cell density
  Cell.Count[nrow(Cell.Count) + 1, ] <- c("Cell Density", cell.den) # Add cell density to results
  #cell.av<<-(cell.total/image_num2) # Calculate average cells per image
  Cell.Count[nrow(Cell.Count) + 1, ] <- c("Imaging Average",((sum(as.numeric(Cell.Count$Cell_Count)))/image_num2)) # Add average to results
  #cell.mL<<-(cell.den/total_vol2) # Calculate cells per mL
  Cell.Count[nrow(Cell.Count) + 1, ] <- c("Total Volume Cell per mL",(cell.den/total_vol2)) # Add cells per mL to results
  write.csv(Cell.Count, paste0(csv_file, csv_file2)) # Write results to CSV file
  beepr::beep(sound=3) # Play a sound to indicate completion
} else{
  # Attempt to read TIFF images into a list
  read_images <- lapply(images, readTIFF)
  
  # Check if images were successfully read
  if (length(read_images) > 0 && all(sapply(read_images, is.array))) {
    # If condition is true, images were successfully read
    
    # Print success message to console
    cat("Images successfully read into the environment.\n")
    
    # Print the number of images read
    print(paste("Number of images read:", length(read_images)))
  } else {
    # If condition is false, there was an error reading the images
    
    # Print error message to console
    cat("Error: Failed to read one or more images.\n")
    
    # Stop script execution and display error message
    stop("Image reading failed. Please check your image files and path.")
  }
  img_transposed <- lapply(read_images,aperm,c(2,1,3)) # Transpose images
  grey_images <- lapply(img_transposed, greyscale, contrast = contrast_adj2) # Convert to greyscale with contrast adjustment
  img1<-lapply(grey_images,mapped_avg) # Apply mapped_avg function to grey images
  for (z in 1:length(images)) { # Loop through each image
    adj1<-median(img1[[z]]) # Calculate median of mapped average
    adj2<-adj1+thresh_adj2 # Adjust threshold
    imagesMapped <- lapply(grey_images, mapped, threshold = adj2) # Apply mapping with adjusted threshold
    imagesConverted <- filamentous_convert(imagesMapped[[z]], w = rec_width2, h = rec_height2, offset = 0.001, areathresh = areathresh2, tolerance = tolerance2, ext = ext2) # Convert filamentous cell images
    final_img <- count_images(imagesConverted, normalize = T, removeEdgeCells = T) # Count and process images
    count <- count_cells(imagesConverted) # Count cells in converted images
    Cell.Count[nrow(Cell.Count) + 1, ] <- c(imgNames[[z]], count) # Add cell count to results
    analyzed_image <- paste0(sub(".tif", replacement = "", x = imgNames[z]), "_analyzed.tiff") # Create analyzed image filename
    writeImage(final_img, files = paste0(image_file, analyzed_image),compression=c("LZW")) # Write analyzed image to file
  }
  #cell.total <<- as.numeric(Cell.Count$Cell_Count) # Convert cell count to numeric
  #cell.total <<- sum(cell.total) # Calculate total cell count
  Cell.Count[nrow(Cell.Count) + 1, ] <- c("Cell Total", sum(as.numeric(Cell.Count$Cell_Count))) # Add total cell count to results
  cell.den <<- cell_density(cell.total, FOV = FOV2, images = image_num2, filtration.area = filter_area2, volume = filter_vol2, total.volume = total_vol2) # Calculate cell density
  Cell.Count[nrow(Cell.Count) + 1, ] <- c("Cell Density", cell.den) # Add cell density to results
  #cell.av<<-(cell.total/image_num2) # Calculate average cells per image
  Cell.Count[nrow(Cell.Count) + 1, ] <- c("Imaging Average",((sum(as.numeric(Cell.Count$Cell_Count)))/image_num2)) # Add average to results
  #cell.mL<<-(cell.den/total_vol2) # Calculate cells per mL
  Cell.Count[nrow(Cell.Count) + 1, ] <- c("Total Volume Cell per mL",(cell.den/total_vol2)) # Add cells per mL to results
  write.csv(Cell.Count, paste0(csv_file, csv_file2)) # Write results to CSV file
  beepr::beep(sound=3) # Play a sound to indicate completion
}