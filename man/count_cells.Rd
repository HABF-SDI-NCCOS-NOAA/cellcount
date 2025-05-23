% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/count_cells.R
\name{count_cells}
\alias{count_cells}
\title{Image count}
\usage{
count_cells(x)
}
\arguments{
\item{x}{converted image variable using the "image_convert" function}
}
\value{
Counts cells within an processed image
}
\description{
Takes information from a converted image and counts cells within the image.
This remove cells along the image border and only count cells within the image
in order to produce a more accurate cell count.
}
\examples{
images <- list.files("C:/Users/Desktop/Test_Images/",pattern = "tif", full.name = T)
grey_images <- greyscale(images,contrast=2)
images_mapped <- mapped(grey_images,threshold=0.3)
imagesConverted <- image_convert(imagesMapped[[1]],w=10,h=10,offset=0.001,areathresh=50,tolerance=0.8,ext=1)
count_cells(imagesConverted)
}
