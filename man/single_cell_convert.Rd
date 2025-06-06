% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/single_cell_convert.R
\name{single_cell_convert}
\alias{single_cell_convert}
\title{Single-cell convert}
\usage{
single_cell_convert(
  x,
  w = 17,
  h = 17,
  offset = 0.001,
  areathresh = 50,
  tolerance = 0.5,
  ext = 1
)
}
\arguments{
\item{x}{an image variable that has been mapped using the "mapped" function}

\item{w}{width of the moving rectangular window (see EBImage)}

\item{h}{heigth of the moving rectangular window (see EBImage)}

\item{offset}{a thresholding offset}

\item{areathresh}{a numerical value regarding the individual pixel areas of cells within each image - this sets a threshold for what items are removed from images}

\item{tolerance}{The minimum height of the object in the units of image intensity between its highest point and the point where it contacts another object (see EBImage for more details)}

\item{ext}{Radius of the neighborhood in pixels for the detection of neighboring objects. Higher values smooth out small objects (see EBImage for more details)}
}
\value{
Produces the proper data format for the function "countImages" to produce color-label images.
}
\description{
Takes a mapped image (i.e., data matrix) and counts individual cells within an image.
This is to be used with images that contains a single
fluorescent dimension of cells. Best used for single-cell type images.
}
\examples{
images <- list.files("C:/Users/Desktop/Test_Images/",pattern = "tif", full.name = T)
grey_images <- greyscale(images,contrast=2)
images_mapped <- mapped(grey_images,threshold=0.3)
single_cell_convert(imagesMapped[[1]],w=10,h=10,offset=0.001,areathresh=50,tolerance=0.8,ext=1)
}
