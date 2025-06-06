% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/border.R
\name{border}
\alias{border}
\title{Border conversion}
\usage{
border(x, y)
}
\arguments{
\item{x}{An image file}

\item{y}{Dimming variable (using the 'dim' function from EBImage)}
}
\value{
Analyzing the cells on the border of \code{x}
}
\description{
Takes images to predict cells along a predetermined border in order
to count cells along the predetermined border. This will be used to help
determine those cells to "remove" them from the count function "countCells".
}
