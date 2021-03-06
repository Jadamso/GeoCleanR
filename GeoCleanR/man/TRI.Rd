% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TRI.R
\name{TRI}
\alias{TRI}
\title{Calculate Terrain Ruggedness with Padding}
\usage{
TRI(E1, nr = 3, nc = nr)
}
\arguments{
\item{E1}{A raster measuring elevation}

\item{nr}{the number of rows in the window used in the neighbourhood calculations}

\item{nc}{the number of columns in the window used in the neighbourhood calculations}
}
\value{
A raster with TRI values
}
\description{
Calculate Terrain Ruggedness with Padding
}
\examples{
TRI( raster::raster(matrix( runif(9), 3,3) ) )
 
}
