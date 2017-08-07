# Terrain Ruggedness Index
#
#' Calculate Terrain Ruggedness with Padding
#'
#' @param E1 A raster measuring elevation
#' @param nr the number of rows in the window used in the neighbourhood calculations
#' @param nc the number of columns in the window used in the neighbourhood calculations
#' 
#' @return A raster with TRI values
#' 
#' @examples TRI( raster::raster(matrix( runif(9), 3,3) ) )
#'  
#' @export
TRI <- function(E1, nr=3, nc=nr) {
	f    <- matrix(1, nrow=nr, ncol=nc)
	E2   <- E1**2
	S    <- raster::focal(E1, f, fun=sum, na.rm=TRUE, pad=TRUE, padValue=NA)
	S2   <- raster::focal(E2, f, fun=sum, na.rm=TRUE, pad=TRUE, padValue=NA)
	TRI2 <- (9*E2) + (S2) - (2*E1*S)
	TRI  <- sqrt(TRI2)
	names(TRI) <- "TRI"
	TRI
}

TRI <- compiler::cmpfun(TRI)

#-------------------------------------------------------------------
# Terrain Ruggedness Index _ OLD

### Home-Brew
#TRI  <- function( E, f=matrix(1,nrow=3, ncol=3), HPC=FALSE ){
#	if(HPC==FALSE){ library(raster)
#	E2   <- E**2
#	S    <- focal(E, f, fun=sum, na.rm=TRUE, pad=TRUE, padValue=NA)
#	S2   <- focal(E2, f, fun=sum, na.rm=TRUE, pad=TRUE, padValue=NA)
#	TRI2 <- (9*E2) + (S2) - (2*E*S)
#	TRI  <- sqrt(TRI2)
#	names(TRI10) <-"TRI"
#	} else { library(raster); library(spatial.tools)
#	NA
#	E2   <- E**2
#	S    <- focal(E, f, fun=sum, na.rm=TRUE, pad=TRUE, padValue=NA)
#	S2   <- focal(E2, f, fun=sum, na.rm=TRUE, pad=TRUE, padValue=NA)
#	TRI2 <- (9*E2) + (S2) - (2*E*S)
#	TRI  <- sqrt(TRI2)
#	names(TRI10) <-"TRI"
#	}
#}

#E1   <- raster( paste0(gdir, "Elevation_Proj.tif") )
#TRI <- focal(E1, w=f, fun=function(x, ...) sum(abs(x[-5]-x[5]))/8, pad=TRUE, padValue=NA)

### Built-IN
# tri <- terrain(E1, opt="TRI")

