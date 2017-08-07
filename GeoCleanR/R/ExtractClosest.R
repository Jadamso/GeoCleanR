#-------------------------------------------------------------------
##################
# Spatial Points/Polygon Extract from Raster
################## 
#http://stackoverflow.com/questions/27562076/if-raster-value-na-search-and-extract-the-nearest-non-na-pixel
#
#' Extract Closest non-NA Raster Values to Spatial Points PARALLEL
#'
#' @param rast A raster
#' @param spdf A SpatialPoints or SpatialPointsDataFrame
#' @param ncore the size of the window used in the neighbourhood calculations
#' @return A list with raster values for each spatial point
#'
# @examples
#' library(raster)
#' spdf <- sp::SpatialPoints( cbind(x=seq(-1,2,by=.1), y=seq(2,-1,by=-.1)) )
#' rast <- raster::raster( matrix(runif(100), 10, 10) )
#' raster::crs(spdf) <- raster::crs(rast) <-  "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#' ExtractClosest(rast, spdf, 1)
#
#' @export
ExtractClosest <- function(rast, spdf, ncore=24) { 
	requireNamespace("raster")
	requireNamespace("parallel")
	requireNamespace("sp")

	## Check if Raster has values to extract
	try( if(length(rast@data@values)==0) stop( "Error: raster values not set\n  try rast@data@values <- values(rast)" ))

	## Coordinates
	XY    <- sp::coordinates(spdf)
	XY    <- parallel::mclapply(seq_len(nrow(XY)), function(i) XY[i,], mc.cores=ncore)

	## Extract Closest Raster Values
	rast_vals <- parallel::mclapply(XY, mc.cores=ncore, FUN=function(XY_i) { 
		rast_val_i <- replace(raster::distanceFromPoints(rast, XY_i), is.na(rast), NA)
		rast@data@values[which.min( rast_val_i ) ] }
	)
	unlist(rast_vals)
}

ExtractClosest <- compiler::cmpfun(ExtractClosest)
#rast@data@values <- getValues(rast)
