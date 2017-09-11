#-------------------------------------------------------------------
##################
#' Spatial Points/Polygon Extract Closest from Raster
################## 
#http://stackoverflow.com/questions/27562076/if-raster-value-na-search-and-extract-the-nearest-non-na-pixel
#
#'
#' @param rast A raster
#' @param spdf A SpatialPoints or SpatialPointsDataFrame
#' @param ncore the size of the window used in the neighbourhood calculations
#' @param setvals set raster values to extract
#'
#' @return A list with raster values for each spatial point
#'
#' @details Extract Closest non-NA Raster Values to Spatial Points in parallel
#'
# @examples
#' library(raster)
#' xy  <- cbind(x=seq(-1,2,by=.1), y=seq(2,-1,by=-.1) )
#' spdf <- sp::SpatialPoints( xy )
#' rast <- raster::raster( matrix(runif(100), 10, 10) )
#' raster::crs(spdf) <- raster::crs(rast) <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#' ExtractClosest(rast, spdf, 1)
#
#' @export

ExtractClosest <- compiler::cmpfun( function(
    rast,
    spdf,
    ncore=24,
    setvals=FALSE) { 
    
	requireNamespace("raster")
	requireNamespace("parallel")
	requireNamespace("sp")

    ## Set values
    if( setvals) {
        rast@data@values <- values(rast)
    } else {
	## Check if Raster has values to extract
	    try( if(length(rast@data@values)==0) {
	        message("try rast@data@values <- values(rast)")
	        stop( "Error: raster values not set")} )
	}

	## Coordinates
	XY <- sp::coordinates(spdf)
	XY <- lapply(seq_len(nrow(XY)), mc.cores=ncore,
	    FUN=function(i) XY[i,] )

	## Extract Closest Raster Values
	rast_vals <- parallel::mclapply(XY, mc.cores=ncore,
	    FUN=function(XY_i) { 
	    
	        rast_dist  <- raster::distanceFromPoints(rast, XY_i)
		    rast_val_i <- replace(rast_dist, is.na(rast), NA)
		    rast@data@values[ raster::which.min( rast_val_i ) ] 
	})
	
	return(unlist(rast_vals))
})



#-------------------------------------------------------------------
##################
#' Wrapper for Extract Closest
################## 
#
#' @rdname ExtractClosest
#' @export

extract_closest <- compiler::cmpfun( function(
    rast,
    spdf,
    ncore=24,
    setvals=FALSE){
    
    hhi_ext <- raster::extract(rast, spdf )
    hhi_na  <- which(is.na(hhi_ext))

    if( length(hhi_na) >0 ){
        hhi_ext0 <- ExtractClosest(hhi0,
             tCentroids[hhi_na2,], ncore, setvals)
        hhi_ext[hhi_na1,]  <- hhi_ext0
    }
    
    return(hhi_ext)
})    
