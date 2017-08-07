#-------------------------------------------------------------------
##################
#' Transform RasterStack to Data.Table using Parallel Processing
##################
#'
#' @param stack stack of rasters to be converted, must have coordinate columns (x,y)
#' 
#' @import data.table 
#' @import parallel
#'
#' @return datatable
#' 
# @examples
#'  
#' @export

#http://stackoverflow.com/questions/34926491/converting-rasterstack-to-csv-parallel-processing-in-r

layer_list <- function(stack){
    requireNamespace("data.table")
    requireNamespace("parallel")

   
    mclapply( 1:raster::nlayers(stack), function(i){
        layer_pts <- as.data.table( 
            raster::rasterToPoints(stack[[i]])
        )

        setkey(layer_pts, x, y)# key on x and y,
        layer_pts
     })
    tbl_out <- Reduce(merge, layer_list) # uses keys from setkey

    # if you wanted the "ID" column (but not essential)
    tbl_out[, ID:= paste0( formatCoordinate(x), formatCoordinate(y) ) ]
}


#-------------------------------------------------------------------
##################
#' Transform RasterStack into DataFrame
##################
#'
#' @param Rstack stack of rasters
#' 
#' @return data.frame
#' 
# @examples
#'  
#' @export

stack2df <- function(Rstack) { 
    print(dim(Rstack))
    as.data.frame(
        raster::rasterToPoints(Rstack),
        xy=TRUE,
        na.rm=TRUE)
}
stack2df <- compiler::cmpfun(stack2df)
