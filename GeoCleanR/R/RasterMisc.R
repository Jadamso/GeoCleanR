#-------------------------------------------------------------------
##################
#' Transform Polygon Shapefile to Raster
################## 
#'
#' @param SHP spatialPolygonDF object
#' @param field_name which variable to turn into raster?
#' @param Base project SHP to this raster
#' @param GDAL which method
#' @param cropmask mask the raster afterwards
#' 
#' @return raster
#' 
# @examples
#'  
#' @export

CRAST_fun <-  compiler::cmpfun( function(
    SHP, field_name, Base, GDAL=TRUE, cropmask=TRUE){

    if( class(Base)=="character") {
        Base <- raster::raster(Base)
    }

    ## Input Shapefile has same projection as base
    shp  <- sp::spTransform(SHP, crs(Base))

    if(GDAL==FALSE){
        Crast <- raster::rasterize(shp, Base, field=field_name, fun=modal)  
    } else {

        #requireNamespace("rgdal")
        #requireNamespace("gdalUtils")

        message("Input Shapefile")
        ogr_file  <- paste0( tempfile(), ".shp")
        lyr_name  <- gsub( ".shp", "", gsub( paste0(tempdir(),"/"), "", ogr_file) )
        lyr_dir   <- tempdir()
        rgdal::writeOGR(shp, dsn=lyr_dir, layer=lyr_name,
            driver="ESRI Shapefile", overwrite_layer=TRUE)

        message("Input Raster")
        base_file <- Base@file@name
        rast_file <- paste0( tempfile(),".tif")
        if(base_file==""){
            raster::writeRaster(Base, rast_file , "GTiff",
                overwrite=TRUE)
        } else {
            file.copy(base_file, rast_file, overwrite=TRUE)
        }

        message("gdal_rasterize")
        Crast <- gdalUtils::gdal_rasterize( ogr_file, rast_file,
            b=1, a=field_name, verbose=TRUE, output_Raster=TRUE)
            ## -where expression: to use function
    }

    ## Make sure output raster has same extent as input shapefile
    message("Crop and Mask") ## Note this takes the most time
    if(cropmask){
        Crast <- raster::mask(
            raster::crop( Crast, extent(shp)),
            shp)
    }

    ## Return
    Crast
})

#-------------------------------------------------------------------
##################
#' Aggregate Raster
################## 
#'
#' @param i aggregation factor
#' @param rast raster being aggregated
#' 
#' @return raster
#' 
# @examples
#'  
#' @export

agg_fun <- compiler::cmpfun( function(i, rast){
    if ( i > 1 ){
        xi <- raster::aggregate(rast, i, 
            expand=TRUE, fun=modal, na.rm=TRUE)
    } else if (i==1){
        xi <- rast
    } else {
        message("Negative Values Not Accepted")
        xi <- NA
    }
    return(xi)
})

