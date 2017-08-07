#------------------------------------------------------------------
##################
#' Download Shapefiles
################## 
#' 
#' @references  jw hollister, Oct 10, 2012
#'
#' @param shape_url the directory containing the shape files (.shp, .shx, ...)
#' @param layer the name of file to download
#' @param outdir the directory where to save the files
#' @param layer_new the filename to save
#' @return NULL
#  @examples download.shapefile("http://awmc.unc.edu/awmc/map_data/shapefiles/modern/", "open_water_mod", "/tmp/") 
#' @export
#' 


# Download Shapefiles
download.shapefile <-function(
    shape_url,
    layer,
    outdir=getwd(),
    layer_new=layer){

    ## Required Packages
    rcurl <- requireNamespace("RCurl", quietly = TRUE)
    #requireNamespace("utils", quietly = TRUE))

    #set-up/clean-up variables
    if(length(grep("/$",shape_url))==0) {
        shape_url<-paste(shape_url,"/",sep="")
    }

    #creates vector of all possible shapefile extensions
    shapefile_ext <-c(".shp",".shx",".dbf",".prj",".sbn",".sbx",
                   ".shp.xml",".fbn",".fbx",".ain",".aih",".ixs",
                   ".mxs",".atx",".cpg")

    #Check which shapefile files exist
    if( rcurl ){
        xurl   <- RCurl::getURL(shape_url)
        xlogic <- NULL
    for(i in paste(layer, shapefile_ext, sep="")) {
        xlogic <- c(xlogic,grepl(i,xurl))
    }

    #Set-up list of shapefiles to download
    shapefiles <- paste(shape_url, layer, shapefile_ext, sep="")[xlogic]
    #Set-up output file names
    outfiles   <- paste(outdir, layer_new, shapefile_ext, sep="")[xlogic]
    }

    #Download all shapefiles
    if(sum(xlogic)>0) {
        for(i in 1:length(shapefiles)) {
            utils::download.file(shapefiles[i],outfiles[i],
                method="auto", mode="wb")
    } } else {
        stop("Error has occured with input URL or shapefile name")
    }
}

download.shapefile <- compiler::cmpfun(download.shapefile)
#------------------------------------------------------------------
##################
#' Download Rasters
################## 
#' 
#' @param shape_url the directory containing the shape files (.shp, .shx, ...)
#' @param layer the name of file to download
#' @param outdir the directory where to save the files
#' @param layer_new the filename to save
#' @return NULL
#  @examples download.raster("http://awmc.unc.edu/awmc/map_data/elevation_data", "mod_elevation", "/tmp/") 
#' @export

download.raster <- function(shape_url,
    layer,
    outdir=getwd(),
    layer_new=layer){

    ## Required Packages
    rcurl <- requireNamespace("RCurl", quietly = TRUE)
    #requireNamespace("utils", quietly = TRUE))

    #set-up/clean-up variables
    if(length(grep("/$",shape_url))==0) {
        shape_url<-paste(shape_url,"/",sep="")
    }

    #creates vector of all possible shapefile extensions
    shapefile_ext <- c(".tfw",".tif",".tif.aux.xml",".tif.ovr",".tif.xml",".grd")

    #Check which shapefile files exist

    #if( require("RCurl") ) {
    if( rcurl ) {
        xurl   <- RCurl::getURL(shape_url)
        xlogic <- NULL
    for(i in paste(layer,shapefile_ext,sep="")) {
        xlogic <- c(xlogic,grepl(i,xurl))
    }
    #Set-up list of shapefiles to download
    shapefiles <- paste(shape_url, layer, shapefile_ext, sep="")[xlogic]
    #Set-up output file names
    outfiles   <- paste(outdir, layer_new, shapefile_ext, sep="")[xlogic] 
    }

    #Download all shapefiles
    if(sum(xlogic)>0){
    for(i in 1:length(shapefiles)) {
        utils::download.file(shapefiles[i],outfiles[i],
            method="auto",mode="wb") 
    } } else {
    stop("Error has occured with input URL or Raster name")
    }
}

download.raster <- compiler::cmpfun(download.raster)

