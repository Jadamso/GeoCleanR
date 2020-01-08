#------------------------------------------------------------------
##################
#' calculate HHI locally
################## 
#'
#' @param inraster
#' 
#' @return a vector of length 1
#' 
# @details
#' @examples
#' rast <- raster::raster( matrix(runif(100), 10, 10) ) 
#' inrast <- as.integer(rast> .2)
#' hhi_rast <- raster::focal(inrast, w=matrix(1,3,3), hhi_in2)
  
#' @export

hhi_in <- compiler::cmpfun( function( inraster ) {
    TAB <- tabulate( inraster )
    SI  <- TAB/sum(TAB)
    HHI <- c( crossprod( SI ) )
    return( HHI )
} )

#' More user-friendly but less-optimized version of hhi_in 
#' @rdname hhi_in
#' @export

hhi_in2 <- compiler::cmpfun(  function( inraster ) {
    TAB <- as.integer( table( inraster ) )
    SI  <- TAB/sum(TAB)
    HHI <- c( crossprod( SI ) )
    #HHI <- sum( TAB**2 ) / sum(TAB)**2
    return( HHI )
} )

#ST1 <- parallel::mclapply(mc.cores=4, c(1E4, 1E5, 1E6, 1E7), function(i){
#    x <- seq_len(i)
#    s1 <- system.time( hhi_in(x) )
#    s2 <- system.time( hhi_in2(x) )
#    do.call("rbind", list(s1=s1,s2=s2) )
#} )

#------------------------------------------------------------------
##################
#' count number of neighbours locally 
################## 
#'
#' @param inraster
#' 
#' @return a vector of length 1
#' 
# @details
# @examples
#'  
#' @export

count_in <- compiler::cmpfun( function( inraster ) {
     return( length( tabulate( inraster ) ) ) } )

#' More user-friendly but less-optimized version of count_in 
#' @rdname count_in
#' @export

count_in2 <- compiler::cmpfun( function( inraster ) {
     return( length( as.integer( table( inraster ) ) ) ) } )
     
#count_in <- compiler::cmpfun( function( inraster ) {
#    TAB <- tabulate( inraster )
#    ncount <- length( TAB[TAB!=0] )*1
#    return(ncount)
#} )


#------------------------------------------------------------------
##################
#' Calculate HHI for each raster cell
################## 
#'
#' @param Crast raster from which to perform calculations
#' @param wind size of local windows to consider
#' @param FUN what to calculate
#' @param mask mask the values afterwards
#' @param writedir write the raster to hard disk
#' @param HHIname what to name the raster
#' 
#' @return a raster
#' 
# @details
#' @examples
#require(deldir)
#require(sp)
## Create Random Map
#w <- tile.list(deldir(x=runif(5),y=runif(5)))
#polys <- vector(mode='list', length=length(w))
#for (i in seq(along=polys)) {
#    pcrds = cbind(w[[i]]$x, w[[i]]$y)
#    pcrds = rbind(pcrds, pcrds[1,])
#    polys[[i]] = Polygons(list(Polygon(pcrds)), ID=as.character(i))
#}
#CMAP <- SpatialPolygons(polys) 
#base_rast <- raster(ncol=10, nrow=10)
#extent(base_rast) <- extent(CMAP)
#Crast <- rasterize( CMAP, base_rast, fun='modal')
## Calculate HHI
#hhi_raster2 <- HHI(Crast, wind=c(3,3), FUN=hhi_in2)
## Calculate HHI with pre-optimization
#SUBID <- sapply(CMAP@polygons, function(e) as.integer(e@ID) )
#ROWID <- 1:length(CMAP)
#ID    <- as.data.frame(cbind(ROWID, SUBID))
#CRAST <- subs(Crast, ID)
#hhi_raster <- HHI(CRAST, wind=c(3,3))
#'  
#' @export


HHI <- compiler::cmpfun( function(
    rast,
    wind,
    FUN=hhi_in,
    mask=NA,
    writedir=NA,
    HHIname=paste0("HHIrast_", wind[1],"_",wind[2])
    ){

    requireNamespace("raster")
    requireNamespace("spatial.tools") # Raster Parallel

    ## HHI Calculation
    HHIrast <- spatial.tools::rasterEngine(
        inraster=rast,
        fun=FUN,
        window_dims=wind,
        processing_unit="single" )

    ## Clean Up Boundaries
    if(!is.na(mask)){
        HHIrast <- raster::mask( HHIrast, mask)
    }

    ## Name Raster
    names(HHIrast) <- HHIname

    ## Write Raster
    if( !is.na(writedir) ){
        RASTname <- paste0(writedir, 
            HHIname, "_", names(rast),".tif")
        raster::writeRaster(HHIrast, RASTname,
            format='GTiff',
            overwrite=TRUE)
        message(RASTname)
        return(RASTname)
    } else {
        return(HHIrast)
    }
} )



