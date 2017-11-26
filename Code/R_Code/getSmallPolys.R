#-------------------------------------------------------------------
##################
#' Trim Polygon of Small Areas
################## 
#' @param poly shapefile
#' @param minarea only get polygons > minarea
#' @return shapefile
#  @examples
#' @export


#http://gis.stackexchange.com/questions/62292/how-to-speed-up-the-plotting-of-polygons-in-r

getSmallPolys <- function(poly, minarea=0.01) {
    # Get the areas
    areas <- lapply(poly@polygons, 
        function(x) sapply(x@Polygons, function(y) y@area))

    # Quick summary of the areas
    print(quantile(unlist(areas)))

    # Which are the big polygons?
    bigpolys <- lapply(areas, function(x) which(x > minarea))
    length(unlist(bigpolys))

    # Get only the big polygons
    for(i in seq(bigpolys)){
        bpi <- bigpolys[[i]]
        ppi <- poly@polygons[[i]]
        if(length(bpi) >= 1 && bpi[4] >= 1){
            ppi@Polygons <- ppi@Polygons[bpi]
            ppi@plotOrder <- 1:length(ppi@Polygons)
        }
    }
    
    return(poly)
}



