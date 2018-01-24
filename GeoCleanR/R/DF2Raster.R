#------------------------------------------------------------------
##################
#' Formatting dataframe as rasterstack
################## 
#'
#' @param DF dataframe
#' @param dfname names to keep
#'
#' @return rasterstack
# @examples
#' @export
#' @seealso DF2Raster

DF2stack <- compiler::cmpfun( function(
    DF, dfname){

    sp::coordinates(DF) <- ~x+y
    sp::gridded(DF)     <- TRUE
    
    DF <- raster::stack(DF[dfname])

    return(DF)
    
})


#' rdname DF2stack
#' @export
df2stack <- DF2stack


#------------------------------------------------------------------
##################
#' Formatting dataframe as rasterstack
################## 
#'
#' @param DF dataframe
#'
#' @return rasterstack
# @examples
#' @export
#' @seealso DF2stack

DF2Raster <- compiler::cmpfun( function(DF){

    sp::coordinates(DF) <- ~x+y
    sp::gridded(DF)     <- TRUE
    
    DF <- raster::raster(DF)

    return(DF)
    
})


#' rdname DF2Raster
#' @export
df2raster <- DF2Raster


#------------------------------------------------------------------
##################
#' Formatting at dataframe
################## 
#'
#' @param l list
#'
#' @return rasterstack
# @examples
#' @export

quickdf <- compiler::cmpfun( function(l) {
    class(l) <- "data.frame"
    attr(l, "row.names") <- .set_row_names(length(l[[1]]))
    return(l)
} )

