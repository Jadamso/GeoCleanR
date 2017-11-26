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


DF2stack <- compiler::cmpfun( function(
    DF,
    dfname){

    sp::coordinates(DF) <- ~x+y
    sp::gridded(DF)     <- TRUE
    DF <- raster::stack(DF[dfname])

    return(DF)
    
})


#' rdname DF2stack
#' @export
DF2Raster <- DF2stack

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

