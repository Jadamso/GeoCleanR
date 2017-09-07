#------------------------------------------------------------------
##################
#' Functions to read in County Shapefiles
##################
#'
#' @param myears years to grab
#' @param rdir from what directory
#' 
#' @return shapefile
#' 
#' @details Modern Counties
# @examples
#'  
#' @export


COUNTIESm <-  compiler::cmpfun( function(myears, rddir) {
	mclapply(myears , function(year) {
		cfile <- paste0("tl_",year,"_us_county")
		SHP   <- rgdal::readOGR( rddir, layer=cfile )
		SHP$ID_NUM <- as.numeric( SHP$GEOID )
		SHP
	})
})


#' @rdname COUNTIESm
#' @details Middle Modern Counties
# Also works for COUNTIESm
#' @export
COUNTIESmm <-  compiler::cmpfun( function(mmyears, rddir) {
	mclapply(mmyears , function(year) {
		cfile <- paste0("tl_",year,"_us_county")
		SHP   <- rgdal::readOGR( rddir, layer=cfile )
		names(SHP) <- gsub("10", "", names(SHP))
		    ## 2010 names unconventional
		SHP$ID_NUM <- as.numeric(
		    paste0(SHP$STATEFP , SHP$COUNTYFP ) )
		SHP
	})
})

#' @rdname COUNTIESm
#' @details Historical Counties
# Also works for COUNTIESm
#' @export
COUNTIESh <- compiler::cmpfun( function(hyears, ddir) {
	COUNTIESh0 <- readOGR( ddir, layer="Counties" )
	mclapply( hyears, function(year) {
		#date_e <- paste0(year, "-12-31")
		date_s  <- paste0(year, "-01-01")
		SHP <- COUNTIESh0[ COUNTIESh0$START <= date_s & COUNTIESh0$END >= date_s , ]
	})
})


#------------------------------------------------------------------
##################
#' Wrapper Functions for County Shapefiles
##################
#'
#' @param hyears,myears,mmyears vectors of years
#' @param ddir,rdir directory of shapefiles
#' 
#' @return list of county shapefiles
#' 
# @details
# @examples
#'  
#' @export
create_counties <- function(
    hyears=c(1980,1990,2000),
    mmyears=2008:2010,
    myears=2011:2015,
    ddir,rdir) { 

	counties <- c(
		if( any(is.na(hyears)) ) {
		    NULL 
		} else {
		    COUNTIESh(hyears, ddir)
		},
		if( any(is.na(mmyears)) ) {
		    NULL 
		} else {
		    COUNTIESmm(mmyears, rddir) },
		if( any(is.na(myears)) ) {
		    NULL
		} else {
		    COUNTIESm(myears, rddir)
		}
	)
}

