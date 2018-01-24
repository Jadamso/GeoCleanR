# Create a Dyadic DataFrame
# DYADmaker0, DYADmaker1, DYADmaker2
# default parameters in Paper6 RBLOCK 012
# @name DYADmaker
# NULL

#------------------------------------------------------------------
##################
#' Make Skeleton for Dyadic Panel
################## 
#' 
#' @param times time period
#' @param dyad_name 
#'
#' @return An empty list to be filled in dyad.maker1
#' @export



dyad.maker0 <- compiler::cmpfun( function(
    times,
    dyad_name=c("Dyad", "Year")){
    
	dyad <- rep(list(NA),length(dyad_name))
	names(dyad) <- dyad_name
	DYAD <- lapply(times, function(t) dyad)
	names(DYAD) <- times
	
	return( DYAD )
	
})



#------------------------------------------------------------------
##################
#' Make A list of dyads 
################## 
# @rdname DYADmaker
#'
#' @param dyad , empty list from DYADmaker0 to be filled
#' @param d_times , time periods to create dyads
#' @param t_span number of periods after d_times to include for data-grouping
#' @param d_tab , Y variable to merge ( table of battles )
#' @param d_df  , X variables to merge ()
#' @param d_df_id , X merger ID
#' @param d_df_names , X var name of ID
#' @param d_df_aggnames1 , X merger name
#' @param d_df_aggnames2 <- X merger name
#' @param d_df2 <- DT
#' @param d_df2_names <- "Start"
#' @param d_tabx , "BTABx" 
#' @param d_taby , "BTABy" 
#'
#' @return A list
#' @export


dyad.maker1 <- compiler::cmpfun( function(
    dyad, d_times, t_span,
	d_df, d_df_id, d_df_names, 
	d_df2, d_df2_names, 
	d_df2_aggnames1, d_df2_aggnames2,
	d_tab=NULL, d_tabx, d_taby ) {

	## Setup d_tab if null
	if( is.null(d_tab) ) {
		# Dyadic Table (Empty)
		d_tab  <- table( d_df[[ d_df_id ]], d_df[[ d_df_id ]])*0
		class(d_tab) <- "matrix"
		d_tabx <- as.integer( rownames(d_tab) )
		d_taby <- as.integer( colnames(d_tab) )
	}

	for(t in d_times){
	
		## Setup Table to Fill
		d_tab_t <- d_tab
		TIMEt   <- rep(t, nrow(d_tab_t))
		rowids  <- which(d_df2[,d_df2_names] >= t-t_span & d_df2[,d_df2_names] < t)
		d_df_t  <- d_df2[ rowids, ]
	
		## Filling Up Dyadic Table, 	
		if( nrow(d_df_t) > 0) {
			for(i in 1:nrow(d_df_t) ){
				ddd  <- d_df[,d_df_names ] 
				b1x  <- d_df[ ddd == d_df_t[,d_df_aggnames1 ][[i]], d_df_id]
				b2y  <- d_df[ ddd == d_df_t[,d_df_aggnames2 ][[i]], d_df_id]
				s    <- sum( identical(b1x, integer(0)),
				    identical(b2y, integer(0)) ) + sum( is.na(b1x), is.na(b2y) )
				#print(c(b1x, b2y))
				if(s <= 0){
					MM1x <- sapply( b1x, grep, d_tabx)
					MM1y <- sapply( b2y, grep, d_taby)
					d_tab_t[MM1x, MM1y]  <- d_tab_t[MM1x, MM1y]+1
				} #else{print(NA)}
			}
		} #else {print("Empty")}
	
		## Combining Duplicates to LowerTriangular Matrix,	
		#message("triangle matrix")
		DyadTable.U <- t(upper.tri(d_tab_t)*d_tab_t)
		DyadTable.L <- lower.tri(d_tab_t)*d_tab_t
		DyadTable.D <- diag(d_tab_t)*diag(nrow(d_tab_t))
		DyadTable   <- DyadTable.U + DyadTable.L + DyadTable.D
		DyadTable[upper.tri(DyadTable)] <- NA
	
		## Saving to DYAD List
		dyad[[which(names(dyad)==t)]][[2]] <- DyadTable
		dyad[[which(names(dyad)==t)]][[3]] <- t
	
	}
	return(dyad)
})
