# Create a Dyadic DataFrame
# DYADmaker0, DYADmaker1, DYADmaker2
# default parameters in Paper6 RBLOCK 012
# @name DYADmaker
# NULL

#------------------------------------------------------------------
##################
#' Make Dyad Skeleton
################## 
#' 
#' @param times
#' @param times 
#'
#' @return An empty list to be filled in DYADmaker1
#' @export

DYADmaker0 <- compiler::cmpfun( function(
    times,
    dyad_name=c("Political", "Dyad", "Year")){
    
	dyad <- list(NA, NA, NA)
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
#' @param d_df  , X variables to merge (POLIS2)
#' @param d_df_var , X variable names
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

DYADmaker1 <- compiler::cmpfun( function(
    dyad, d_times, t_span,
	d_df, d_df_var, d_df_id, d_df_names, 
	d_df_aggnames1, d_df_aggnames2,
	d_df2, d_df2_names, 
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
	
		## Setup
		#message("setup")
		d_tab_t <- d_tab
		TIMEt   <- rep(t, nrow(d_tab_t))
		POLt    <- d_df[ , paste0(d_df_var, t)]
		names(POLt) <- d_df[[ d_df_id ]]
		rowids  <- which(d_df2[,d_df2_names] >= t-t_span & d_df2[,d_df2_names] < t)
		d_df_t  <- d_df2[ rowids, ]
	
		## Filling Up Dyadic Table, 	
		#message("fill table")
		if( nrow(d_df_t) > 0) {
			for(i in 1:nrow(d_df_t) ){
				#i<-178
				b1x  <- d_df[d_df[,d_df_names ] == d_df_t[,d_df_aggnames1 ][[i]], d_df_id]
				b2y  <- d_df[d_df[,d_df_names ] == d_df_t[,d_df_aggnames2 ][[i]], d_df_id]
				s    <- sum( identical(b1x, integer(0)), identical(b2y, integer(0)) ) + sum( is.na(b1x), is.na(b2y) )
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
	
		## Saving to DYAD List, 
		#message("Dyad list")
		dyad[[which(names(dyad)==t)]][[1]] <- POLt
		dyad[[which(names(dyad)==t)]][[2]] <- DyadTable
		dyad[[which(names(dyad)==t)]][[3]] <- t
	
	}
	return(dyad)
})


#------------------------------------------------------------------
##################
#' Dyad List Formatting
################## 
#'
#' @param dfname=c("Polis1", "Polis2", "Battles", "Pol1.Politic", "Pol2.Politic", "Year")
#' @param dyad , DYAD
#'
#' @details Transforming List into Dyadic DF (Part 3)
#'
#' @return A dataframe with Battle and Political Data
#' @export

DYADmaker2 <- compiler::cmpfun( function(dfname, dyad, ...){ 
 	requireNamespace("igraph")
	requireNamespace("parallel")

	# Transforming List into Dyadic DF
	## Parallel
	DF0 <- parallel::mclapply( 1:length(dyad), ...,function(i){
		# Battle Dyad Info
		g <- graph.adjacency(dyad[[i]][[2]], weighted=TRUE)
		edge.attributes(g)$weight[ is.na(edge.attributes(g)$weight) ] <- 0
		df <- get.data.frame(g)
		
		## Political Dyad Info
		PPP   <- dyad[[i]][[1]]
		df$p1 <- sapply(df[,1], function(e) PPP[which(names(PPP)==e)] )
		df$p2 <- sapply(df[,2], function(e) PPP[which(names(PPP)==e)] )
		
		## Year Info
		df$t  <- dyad[[i]][[3]]
		df
	}
	)
	
	DF <- do.call("rbind", DF0)
	names(DF) <- dfname

	## Create Dyad ID's: ## http://stackoverflow.com/questions/13566562/creating-a-unique-id-in-r
	## ID's as integers
	DF[,dfname[1]] <- as.numeric(DF[,dfname[1]])
	DF[,dfname[2]] <- as.numeric(DF[,dfname[2]])
	## Dyad ID is ordered by (Polis1, Polis2)
	reorderdf <- which(DF[, dfname[1]] > DF[, dfname[2]])
	DF[reorderdf,c(dfname[1:2])] <- DF[reorderdf,c(dfname[2:1])]
	## Assign Dyad ID
	comb <- with(DF, paste( get( dfname[1]), get( dfname[2])))
	DF$DYAD_ID <- match(comb, unique(comb))
	DF
})


	# Transforming List into Dyadic DF
	## Sequentially
	#DF     <- data.frame( row.names=dfname)
	#for(i in 1:length(dyad)){ 
	#	# Battle Dyad Info
	#	g <- graph.adjacency(dyad[[i]][[2]], weighted=TRUE)
	#	edge.attributes(g)$weight[ is.na(edge.attributes(g)$weight) ] <- 0
	#	df <- get.data.frame(g)
	#
	#	## Political Dyad Info
	#	PPP <- dyad[[i]][[1]]
	#	df$p1 <- sapply(df[,1], function(e) PPP[which(names(PPP)==e)] )
	#	df$p2 <- sapply(df[,2], function(e) PPP[which(names(PPP)==e)] )
	#	
	#	## Year Info
	#	df$t  <- dyad[[i]][[3]]
	#	DF    <- rbind(DF, df)
	#}
	#names(DF) <- dfname

