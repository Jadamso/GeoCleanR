
pack  <- "GeoCleanR"
pdir  <- paste0(path.expand("~/Desktop/Packages/"),pack,"/")
packg <- paste0(pdir, pack)


Version <- numeric_version("0.1.0")

#-------------------------------------------------------------------
##################
# Setup R Package
################## 
source(paste0(pdir,"Code/PackageSetup.R") )

#-------------------------------------------------------------------
##################
# Create R Package Contents
################## 
source(paste0(pdir,"Code/CodeSetup.R") )

#-------------------------------------------------------------------
##################
# Make and Upload R Package
################## 
source(paste0(pdir,"Code/PackageMake.R") )


## importFrom(data.table, parallel)

## source("~/Desktop/Packages/GeoCleanR/Code/GeoCleanR.R")
