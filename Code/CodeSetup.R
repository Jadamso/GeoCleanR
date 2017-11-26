#-------------------------------------------------------------------
##################
# Libraries
##################

## Imported Packages: (.packages())
rfiles <- c("raster", "sp", "rgeos", "rgdal", "maptools",
    "spam", "spam64", "gdalUtils", "fields", "cleangeo")
for( i in rfiles) {
    devtools::use_package( i, pkg=packg)
}

# devtools::use_package( i, "Suggests", pkg=pdir)}

#install.packages("sf")
#devtools::install_github("thk686/rgdal2")
# library(spacetime)

#-------------------------------------------------------------------
##################
# Common Projections Used
################## 
proj.m <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
proj.w <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

devtools::use_data(proj.m, proj.w, overwrite=T, pkg=packg)

#-------------------------------------------------------------------
##################
# Add Codes
################## 

rfile <- c(
    "DownloadGeography.R",
    "TRI.R",
    "ExtractClosest.R",
    "DYADmaker.R",
    "getSmallPolys.R",
    "DF2Raster.R",
    "Raster2DF.R",
    "RasterMisc.R",
    "CountyCleaner.R"
)

rfiles <- paste0(pdir,"Code/R_Code/",rfile)

# Move Code
file.copy(rfiles, rdir, overwrite=T )
devtools::load_all( rdir )

# Create Code Documentation
devtools::document( pkg=packg)


