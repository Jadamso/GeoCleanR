#########################
# Geo Packages and Functions
#########################
library(sp) 
library(maptools)  # GoeMaps, Shapefiles
library(raster)    # Raster and Clipping
library(rgeos)     # Clipping Shapefile
library(rgdal)     # Read & Write GeoFiles
library(spam)      # spam_rdist betterthan fields::rdist
library(spam64)
library(fields)    # Great-Circle Distance
library(gdalUtils) #
library(cleangeo)
library(GeoCleanR)
#install.packages("sf")
#devtools::install_github("thk686/rgdal2")
# library(spacetime)

#-------------------------------------------------------------------
##################
# Common Projections Used
################## 
proj.m <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
proj.w <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


#-------------------------------------------------------------------
##################
# Common Functions Used
################## 

source("~/Desktop/Common/R_Code/DownloadGeography.R")

source("~/Desktop/Common/R_Code/getSmallPolys.R")

source("~/Desktop/Common/R_Code/TRI.R")

source("~/Desktop/Common/R_Code/ExtractClosest.R")

source("~/Desktop/Common/R_Code/Raster2DF.R")


#-------------------------------------------------------------------
##################
# Unused
################## 
# Where to store temporary rasters
#if( system("echo $HOME",intern=TRUE)== "/home/jadamso"){
#    rasterOptions(tmpdir="/scratch1/jadamso")
#} else {
# rasterOptions(tmpdir="/tmp")
#}

# coerce stsdf to stfdf
# library(spacetime)
# https://github.com/edzer/spacetime/blob/master/R/coerce.R


