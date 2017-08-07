#########################
# RBLOCK 000
#########################
# http://r-pkgs.had.co.nz

## use for R-3.2.2 for support on Palmetto Cluster
#/home/Jadamso/R-3.2.2/bin/R
# https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html

library(devtools)
library(roxygen2)
source( "~/Desktop/Common/R_Code/dir.create.safe.R")
#------------------------------------------------------------------
##################
# Author Info
################## 

#devtools::session_info()
authors_at_r <- paste0( "'",
    person(
        given = "Jordan",
        family= "Adamson",
        email = "jordan.m.adamson@gmail.com",
        role  = c("aut", "cre")),
    "'" )

options(devtools.name="Jordan Adamson", 
    devtools.desc.author=authors_at_r)

#------------------------------------------------------------------
##################
# Package Description
################## 

Title <- 'A Library for Processing Geospatial Data'
URL   <- 'https://sites.google.com/a/g.clemson.edu/ja-resources'
Description <- 'Functions and Packages to Process Geospatial Data in R. See my website <https://sites.google.com/a/g.clemson.edu/ja-resources>. or github <https://github.com/Jadamso>.'
#This projects adheres to semantic versioning <http://semver.org/>
Maint <- "'Jordan Adamson<jordan.m.adamson@gmail.com>'"


my_description <- list(
    # "Maintainer" = Maint, 
    "Title"      = Title,
    "Description"= Description,
    "Published"  = Sys.Date(),
    "Date"       = Sys.Date(),
    "URL"        = URL,
    "Version"    = Version
)

#------------------------------------------------------------------
##################
# CHUNK C
################## 
# Create Package

## package.skeleton
## install.packages(sinew)q


# license warning goes away with next code block

package.setup.safe(
    packg,
    my_description,
    rstudio=F,
    check=T)


rdir    <- paste0(packg, "/R")
ddir    <- paste0(packg, "/data/")
idir    <- paste0(packg, "/inst") 
extdir  <- paste0(packg, "/inst/extdata/")

for (i in c(ddir, idir, extdir) ) { dir.create.safe(i) }


#------------------------------------------------------------------
##################
# CHUNK D
################## 

## License
options( devtools.desc.license='MIT + file LICENSE' )
file.copy("~/Desktop/Common/Packages/LICENSE", paste0(packg,"/LICENSE"), overwrite=T )


## Citation
year <- as.numeric(format(Sys.Date(), "%Y"))
month <- as.numeric(format(Sys.Date(), "%m"))
note <- sprintf("R package version %s", Version)
header <- citHeader( paste0("To cite package '",pack,"' in publications use:") )
textVersion <- paste0(
    "Jordan Adamson ",
    paste0( "(", year, "). "),
    paste0(pack,": ", Title, ". "),
    note, ".")

bibref <- bibentry(
    bibtype = "Manual",
    key = pack, 
    title = paste0("{",pack,"}: ", Title),
    textVersion = textVersion,
    author = authors_at_r,
    header = header, 
    year = year,
    month = month,
    note = note, 
    url = URL)
print(bibref, style = "citation")


writeLines(
    paste(format(bibref, "R"),
    collapse = "\n\n"),
    con=paste0(idir, "/CITATION"))

#devtools::check(pkg=packg)

