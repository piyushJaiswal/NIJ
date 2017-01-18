install.packages("rgdal")
install.packages("sp")
install.packages("maptools")

library(sp)
library(maptools)
library(rgdal)
library(rgeos)

# Use coordinates(df) = lat + long... transforms a table to spatialpointsdataframe

# Get the spatial refernce using proj4string
projInfo(type = "proj")
CRS("+init=epsg:4269")

# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf

# Read Sample Submission using readOGR
    # !!!! Not working
fileOGR = readOGR(dsn="../SUBMISSION/NAME/ACFS/1MO/",layer="1MO/NAME_ACFS_1MO")
proj4string(dfs) <- CRS("+init=epsg:2913")
dfs <- spTransform(dfs, CRS("+init=epsg:4326"))

# Read dbf, shp and shx together
    # Gives a list
files_districts <- read.shapefile("../RAW/Portland_Police_Districts")

# Read the shapefile as a raster with a centrodal coordinate for each region
    # Gives a SpatialPolygonsDataFrame
    # coordinates can be obtained by coordinates(df)
    # data itself contains the dbf information
file_submission <- shapefile(x = "../SUBMISSION/NAME/ACFS/1MO/NAME_ACFS_1MO.shp")

# Writing dbf, shx, shp files
coords <- data.frame(x=c(7604005,7701431), y=c(651315.6,733815.4))
df = data.frame(hotspot=c(0,1),area=c(100,200))
sp <- SpatialPointsDataFrame(coords = coords,data = df, proj4string = CRS("+init=epsg:2913"))
    # writeSpatialShape in maptools
writeSpatialShape(sp,"../SUBMISSION/submission")
    # investigating the written files
file_shp = read.shp(shp.name = "../SUBMISSION/submission.shp") 
file_shp
file_dbf = read.dbf(dbf.name = "../SUBMISSION/submission.dbf") 
file_dbf
    # What are headers in these files??
file_shx = read.shx(shx.name = "../SUBMISSION/submission.shx") 
file_shx

# Write spatial files
file2 = read.shp(shp.name = "../SUBMISSION/NAME/ACFS/1MO/NAME_ACFS_1MO.shp")
spl = SpatialPolygons(list(Polygons(list(Polygon(file2$shp[[1]]$points)),1),
                           Polygons(list(Polygon(file2$shp[[2]]$points)),2)), 
                      proj4string = CRS("+init=epsg:2913"))
spld <- SpatialPolygonsDataFrame(spl,data = df)

writeSpatialShape(spld,"../SUBMISSION/submission_cells")
    # investigating the written files
file_shp = read.shp(shp.name = "../SUBMISSION/submission_cells.shp") 
file_shp
file_dbf = read.dbf(dbf.name = "../SUBMISSION/submission_cells.dbf") 
file_dbf
    # File contains SP_ID, hotspot and area. SP_ID is the field coming from the polygons object

# Test if a spatial polygon contains a spatial point
sp1 <- SpatialPolygons(list(Polygons(list(Polygon(file2$shp[[1]]$points)),1)), 
                       proj4string = CRS("+init=epsg:2913"))
point <- data.frame(lon=7637754, lat=651500)
sp2   <- SpatialPoints(point,proj4string=CRS("+init=epsg:2913"))
gContains(sp1,sp2)

# Retrieve coordinates from a polygon object
sp1 = Polygons(list(Polygon(sample_shp$shp[[1]]$points)),1)
sp1@Polygons[[1]]@coords

# check if interior point within polygons
check_interior <- function(x,y){
  point <- data.frame(lon=x, lat=y)
  sp2   <- SpatialPoints(point,proj4string=CRS("+init=epsg:2913"))
  
  found=FALSE
  for(i in 1:length(s_polygons)){
    pls <- s_polygons[[i]]
    if(gContains(pls,sp2)){
      found=T
      return(i)
    }
  }
  if(found==F)
    return(100000)
}


