#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: EXploratory Analysis
#Coder: Nate Jones
#Date: 12/31/2020
#Purpose: Demo script to define areas of gully formation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Complete!
#Step 1: Delineate watershed and clip DEM
#Step 2: Define Stream Line
#Step 3: Create Cross section
  
#Still need to go!
#Step 4: Extract elevation data
#Step 5: Define metrics that could be used to identify gullies

#Issues --
  # When xs are near branches, they grab segments from the other side of the split.  
  # One solution, define branches, and clip to branch of interest before investigating
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: Setup Workspace -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear Memory
remove(list=ls())

#Load libraries of interest
library(tidyverse) #join the cult!
library(raster)
library(sf)
library(whitebox)
library(stars)

#Define dir of interest
data_dir<-"data/I_data/"
scratch_dir<-"data/II_scratch/"
output_dir<-"data/III_output/"

#define dem
outlet<-st_read(paste0(data_dir,"tanglewood_south_outlet.shp"))
dem<-raster(paste0(data_dir, "USGS_one_meter_x43y364_AL_ADECA_B1_2016.tif"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2: Delineate Watershed ---------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create function to delineate watershed
#2.1 Create function to create watershed shape~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
watershed_fun<-function(outlet, dem, scratch_dir){
  
  #Load libraries of interest
  library(tidyverse) #join the cult!
  library(raster)
  library(sf)
  library(whitebox)
  library(stars)
  
  #Export DEM to scratch workspace
  writeRaster(dem, paste0(scratch_dir,"dem.tif"), overwrite=T)
  
  #Smooth DEM
  wbt_gaussian_filter(
    input = "dem.tif", 
    output = "dem_smoothed.tif",
    wd = scratch_dir)
  
  #breach depressions
  wbt_breach_depressions(
    dem =    "dem_smoothed.tif",
    output = "dem_breached.tif",
    fill_pits = F,
    wd = scratch_dir)
  
  #Flow direction raster
  wbt_d8_pointer(
    dem= "dem_breached.tif",
    output ="fdr.tif",
    wd = scratch_dir
  )
  
  #Flow accumulation raster
  wbt_d8_flow_accumulation(
    input = "dem_breached.tif",
    output = "fac.tif",
    wd = scratch_dir
  )
  
  #Create Stream Layer
  stream<-raster(paste0(scratch_dir,"fac.tif"))
  stream[stream<10000]<-NA
  writeRaster(stream, paste0(scratch_dir,"stream.tif"), overwrite=T)
  
  #Paste point points in scratch dir
  st_write(outlet, paste0(scratch_dir,"pp.shp"), delete_dsn = T)
  
  #Snap pour point
  wbt_jenson_snap_pour_points(
    pour_pts = "pp.shp", 
    streams = "stream.tif",
    snap_dist = 100,
    output =  "snap.shp",
    wd= scratch_dir)
  
  #2.4 Delineat watersheds~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  wbt_watershed(
    d8_pntr = "fdr.tif",
    pour_pts = "snap.shp", 
    output = "sheds.tif" ,
    wd=scratch_dir)
  
  #load watershed raster into R env
  sheds<-raster(paste0(scratch_dir,"sheds.tif"))
  
}

#2.2 Execute function and clip dem~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create watershed raster
shed<-watershed_fun(outlet, dem, scratch_dir)

#Mulitply times dem
dem<-dem*shed

#2.3 Plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mapview(
  dem,
  alpha.regions=0.9,
  map.types=c("OpenTopoMap"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 3: Create Stream Layer ---------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.1 Create function to create stream layer ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stream_fun<-function(dem, threshold_m2, scratch_dir){

  #Load libraries of interest
  library(tidyverse) #join the cult!
  library(raster)
  library(sf)
  library(whitebox)
  library(stars)
  
  #Export DEM to scratch workspace
  writeRaster(dem, paste0(scratch_dir,"dem.tif"), overwrite=T)
  
  #Smooth DEM
  wbt_gaussian_filter(
    input = "dem.tif", 
    output = "dem_smoothed.tif",
    wd = scratch_dir)
  
  #breach depressions
  wbt_breach_depressions(
    dem =    "dem_smoothed.tif",
    output = "dem_breached.tif",
    fill_pits = F,
    wd = scratch_dir)
  
  #Flow direction raster
  wbt_d8_pointer(
    dem= "dem_breached.tif",
    output ="fdr.tif",
    wd = scratch_dir
  )
  
  #Flow accumulation raster
  wbt_d8_flow_accumulation(
    input = "dem_breached.tif",
    output = "fac.tif",
    wd = scratch_dir
  )
  
  #Create Stream Layer
  stream<-raster(paste0(scratch_dir,"fac.tif"))
  stream[stream<threshold_m2]<-NA
  writeRaster(stream, paste0(scratch_dir,"stream.tif"), overwrite=T)
  
  #Convert stream to vector
  wbt_raster_streams_to_vector(
    streams = "stream.tif",
    d8_pntr = "fdr.tif",
    output = "streams.shp",
    wd = scratch_dir)
  
  #Read streams layer in 
  streams<-st_read(paste0(scratch_dir,"streams.shp"), crs=st_crs(dem@crs))

  #Export streams
  streams
}

#Apply streams function ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
streams<-stream_fun(dem, threshold_m2=2500, scratch_dir)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 4: Create XS's -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.1 Define XS location along flowline ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Define distance between cross sections
dist<- 100 #m

#Estimate number of cross sections to create based on distance
n_points<-sum(st_length(streams))/dist 
n_points<-n_points %>% as.numeric(.) %>% round()

#Create points along flow lines
stream_pnts<-st_union(streams)
stream_pnts<-st_line_merge(stream_pnts)
stream_pnts<-as_Spatial(stream_pnts, cast=FALSE)
stream_pnts<-spsample(stream_pnts, n = n_points, type="regular")
stream_pnts<-st_as_sf(stream_pnts)

#4.2 Define XS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create fun to create cross sections
xs_fun<-function(n, width=100){
  #For testing
  pnt<-stream_pnts[n,]
  
  #Define flowline segment
  reach<-st_intersection(streams, st_buffer(pnt, dist = 1))
  reach_backup<-reach
  
  #Estimate planar slope
  reach<-st_coordinates(reach)
  reach_slope<-(reach[1,"Y"]-reach[nrow(reach),"Y"])/(reach[1,"X"]-reach[nrow(reach),"X"])
  
  #Estimate inverse slope
  xs_slope <- -1/reach_slope
  
  #Estimate endpoints of XS
  xs_coord <- st_coordinates(pnt)
  xs_coord <-rbind(
    xs_coord, 
    matrix(0, nrow=2, ncol=2)
  )
  xs_coord[2,"X"] <- xs_coord[1,"X"] + width/2*cos(atan(xs_slope))
  xs_coord[2,"Y"] <- xs_coord[1,"Y"] + width/2*sin(atan(xs_slope))
  xs_coord[3,"X"] <- xs_coord[1,"X"] - width/2*cos(atan(xs_slope))
  xs_coord[3,"Y"] <- xs_coord[1,"Y"] - width/2*sin(atan(xs_slope))
  xs_coord<-xs_coord[-1,]
  
  #Create XS
  xs<-xs_coord %>%  
    as_tibble() %>% 
    st_as_sf(coords = c("X","Y")) %>% 
    st_coordinates() %>% 
    st_linestring() %>% 
    st_sfc(.) %>% 
    st_set_crs(st_crs(dem@crs)) %>% 
    st_as_sf() 
  
  #Export XS Shape
  xs
}

#Apply function
xs <- lapply(X=seq(1, nrow(stream_pnts)), FUN = xs_fun) %>% bind_rows(xs)

#4.3 Plot for funzies ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Plot for funzies
mapview(
  dem,
  alpha.regions=0.9,
  map.types=c("OpenTopoMap")) +
  mapview(streams, color=c("dark blue"))+
  mapview(stream_pnts, col.regions=c("dark orange")) +
  mapview(xs, color=c("dark red"))