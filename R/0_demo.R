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
library(fasterize)
library(mapview)
library(parallel)

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
dist<- 50 #m

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
xs_fun<-function(n, width=200){
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
xs <- lapply(X=seq(1, nrow(stream_pnts)), FUN = xs_fun) %>% bind_rows(.)

#4.3 Plot for funzies ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Plot for funzies
mapview(
  dem,
  alpha.regions=0.9,
  map.types=c("OpenTopoMap")) +
  mapview(streams, color=c("dark blue"))+
  mapview(stream_pnts, col.regions=c("dark orange")) +
  mapview(xs, color=c("dark red"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 5: Extract elevation data ------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5.1 Create function to extract elevation data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
elevation_fun<-function(n){
  
  #Call Libraries of interest
  library(tidyverse) #join the cult!
  library(raster)
  library(sf)
  
  #isolate xs
  xs_temp<-xs[n,]
  
  #convert to raster
  m<-dem*0+1
  m<-crop(m, st_buffer(xs_temp,250))
  xs_temp<-rasterize(xs_temp, m)
  
  #Add values
  xs_temp<-xs_temp*dem
  
  #Convert to points
  xs_temp<-rasterToPoints(xs_temp)

  #Convert to 2D XS
  xs_temp <- xs_temp %>% 
    #Convert to tibble
    as_tibble() %>% 
    #Estimate distance along transect using distance formula
    mutate(
      dist = ((x[1]-x)^2 + (y[1]-y)^2)^0.5
    ) %>% 
    #Select cols of interest
    dplyr::select(dist, ele = layer)
  
  #Interpolate in the x and y directions
  interp_fun<-approxfun(xs_temp$dist, xs_temp$ele)
  xs_temp<-tibble(
    dist = seq(0, max(xs_temp$dist), 1),
    ele  = interp_fun(seq(0, max(xs_temp$dist), 1)), 
    xs_id = n)
  
  #Export data
  xs_temp
}

#5.2 Execute function~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Define number of cores
n_cores<-detectCores()-1

#Create cores
cl<-makeCluster(n_cores)

#Send xs data to cores
clusterExport(cl, c('dem','xs'))

#Apply function
xs_ele<-parLapply(cl, X=seq(1, nrow(xs)), fun = elevation_fun) %>% bind_rows()

#Stop clusters
stopCluster(cl)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 6: Estimate XS area, w/d ratio, and geomorphic unit  ---------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#6.1 Create function ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
xs_metrics_fun<-function(n){
  
  #Call Libraries of interest
  library(tidyverse) #join the cult!
  
  #Identify XS of interest
  id<-xs_ele %>% dplyr::select(xs_id) %>% unique()
  id<-id$xs_id[n]
  
  #Isolate data
  xs_ele<-xs_ele %>% filter(xs_id == id)
  
  #Identify the valley bottom
  invert_ele<-xs_ele %>% 
    filter(dist>95, dist<105) %>% 
    filter(ele == min(ele)) %>% 
    dplyr::select(ele) %>% pull()
  
  invert_dist<-xs_ele %>% 
    filter(dist>95, dist<105) %>% 
    filter(ele == min(ele)) %>% 
    dplyr::select(dist) %>% pull()
  
  #Create function to estimate metrics from XS
  metrics_fun<-function(dz){
    #Identify points below invert_ele + dz
    df<-xs_ele %>% filter(ele <= invert_ele+dz)
    
    #Define groups of connected points
    df<-df %>% 
      mutate(dx = dist-lag(dist), 
             dx = replace_na(dx,0)) %>% 
      mutate(group_marker = ifelse(dx==1,0,1)) %>% 
      mutate(group = cumsum(group_marker)) 
    
    #Identify area connected to channel in question
    xs_group<-df %>% 
      filter(dist==invert_dist) %>% 
      dplyr::select(group) %>% 
      pull()
    
    #Limit xs to chanel/valley in question
    df<-df %>% filter(group==xs_group) %>% dplyr::select(-group) %>% dplyr::select(-group_marker) %>% dplyr::select(-dx)
    
    #Estimate metrics
    #XS Area
    output<-df %>% 
      mutate(
        dx_lag = (dist - lag(dist))/2,
        dx_lag = replace_na(dx_lag,0), 
        dx_lead = (lead(dist)-dist)/2, 
        dx_lead = replace_na(dx_lead,0), 
        dx = dx_lag + dx_lead, 
        dy = (dz + invert_ele) - ele, 
        dA = dx*dy) %>% 
      summarise(
        dz = dz,
        a_xs = sum(dA), 
        d_mean = mean(dy),
        w_mean = a_xs/d_mean,
        d_max = dz, 
        w_max = max(dist)-min(dist), 
        w_d_ratio = w_max/d_max)
      
    #Export output
    output 
  }
  
  #Apply metrics
  dz<-max(xs_ele$ele)-invert_ele
  metrics<-lapply(X=seq(0,dz,by=0.05), FUN = metrics_fun) %>% bind_rows()
  
  #Add Unique ID
  metrics<-metrics %>% mutate(xs_id = id)
  
  #Export metrics
  metrics
}

#6.2 Execute function~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Define number of cores
n_cores<-detectCores()-1

#Create cores
cl<-makeCluster(n_cores)

#Send xs data to cores
clusterExport(cl, c('xs_ele'))

#Apply function
xs_metrics<-parLapply(cl, X=seq(1, nrow(xs)), fun = xs_metrics_fun) %>% bind_rows()

#Stop clusters
stopCluster(cl)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 7: Identify of entrenchment  ---------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


m<-mapview(
  dem,
  alpha.regions=0.9,
  map.types=c("OpenTopoMap")) +
  mapview(streams, color=c("dark blue"))+
  mapview(stream_pnts, col.regions=c("dark orange")) +
  mapview(xs, color=c("dark red"))

htmlwidgets::saveWidget(m, "initial_map.html")
