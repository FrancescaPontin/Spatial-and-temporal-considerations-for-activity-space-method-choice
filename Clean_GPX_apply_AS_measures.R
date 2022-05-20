#### code provided using Christchurch NZ as an example location for OSM data,

### Original GPS data is not provided inline with data security and disclosure risk preventative measures


# load required packages
library(sf)
library(dplyr)
library(tmap)
library(maptools)
library(sp)
library(aspace)
library(rgdal) 
library(raster)
library(spatialEco)
library(tidyverse)
library(ggplot2)
library(stats) 
library(broom)
library(viridis)
library(spatstat)
library(osmdata)
library(tidygraph)
library(igraph)



# set working directory (source file location)
setwd("/Users/franpontin/OneDrive - University of Leeds/PhD/PhD_Analysis/OIV_NZ/Activity_space_measures")
# get required funcitons
source("Activity_space_functions.R")

# set tmap mode to inteactive view
tmap_mode('view')
# set max. number of mapping facets to 10
tmap_options(limits = c(facets.view = 10))


#### DATA PRE_PROCESSING ####


# read in all GPX files in directory into 1 spatial lines df

# add filpath to folder contianing GPS data (can be in multiple files)
# file name is used as a unique ID column in resultign geodataframe
# recommend saving each date as a unique file and naming the file with said date
# e. 10_02_2022.gpx, 11_02_2022.gpx etc...
gps_list <- spatial_dfs_from_GPX("gpx_files_nz/")
gps_pts <- gps_list$sdf_pts
gps_lines <- gps_list$sdf_lines


# plot GPS to check completeness of data
tm_shape(gps_pts)+tm_dots()+
  tm_shape(gps_lines)+tm_lines()


# keep uncleaned version for comparison/ reference
gps_pts_unclean <- gps_pts
gps_lines_unclean <- gps_lines


# get osm for area as a single layer, e.g. Christchurch NZ
Christchurch_network <- get_clean_osm("christchurch New Zealand") # may take a while to run

# plot OSM network
#tm_shape(Christchurch_network)+tm_lines()

# get network for area
# for later shortest route analysis 
Christchruch_network_list <- create_network(Christchurch_network)

# get nodes, edges, grpah and coord elemetns from function output list
gps_nodes <- Christchruch_network_list[[1]]

gps_edges <- Christchruch_network_list[[2]]

gps_graph <- Christchruch_network_list[[3]]

gps_coords <- Christchruch_network_list[[4]]


gps_coords

st_write(gps_nodes,'generated_data/gps_nodes.shp',delete_layer = TRUE)
st_write(gps_edges,'generated_data/gps_edges.shp',delete_layer = TRUE)
write.csv(gps_coords,'generated_data/gps_coords.csv')
write.csv(gps_graph,'generated_data/gps_graph.csv')

# get the nearest node on OSM network for every GPS point
# (overwrites current spatial points df)
# these are referred to as cleaned GPS points 
gps_pts <-get_nearest_node(gps_pts, gps_coords, gps_nodes) # may take a while to run

# plot to see cleaned point snapped to enarest network node
tm_shape(gps_pts)+tm_dots()



# get average (median) distance between consecutive nodes
gps_pts_distance <- NA
n.values <- nrow(gps_pts)
for (i in c(1:n.values)){
  gps_pts_distance[i] <- as.numeric(st_distance(gps_pts$geometry[i] ,gps_pts$geometry[i+1]))}
median(gps_pts_distance, na.rm= TRUE)

# get the shortest path route between cleaned GPS points  

# calculate route between the GPX point (origin) to the next GPX point (destination), 
# for all cleaned GPX points 

gps_lines_network <-  network_route(gps_pts,gps_graph, gps_nodes,gps_edges) # may take a while to run

namelist <- as.list(unique(gps_lines_network$name))
gps_lines_sf = data.frame()
for (i in namelist){
  temp <- data.frame(gps_lines_network[gps_lines_network$name == i,]%>% st_combine())
  temp$name <-as.character(i)
  gps_lines_sf <- rbind(gps_lines_sf, temp)
}

gps_lines_sf <- st_as_sf(gps_lines_sf)
tm_shape(gps_lines_sf)+tm_lines()




# date time formatting
gps_pts <- point_duration(gps_pts)

# get sdf of x and y coordinates in separate columns for later analysis
gps_x_y <- get_coords_df(gps_pts)



# Get distance (euclidean) between points

# define blank column
gps_pts$distance <- NA
# set crs to match across origin and destination
gps_pts$destination_clean<-st_set_crs(gps_pts$destination_clean, st_crs(gps_pts$origin_clean))


# get n rows
n.values <- nrow(gps_pts)
for (i in c(1:n.values)){
  gps_pts$distance[i] <- as.numeric(st_distance(gps_pts$origin_clean[i] ,gps_pts$destination_clean[i]))}

gps_pts$speed <- gps_pts$distance/ gps_pts$time_diff

# Define transport mode
gps_pts$mode<-NA
gps_pts[gps_pts$speed >5, "mode"] <- "driving"
gps_pts[gps_pts$speed  <=5, "mode"] <- "active_travel"
gps_pts[gps_pts$distance >1000, "mode" ] <- "driving"

tm_shape(gps_pts)+tm_dots("mode")+tm_shape(gps_lines_network)+tm_lines("name")
gps_pts

# save point and line sdfs to shp files
st_write(gps_pts,'generated_data/gps_pts.shp',delete_layer = TRUE) # warning overwites previous version
st_write(gps_lines,'generated_data/gps_lines.shp',delete_layer = TRUE) # warning overwites previous version
st_write(gps_lines_network,'generated_data/gps_lines_network.shp',delete_layer = TRUE) # warning overwites previous version
st_write(gps_lines_sf,'generated_data/gps_lines_sf.shp',delete_layer = TRUE) # warning overwites previous version
###############

ggplot() +
  geom_sf(data = gps_graph_copy %>% activate(edges) %>% as_tibble() %>% st_as_sf(), col = 'grey50') + 
  geom_sf(data = gps_graph_copy %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), aes(col = betweenness, size = betweenness)) +
  scale_colour_viridis_c(option = 'inferno') +
  scale_size_continuous(range = c(0,4))


ggplot() +
  geom_sf(data = gps_graph_copy %>% activate(edges) %>% as_tibble() %>% st_as_sf(), aes(col = betweenness, size = betweenness)) +
  scale_colour_viridis_c(option = 'inferno') +
  scale_size_continuous(range = c(0,4))
############################## LEEDS #########################


### GET KEY LOCATIONS, HOME LOCATION AND WORK LOCATION ###  
## Key locations##

# get points where individuals spend > than x ammount of time (in seconds), specifying the estimated GPS error (meters)
# e.g. points where individuals spend >10 minutes (600 seconds) and GPS error thought to be 50m 

  # gets points where time difference is >= to the time specified 
  # then gets the centroid of those points within 50m of each other,
  # taking the centorid of those close points and then re-cleaning 
  # that point to the nearest road network node


# Need to consider appropriate projection for accurate distance calculations
# 2193: NZ traverse mercator projection
# 27700: UK british national grid (traverse mercator projection)

gps_key_locations <- get_key_locations(gps_pts, gps_pts$tim_dff, 600, 50, 2193, gps_coords, gps_nodes)
st_write(gps_key_locations,'generated_data/gps_key_locations.shp',delete_layer = TRUE) # warning overwites previous version


# get sdf of x and y coordinates in sepearte columns for later analysis
gps_key_locations_x_y <- get_coords_df(gps_key_locations)

# shortest path between key locations
# note this is not a perfect measure of shortest route between key locations (needs looking into further)
gps_key_locations_lines_network <- network_route(gps_key_locations, gps_graph, gps_nodes, gps_edges)
st_write(gps_key_locations_lines_network,'generated_data/gps_key_locations_lines_network.shp',delete_layer = TRUE) # warning overwites previous version

# get straight lines joining key locations not along street network 
gps_key_locations_lines <- gps_key_locations %>% group_by(name)  %>% summarise()   %>%st_cast("LINESTRING")
st_write(gps_key_locations_lines,'generated_data/gps_key_locations_lines.shp',delete_layer = TRUE) # warning overwites previous version


## Home location ##

# if the most common first node recorded across all days of activity 
# is the same as the most common last node recorded across all days of activity
# this is taken to be the home location
# else the home location is the most common node 


# NOTE: may want different defintion? 
#       may want to add into fucntion to exclude somewhere in the work region (difficult if students live on campus)
#       will need to groupby students across all days to do this (at the moment run on one person so uses all sdf_pts)
gps_home_point <- home_location(gps_pts)
tm_shape(gps_home_point)+tm_dots()
## Workplace location ##




# Manually defined by box arround uni location ('at work')
gps_uni<- workplace(172.579574,172.587911,-43.520638,-43.526052, gps_nodes, gps_coords)
# get centroid workplace location
gps_uni_point <- st_as_sf(gps_uni$uni_pt)
# get area which will be defined as being in work
gps_uni_area <- st_as_sf(gps_uni$uni_area, crs=4326)

tm_shape(gps_uni_point)+tm_dots()

gps_home_point.df <-  as.data.frame(gps_home_point) 
colnames(gps_home_point.df)<- c('nodeID','geometry')
gps_home_point <- st_as_sf(gps_home_point.df, crs=4326)

gps_uni_point.df <-  as.data.frame(gps_uni_point) 
colnames(gps_uni_point.df)<- c('nodeID','geometry')
gps_uni_point <- st_as_sf(gps_uni_point.df, crs=4326)

gps_home_uni <- network_route_pts(gps_home_point, gps_uni_point, gps_graph, gps_nodes, gps_edges)
tm_shape(gps_home_uni)+tm_lines()

st_write(gps_home_point,'generated_data/gps_home_point.shp',delete_layer = TRUE) # warning overwites previous version

st_write(gps_uni_point,'generated_data/gps_home_point.shp',delete_layer = TRUE) # warning overwites previous version

st_write(gps_home_uni,'generated_data/gps_home_point.shp',delete_layer = TRUE)

#### MEASURES OF ACTIVITY SPACE ####



# read saved point and line sdfs to shp files
gps_pts<- st_read('generated_data/gps_pts.shp',) # warning overwites previous version
gps_lines<- st_read('generated_data/gps_lines.shp') # warning overwites previous version
gps_lines_network<- st_read('generated_data/gps_lines_network.shp') # warning overwites previous version
gps_lines_sf<- st_read('generated_data/gps_lines_sf.shp') # warning overwites previous version

gps_key_locations<- st_read('generated_data/gps_key_locations.shp') # warning overwites previous version

gps_key_locations_lines_network <- st_read('generated_data/gps_key_locations_lines_network.shp')
gps_key_locations_lines <- st_read('generated_data/gps_key_locations_lines.shp') # warning overwites previous version



#### ALL MOVEMENT ####

## BUFFER ##
gps_network_buffer_80 <- get_network_buffer(gps_lines, 80)
tm_shape(gps_network_buffer_80)+tm_polygons()
st_write(gps_network_buffer_80,'generated_data/gps_network_buffer_80.shp',delete_layer = TRUE) # warning overwites previous version

gps_x_y <- get_coords_df(gps_pts)

## SDE ##
gps_sde   <-get_sde(gps_x_y)
tm_shape(gps_sde)+tm_polygons()
st_write(gps_sde,'generated_data/gps_sde.shp',delete_layer = TRUE) # warning overwites previous version



## CONVEX HULL ##
gps_convex <- get_convex_hull(gps_lines)
tm_shape(gps_convex )+tm_polygons()
st_write(gps_convex,'generated_data/gps_convex.shp',delete_layer = TRUE) # warning overwites previous version


#### LOCATIONS VISITED ####
# key locations (kl)

## BUFFER ##
gps_kl_buffer_1km <- get_point_buffer(gps_key_locations, 1000)
tm_shape(gps_kl_buffer_1km)+tm_polygons('name')
st_write(gps_kl_buffer_1km,'generated_data/gps_kl_buffer_1km.shp',delete_layer = TRUE) # warning overwites previous version



## SDE ##
gps_key_locations_x_y <- get_coords_df(gps_key_locations)
gps_kl_sde <- get_sde(gps_key_locations_x_y)
tm_shape(gps_kl_sde)+tm_polygons('name',aplha=0.5)
st_write(gps_kl_sde,'generated_data/gps_kl_sde.shp',delete_layer = TRUE) # warning overwites previous version

## CONVEX HULL ##

# kl_convex <- get_convex_hull(key_locations_lines_network) # does not work well with issue with network route
gps_kl_convex <- get_convex_hull(gps_key_locations_lines)
tm_shape(gps_kl_convex)+tm_polygons('name',alpha=0.5)
st_write(gps_kl_convex,'generated_data/gps_kl_convex.shp',delete_layer = TRUE) # warning overwites previous version


## SPECIFIC ROUTE ##
# example only uses two locations- home and work

## BUFFER ###
## Locations only 

# specify buffer size in m e.g. 1000m = 1km
home_buffer_1km <- get_point_buffer(gps_home_point, 1000)
work_buffer_1km <- get_point_buffer(gps_uni_point, 1000)

# combine to one dataframe to plot multiple buffers of interest
home_work_buffer_1km <- rbind(home_buffer_1km, work_buffer_1km)

# plot
tm_shape(home_work_buffer_1km)+tm_polygons(alpha=0.5)

# Get route network between two points of interest e.g. home and work locations
home_uni <- network_route_pts(gps_home_point, gps_uni_point, gps_graph)
home_uni <- st_as_sf(gps_home_uni,crs=4326)
tm_shape(home_uni) +tm_lines()


