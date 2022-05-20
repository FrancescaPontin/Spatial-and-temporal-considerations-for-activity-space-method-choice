#### function for reading in GPX line data (from Strava) ####
GPX_format_lines <- function(path) {
  strava <- st_read(path, layer = "tracks")
  strava_lines <-st_cast(strava, 'LINESTRING')
  strava_lines$name <-path
  return(strava_lines)
}

spatial_dfs_from_GPX <- function(folder){
  # define file type and directory
  files <- list.files(path=folder, pattern="*.gpx", full.names=TRUE, recursive=FALSE)
  # get number of files
  n.values <- length(files) 
  
  # create blank dfs for data once read-in and processed
  df_lines <- data.frame(name  = files, geometry = NA, type= NA)
  #df_pts <- data.frame(name  = files, type= NA, geometry = NA)
  df_pts <- data.frame(track_seg_point_id =NA, ele =NA, time =NA, name =files, geometry =NA)          
  colnames(df)
  # apply unction for reading in GPX line data and save to df
  for (i in c(1:n.values)){ 
    # apply function
    lines <- GPX_format_lines(files[i])
    # name as filename (unique ID)
    lines$name <-files[i]
    # remove columns containg no data
    # 3 columns remaining, name, type and geometry
    lines<-lines[,colSums(is.na(lines))<nrow(lines)]
    #save each file line to new row in df
    df_lines[i,] <-lines
    # rename name  to remove .gpx and folder name
    df_lines$name<-sub('\\.gpx$', '',basename(as.character(df_lines$name)))
  }
  
  # apply unction for reading in GPX point data and save to df
  for (i in c(1:n.values)){ 
    # apply function
    pts <- GPX_format_pts(files[i])
    # name as filename (unique ID)
    pts$name <-files[i]
    # remove columns containg no data
    pts<-pts[,colSums(is.na(pts))<nrow(pts)]
    pts <-pts[c("track_seg_point_id","ele","time","name","geometry")]
    # save pt data to unique df
    df <-pts
    # append df to df contiang all pint data
    df_pts<- rbind(df_pts,df)
    #df_pts<- dplyr::bind_rows(df_pts,df)} <- previously this, above line added to remove error
    # remove any rows with missing data 
    df_pts<-df_pts[!is.na(df_pts$time),]
    #rename name to remove .gpx and folder name
    df_pts$name<-sub('\\.gpx$', '',basename(as.character(df_pts$name))) 
  }
  # create sf object from df
  sdf_pts <-st_sf(df_pts, crs=4326)
  
  # create sf object from df
  sdf_lines <-st_sf(df_lines, crs=4326)
  sdf_lines <- subset(sdf_lines, select = c(name, geometry))
  return(list(sdf_lines= sdf_lines, sdf_pts = sdf_pts))
}


#### function for reading in GPX point data (from Strava) ####
GPX_format_pts <- function(path) {
  strava <- st_read(path, layer = "track_points")
  strava_pts <-st_cast(strava, 'POINT')
  strava_pts$name <-path
  return(strava_pts)
}

##### function for getting and cleaning openstreetmap data before creating networks ####
get_clean_osm <- function(placename_string) {
  # get OSM highway features
  placename_streetmap<- opq(bbox=placename_string) %>%
    add_osm_feature(key = 'highway')%>%
    osmdata_sf()%>%
    osm_poly2line()
  placename <- placename_streetmap$osm_lines %>% 
    dplyr::select(highway) # 32961 obs
  
  #### OSM data pre-processing/cleaning ####
  placename_all <- unique(placename) # 32959 obs
  
  # get the union of all geometries in the cc_streetmap sf
  # returns a single geometry with resolved boundaries
  # prevents issues with roads joining and therefore route gaps later
  placename_union <- st_union(placename_all) # takes a while to run
  
  # cast multilinestring back to individual linestring df
  placename_linestring <- st_cast(placename_union, "LINESTRING")
  
  # df to sf
  placename.sf <-st_as_sf(placename_linestring)
  
  # check class is sf dataframe
  # class(cc_center_union_df) 
  
  # rename column x as geometry 
  placename.sf <- placename.sf%>% 
    rename(geometry=x)
  return(placename.sf)
}


#### function returning NETWORK from cleaned OSM data ####
# returns graphs edges, nodes and coords as list
# get elements of list by name  
# "node"=nodes,"edges"=edges, "graph"=graph, "coords"=coords
create_network <- function(clean_osm_sf){
  edges <- clean_osm_sf %>%
    mutate(edgeID = c(1:n()))
  nodes <- edges %>%
    st_coordinates() %>%
    as_tibble() %>%
    rename(edgeID = L1) %>%
    group_by(edgeID) %>%
    slice(c(1, n())) %>%
    ungroup() %>%
    mutate(start_end = rep(c('start', 'end'), times = n()/2))
  
  # give each node a unique index
  nodes <- nodes %>%
    mutate(xy = paste(.$X, .$Y)) %>% 
    mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) %>%
    dplyr::select(-xy)
  

  # Combine the node indices with the edges
  # define start nodes
  source_nodes <- nodes %>%
    filter(start_end == 'start') %>%
    pull(nodeID)
  # defne end nodes
  target_nodes <- nodes %>%
    filter(start_end == 'end') %>%
    pull(nodeID)

  # specify for each edge whcih node it starts and whic node it ends in
  edges = edges %>%
    mutate(from = source_nodes, to = target_nodes)
  

  # issue_nodes <- unique(edges[edges$from == edges$to,]$edgeID)
  # edges <-edges[edges$from != edges$to,]
  # `%not_in%` <- purrr::negate(`%in%`)
  # 
  # nodes <- nodes[nodes$edgeID %not_in% issue_nodes,]
 
  #remove duplciate nodes
  nodes <- nodes %>%
    distinct(nodeID, .keep_all = TRUE) %>%
    dplyr::select(-c(edgeID, start_end)) %>%
    st_as_sf(coords = c('X', 'Y')) %>%
    st_set_crs(st_crs(edges))
  
  # investigate <- list("node"=nodes,"edges"=edges)
  # return(investigate)
  #
  # Convert to tbl_graph
  graph = tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = FALSE)
  
  # get length of edge
  graph <- graph %>%
    activate(edges) %>%
    mutate(length = st_length(geometry))

  # get coordiantes of all nodes in grpah
  coords <- nodes %>%
    st_coordinates()

  network_list <- list("node"=nodes,"edges"=edges, "graph"=graph, "coords"=coords)
  return(network_list)
}


#### function that returns the nearest network node for each gps point in the spatial pts df ####
get_nearest_node <- function(spatial_pts_df, node_coordinates, nodes){
  
  # Add extra columns to fill
  spatial_pts_df$origin_clean <- NA
  spatial_pts_df$destination_clean <- 
    spatial_pts_df$origin_node_id <- NA
  spatial_pts_df$destination_node_id <- NA
  
  # get n rows
  n.values <- nrow(spatial_pts_df) -1
  for (i in c(1:n.values)){
    
    # get coordinate of point in row
    coords_o.i <- spatial_pts_df[i,]$geometry %>%
      st_coordinates() %>%
      matrix(ncol = 2)
    # get coordainte of next point (points chronological)
    coords_d.i <- spatial_pts_df[i+1,]$geometry %>%
      st_coordinates() %>%
      matrix(ncol = 2)
    # Find K nearest neighbours, where k =1 i.e. nearest neichbour only
    index_o.i <- nabor::knn(data = node_coordinates, query = coords_o.i, k = 1)
    index_d.i <- nabor::knn(data = node_coordinates, query = coords_d.i, k = 1)
    
    # save the cleaned point coordiantes and corresponding network node Id as new sf columns
    spatial_pts_df[i,"origin_clean"] <-  nodes[index_o.i$nn.idx,2 ]
    spatial_pts_df[i,"origin_node_id"] <-  as.numeric(nodes[index_o.i$nn.idx,]$nodeID)
    # also save the (cleaned) coordaintes of the next point (the desitnaiton) and the destination node ID as new sf columns
    spatial_pts_df[i,"destination_clean"] <-  nodes[index_d.i$nn.idx, 2]
    spatial_pts_df[i,"destination_node_id"] <-  as.numeric(nodes[index_d.i$nn.idx, ]$nodeID)
    
  }
  # remove duplciate origin desintation
  spatial_pts_df <-spatial_pts_df[spatial_pts_df$origin_node_id!= spatial_pts_df$destination_node_id,]
  # removed any points with Na (last point recorded cannot not have a destinaiton)
  spatial_pts_df <- drop_na(spatial_pts_df)
  
  # remove geometry column and make cleaned origin coordinates new geometry column 
  spatial_pts_df <- st_drop_geometry(spatial_pts_df)
  spatial_pts_df <- st_as_sf(spatial_pts_df, crs=4326)
  
  spatial_pts_df$geometry <-spatial_pts_df$origin_clean
  return(spatial_pts_df)
}

#### function that returns the shortest path route between all snapped to network GPS points ####
## shortest route for each unique id (name) e.g. GPX file read in ##

network_route <- function(spatial_pts_df, graph, nodes, edges){
  namelist <- as.list(unique(spatial_pts_df$name))
  namelist
  # create a blank df to fill
  df_names = data.frame()
  
  for (j in namelist){
    
    data.temp <- subset(spatial_pts_df, name == as.character(j))
    
    # define number of values (same as no points/rows in GPX point sf)
    n.values <- nrow(data.temp)
    
    # create a blank df to fill
    df_total = data.frame()
    
    for (i in c(1:n.values)){ # will take a while to run
      # define from node
      from_node <- graph %>%
        activate(nodes) %>%
        # in the network find the node which has the same value as the origin node id
        # of line i in the GPX points sf
        filter(nodeID == data.temp$origin_node_id[i]) %>%
        pull(nodeID)
      
      to_node <- graph %>%
        activate(nodes) %>%
        # in the network find the node which has the same value as the destination node id
        # of line i in the GPX points sf
        filter(nodeID == data.temp$destination_node_id[i]) %>%
        pull(nodeID)
      
      # get the shortest path between origin and desintation nodes
      path <- shortest_paths(
        # using graph as network
        graph = graph,
        from = from_node,
        to = to_node,
        # weighted to walk along shortest length path between nodes (euclidean distance) 
        # (if excluded caculates the path with fewest nodes bewteen the two nodes)
        weights =st_length(edges$geometry),
        # both directions
        output = 'both')
      
      # create a subgraph containing only nodes + edges of calcualted path
      path_graph <- graph %>%
        subgraph.edges(eids = path$epath %>% unlist()) %>%
        as_tbl_graph()
      
      # get path as single geometry using union
      path_graph_multi <- path_graph %>%
        activate(edges) %>%
        as_tibble() %>%
        st_as_sf()%>%
        st_union()
      # convert geometry to df for storage
      df <- data.frame(path_graph_multi)
      #rbind df to bottom of total df to allow storage of all paths between all nodes in one df
      df_total <- rbind(df_total,df)
      
    }
    df_total$name <-as.character(j)
    df_names <- rbind(df_names,df_total)
  }  
  spatial_network_lines <- st_as_sf(df_names)
  return(spatial_network_lines)
  
}



network_distances <- function(spatial_pts_df){
  namelist <- as.list(unique(spatial_pts_df$name))
  namelist
  # create a blank df to fill
  df_names = data.frame()
  
  for (j in namelist){
    
    data.temp <- subset(spatial_pts_df, name == as.character(j))
    
    # define number of values (same as no points/rows in GPX point sf)
    n.values <- nrow(data.temp)
    
    # create a blank df to fill
    df_total = data.frame()
    
    for (i in c(1:n.values)){ # will take a while to run
      # define from node
      from_node <- graph %>%
        activate(nodes) %>%
        # in the network find the node which has the same value as the origin node id
        # of line i in the GPX points sf
        filter(nodeID == data.temp$origin_node_id[i]) %>%
        pull(nodeID)
      
      to_node <- graph %>%
        activate(nodes) %>%
        # in the network find the node which has the same value as the destination node id
        # of line i in the GPX points sf
        filter(nodeID == data.temp$destination_node_id[i]) %>%
        pull(nodeID)
      
      # get the shortest path between origin and desintation nodes
      path <- distances(
        graph = graph, 
        v = from_node, 
        to = to_node, 
        algorithm = "automatic")
      # create a subgraph containing only nodes + edges of calcualted path
      path_graph <- graph %>%
        subgraph.edges(eids = path$epath %>% unlist()) %>%
        as_tbl_graph()
      
      # get path as single geometry using union
      path_graph_multi <- path_graph %>%
        activate(edges) %>%
        as_tibble() %>%
        st_as_sf()%>%
        st_union()
      # convert geometry to df for storage
      df <- data.frame(path_graph_multi)
      #rbind df to bottom of total df to allow storage of all paths between all nodes in one df
      df_total <- rbind(df_total,df)
      
    }
    df_total$name <-as.character(j)
    df_names <- rbind(df_names,df_total)
  }  
  spatial_network_lines <- st_as_sf(df_names)
  return(spatial_network_lines)
  
}

  
  
 
  
  
  
  



network_route_pts <- function(start_point, end_point, graph, nodes, edges){
  from_node <- start_point %>%
  pull(nodeID)
  
  to_node <- end_point %>%
  pull(nodeID)
  
  # get the shortest path between origin and desintation nodes
  path <- shortest_paths(
  # using graph as network
  graph = graph,
  from = from_node,
  to = to_node,
  # both directions
  output = 'both')

  # create a subgraph containing only nodes + edges of calcualted path
  path_graph <- graph %>%
    subgraph.edges(eids = path$epath %>% unlist()) %>%
    as_tbl_graph()
  
  # get path as single geometry using union
  path_graph_multi <- path_graph %>%
    activate(edges) %>%
    as_tibble() %>%
    st_as_sf()%>%
    st_union()
  # convert geometry to df for storage
  df <- data.frame(path_graph_multi)
  sf <- st_as_sf(df, crs=4326)
  return(sf)  
}    
  
  
  
  
  
  
  

#### function that formats datetime to calculate duration at each point in spaital points df (with timestamp) ####
point_duration <-function(spatial_pts_df){ 
  # FORMAT TIMESTAMP
  #spatial_pts_df$date <-as.Date(spatial_pts_df$time, origin = lubridate::origin,format="%Y-%m-%d %H:%M:%S")
    #spatial_pts_df$daytime<-strftime(as.Date(spatial_pts_df$time,origin=lubridate::origin), format="%H:%M:%S",)
  
  spatial_pts_df$time <-as.POSIXct(spatial_pts_df$time, origin="1970-01-01", tz='Pacific/Auckland')
  spatial_pts_df$date <-as.Date(spatial_pts_df$time)
  spatial_pts_df$daytime<-strftime(spatial_pts_df$time, format="%H:%M:%S")
  
  # get duration of time spent at each point
  spatial_pts_df$time_diff <-as.numeric(spatial_pts_df$time - lag(spatial_pts_df$time))
  spatial_pts_df$time_diff[is.na(spatial_pts_df$time_diff)] <- 0
  
  return(spatial_pts_df)
  
}

#### function that returns a df of x and y coordinates ####
##for calcualting SDE ##
get_coords_df <-function(spatial_pts_df){
  spatial_pts_x_y <- data.frame(st_coordinates(spatial_pts_df$geometry), spatial_pts_df$name)
  colnames(spatial_pts_x_y) <- c("X","Y","name")
  return(spatial_pts_x_y)
}



#### fucntion that returns the convex hull or a lines spatial df ####
get_convex_hull <- function(spatial_lines_df) {
  # get unique individuals(from lines sf df) 
  unique_ind_tag <- unique(spatial_lines_df$name)
  
  # get length of unqiue individuals (number of unique students)
  n.values <- length(unique_ind_tag)  
  
  # create blank dataframe with unique student IDs listed and blank geometry column
  convex.df <- data.frame(name  = unique_ind_tag, value.geometry = NA)
  
  #CONVEX HULL FOR LOOP #
  # loops through line geometry of all individuals 
  for (i in 1:n.values){
    
    #get value in row i of the "i" column
    # get ID of individual in row i of the blank df
    i.current_ind <- as.character(convex.df[i,"name"])
    
    # get the line geometry of the individual with selected ID
    i.ind <- spatial_lines_df[spatial_lines_df$name==i.current_ind,]
    
    # apply the st_convex_hull methods to the sf object 
    i.ind_convex <- st_convex_hull(i.ind)
    
    # save the geometry of the convex polygon to the geometry column
    convex.df[i,"value.geometry"] <- i.ind_convex[2]
  }
  # create Sf object from convex dataframe
  ind_convex <-st_sf(convex.df, crs=4326)
  return(ind_convex)
  
}


#### function that returns the SDE of spatial coordinates df ####
get_sde <- function(spatial_pts_x_y) {
  
  # get unique individuals (from points sf df)  
  unique_inds <- unique(spatial_pts_x_y$name)
  
  # create blank dataframe with unique student IDs listed and blank geometry column
  # will fill in df using for loop
  sde.df <- data.frame(name  = unique_inds, value.geometry = NA)
  # get length of unqiue individuals (number of unique students)  
  n.values <- length(unique_inds)  
  
  # SDE ELLIPSE FOR LOOP
  # loops through x and Y coordinates of an individual's point geometry, of all individuals
  # (may take a while to run)
  for (i in 1:n.values){
    
    #get value in row i of the "i" column
    # get ID of individual in row i of the blank df
    i.current_ind <- as.character(sde.df[i,"name"])
    
    # get all point geometries of the individual with selected ID
    i.ind <- spatial_pts_x_y[spatial_pts_x_y$name==i.current_ind,]
    
    # keep just the X and Y coordinates of the point geometries (remove ID)
    i.x_y <-dplyr::select(i.ind, -name)
    
    # use the calc_sde function on x and Y points
    # https://www.rdocumentation.org/packages/aspace/versions/3.2/topics/calc_sde
    calc_sde(id=i,points=i.x_y)
    
    # save the SDE coordinates as a sf df
    i.calc_sde <- st_as_sf(sdeloc, coords= c("x","y"), crs=4326)
    
    # convert sde coordinates to polygon
    i.sde_poly = i.calc_sde %>% 
      group_by(id) %>% 
      summarise() %>%
      st_cast("POLYGON") %>% 
      st_convex_hull()  
    
    # save the geometry of the sde ellipse to the geometry column
    sde.df[i,"value.geometry"] <- i.sde_poly[2]
    
    
  }
  # create Sf object from sde ellipse dataframe 
  ind_sde <-st_sf(sde.df, crs=4326)
  return(ind_sde)
}

#### function that returns the buffer of a line neworks (spatial lines df) ####
get_network_buffer <- function(spatial_lines_df,buffersize_m){
  
  # get unique individuals (from points sf df)  
  unique_inds <- unique(spatial_lines_df$name)
  
  # get length of unqiue individuals (number of unique students) 
  n.values <- length(unique_inds) 
  
  # create blank dataframe with unique student IDs listed and blank geometry column I.e. 
  
  buffer.df <- data.frame(name  = unique_inds, value.geometry = NA)
  
  #transform to NZGD2000 / New Zealand Transverse Mercator 2000 -- New Zealand Transverse Mercator (NZTM)
  # To ensure accurately measured buffer in meters 
  ind_lines_tranf <- st_as_sf(spatial_lines_df) %>% st_transform(2193)
  
  
  # BUFFER FOR LOOP
  # loops through line geometry of all individuals 
  for (i in 1:n.values){
    
    # get value in row i of the "i" column
    # get ID of individual in row i of the blank df
    i.current_ind <- as.character(buffer.df[i,"name"])
    
    # get the line geometry of the individual with selected ID
    i.ind <- ind_lines_tranf[ind_lines_tranf$name==i.current_ind,]
    
    # use buffer method to calcualte buffer (dist = 10m )
    # note: may want to change buffer size for students (50m- )
    i.ind_lines_buff <- st_buffer(i.ind, buffersize_m)
    
    # added 28/05
    # get union of buffer
    i.ind_lines_buff <- st_union(i.ind_lines_buff)
    
    # transform crs back for plotting
    i.ind_buff <- st_as_sf(i.ind_lines_buff) %>% st_transform(4326)
    
    # save the geometry of the buffer to the geometry column
    buffer.df[i,"value.geometry"] <- i.ind_buff[1] #by getting union of buffer desired column changes to 1 from 2
    #i.ind_buff[2]
    
  }
  # create Sf object from buffer dataframe 
  ind_buffer <-st_sf(buffer.df, crs=4326)
  return(ind_buffer)
  
}

#### function that returns the buffer of points in a spatial points df ####
get_point_buffer <- function(sdf_pts, buffersize_m) {
  points_transformed  <- st_as_sf(sdf_pts) %>% st_transform(2193)
  buffer_sdf <-st_buffer(points_transformed, buffersize_m)%>% st_transform(4326)
}


# #### functionn returning points where the individual has spent x ammount of time, factoring in GPS error ####
# # get_key_locations <- function(spatial points df, spatia df points$timedifference column, time in seconds, GPS error in meters)
get_key_locations <- function(spatial_points_df, spatial_points_column, time_seconds, GPS_error,crs, coords, nodes){

  key_locations <- spatial_points_df[spatial_points_column >=as.numeric(time_seconds),]

  #transform CRS to accurtely measure in m
  #key_locations_tranf <- st_as_sf(key_locations) %>% st_transform(2193)
  key_locations_tranf <- st_as_sf(key_locations) %>% st_transform(crs)
  namelist <- as.list(unique(key_locations_tranf$name))
  namelist
  # create a blank df to fill
  centroids = data.frame(x= NA, name = NA)
  # centroids = data.frame(geometry= NA, name = NA)
  for (i in namelist){
    temp.sf <- subset(key_locations_tranf, name == i)

    # add 50m buffer to points
    key_locations_buff.temp <-st_buffer(temp.sf, GPS_error)%>% st_transform(4326)

    # take the union of the buffered points
    buff_union <- st_union(key_locations_buff.temp)
    buff_union <- st_as_sf(buff_union)

    #buff_union <- st_cast(buff_union, "MULTIPOLYGONS")
    buff_union_poyls <- st_cast(buff_union, "GEOMETRYCOLLECTION") %>% st_collection_extract("POLYGON")

    buff_union_poyls <-st_cast(buff_union_poyls , "POLYGON")

    centroids.temp <- st_centroid(buff_union_poyls)
    # centroids["geometry"] <- centroids.temp
    # # centroids[i, "name"] <- i

    temp.df <-data.frame(centroids.temp)
    # temp.df <- temp.df[c("clean_geometry","name")]
    temp.df$name <- i
    centroids <- rbind(centroids, temp.df)
    centroids <- drop_na(centroids)
    centroids.sf <-st_sf(centroids, crs=4326)

    # get n rows
    n.values <- nrow(centroids.sf)-1

    # get the nearst network node point for each centroid point
    # the nearest network ndoe will be the new 'cleaned' GPX point
    for (j in c(1:n.values)){

      # get coordinate of point in row
      coords_o.i <- centroids.sf[j,]$x %>%
        st_coordinates() %>%
        matrix(ncol = 2)

      # # get coordainte of next point (points chronological)
      coords_d.i <-centroids.sf[(j+1),]$x %>%
        st_coordinates() %>%
        matrix(ncol = 2)

      # # Find K nearest neighbours, where k =1 i.e. nearest neichbour only
      index_o.i <- nabor::knn(data = coords, query = coords_o.i, k = 1)
      index_d.i <- nabor::knn(data = coords, query = coords_d.i, k = 1)

      # save the cleaned point coordiantes and corresponding network node Id as new sf columns
      centroids.sf[j,"origin_geometry"] <-   nodes[index_o.i$nn.idx,2 ]
      centroids.sf[j,"origin_node_id"] <-  as.numeric(nodes[index_o.i$nn.idx,]$nodeID)
      # also save the (cleaned) coordaintes of the next point (the desitnaiton) and the destination node ID as new sf columns
      centroids.sf[j,"destination_clean"] <-  nodes[index_d.i$nn.idx, 2]
      centroids.sf[j,"destination_node_id"] <-  as.numeric(nodes[index_d.i$nn.idx, ]$nodeID)

    }
    #

  }
  #centroids.sf %>%
   # rename(
   #   geometry = x)
  centroids.sf <- drop_na(centroids.sf)
  # remove geometry column and make cleaned origin coordinates new geometry column
  centroids.sf <- st_drop_geometry(centroids.sf)
  centroids.sf <- st_as_sf(centroids.sf, crs=4326)

  centroids.sf$geometry <-centroids.sf$origin_geometry

  return(centroids.sf)


}

#### functionn returning points where the individual has spent x ammount of time, factoring in GPS error ####
# get_key_locations <- function(spatial points df, spatia df points$timedifference column, time in seconds, GPS error in meters)
get_key_locations <- function(spatial_points_df, spatial_points_column, time_seconds, GPS_error,crs, coords, nodes){

  key_locations <- spatial_points_df[spatial_points_column >=as.numeric(time_seconds),]

  #transform CRS to accurtely measure in m
  #key_locations_tranf <- st_as_sf(key_locations) %>% st_transform(2193)
  key_locations_tranf <- st_as_sf(key_locations) %>% st_transform(crs)
  namelist <- as.list(unique(key_locations_tranf$name))
  namelist
  # create a blank df to fill
  centroids = data.frame(x= NA, name = NA)
  # centroids = data.frame(geometry= NA, name = NA)
  for (i in namelist){
    temp.sf <- subset(key_locations_tranf, name == i)

    # add 50m buffer to points
    key_locations_buff.temp <-st_buffer(temp.sf, GPS_error)%>% st_transform(4326)

    # take the union of the buffered points
    buff_union <- st_union(key_locations_buff.temp)
    buff_union <- st_as_sf(buff_union)

    #buff_union <- st_cast(buff_union, "MULTIPOLYGONS")
    buff_union_poyls <- st_cast(buff_union, "GEOMETRYCOLLECTION") %>% st_collection_extract("POLYGON")

    buff_union_poyls <-st_cast(buff_union_poyls , "POLYGON")

    centroids.temp <- st_centroid(buff_union_poyls)
    # centroids["geometry"] <- centroids.temp
    # # centroids[i, "name"] <- i

    temp.df <-data.frame(centroids.temp)
    # temp.df <- temp.df[c("clean_geometry","name")]
    temp.df$name <- i
    centroids <- rbind(centroids, temp.df)
    centroids <- drop_na(centroids)
    centroids.sf <-st_sf(centroids, crs=4326)

    # get n rows
    n.values <- nrow(centroids.sf)-1

    # get the nearst network node point for each centroid point
    # the nearest network ndoe will be the new 'cleaned' GPX point
    for (j in c(1:n.values)){

      # get coordinate of point in row
      coords_o.i <- centroids.sf[j,]$x %>%
        st_coordinates() %>%
        matrix(ncol = 2)

      # # get coordainte of next point (points chronological)
      coords_d.i <-centroids.sf[(j+1),]$x %>%
        st_coordinates() %>%
        matrix(ncol = 2)

      # # Find K nearest neighbours, where k =1 i.e. nearest neichbour only
      index_o.i <- nabor::knn(data = coords, query = coords_o.i, k = 1)
      index_d.i <- nabor::knn(data = coords, query = coords_d.i, k = 1)

      # save the cleaned point coordiantes and corresponding network node Id as new sf columns
      centroids.sf[j,"origin_geometry"] <-   nodes[index_o.i$nn.idx,2 ]
      centroids.sf[j,"origin_node_id"] <-  as.numeric(nodes[index_o.i$nn.idx,]$nodeID)
      # also save the (cleaned) coordaintes of the next point (the desitnaiton) and the destination node ID as new sf columns
      centroids.sf[j,"destination_clean"] <-  nodes[index_d.i$nn.idx, 2]
      centroids.sf[j,"destination_node_id"] <-  as.numeric(nodes[index_d.i$nn.idx, ]$nodeID)

    }
    #

  }
  #centroids.sf %>%
  # rename(
  #   geometry = x)
  centroids.sf <- drop_na(centroids.sf)
  # remove geometry column and make cleaned origin coordinates new geometry column
  centroids.sf <- st_drop_geometry(centroids.sf)
  centroids.sf <- st_as_sf(centroids.sf, crs=4326)

  centroids.sf$geometry <-centroids.sf$origin_geometry

  return(centroids.sf)


}
home_location <- function(spatial_points_df){

  # get first and last points in df
  am_first<- spatial_points_df %>% group_by(name) %>% slice(1)
  pm_last <- spatial_points_df %>% group_by(name) %>% slice(n())

  # define function to get mode
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  # if the most common first node recorded across all days of activity
  # is the same as the most common last node recorded across all days of activity
  # this is taken to be the home location
  # else the home location is the most common node
  home <- data.frame(origin_node_id = NA, origin_clean =NA)

  if (Mode(am_first$origin_node_id)!=Mode(pm_last$destination_node_id)) {
    home <-pm_last[1,c("origin_node_id","origin_clean" )]
  } else {
    most_common_node <-Mode(spatial_points_df$origin_node_id)
    home$origin_clean <- spatial_points_df[spatial_points_df$origin_node_id == as.numeric(most_common_node),]$origin_clean[1]
    home$origin_node_id <- spatial_points_df[spatial_points_df$origin_node_id == as.numeric(most_common_node),]$origin_node_id[1]
  }

  home <-st_as_sf(data.frame(home),crs=4326)
  return(home)
}


# home_location <- function(spatial_points_df){  
#   # get first and last points in df
#   am_first<- spatial_points_df %>% group_by(name) %>% slice(1)
#   pm_last <- spatial_points_df %>% group_by(name) %>% slice(n())
#   
#   # define function to get mode
#   Mode <- function(x) {
#     ux <- unique(x)
#     ux[which.max(tabulate(match(x, ux)))]
#   }
#   
#   # if the most common first node recorded across all days of activity 
#   # is the same as the most common last node recorded across all days of activity
#   # this is taken to be the home location
#   # else the home location is the most common node 
#   home <- data.frame(orgn_n_ = NA)
#   
#   if (Mode(am_first$orgn_n_)!=Mode(pm_last$dstnt__)) {
#     home <-pm_last[1,c("orgn_n_")]
#   } else {
#     most_common_node <-Mode(spatial_points_df$orgn_n_)
#     # home$origin_clean <- spatial_points_df[spatial_points_df$orgn_n_ == as.numeric(most_common_node),]$origin_clean[1]
#     home<-spatial_points_df[spatial_points_df$orgn_n_ == as.numeric(most_common_node),][1,c('orgn_n_','geometry')]
#   }
#   home <-st_as_sf(data.frame(home),crs=4326)
#   return(home)
# }
# 

workplace <- function(long1,long2, lat1,lat2,nodes, coords){
  
  uni_pts <- st_sfc(st_point(c(long1,lat1)), st_point(c(long2,lat1)), st_point(c(long2, lat2)),st_point(c(long1,lat2)))
  uni_poly <-st_as_sfc(st_bbox(uni_pts), crs=4326)
  
  uni_point <- st_centroid(uni_poly)
  uni_point <-st_as_sf(uni_point, crs=4326)
  # get coordinate of point in row
  coords_o.i <- uni_point$x %>%
    st_coordinates() %>%
    matrix(ncol = 2)
  
  # Find K nearest neighbours, where k =1 i.e. nearest neichbour only
  index_o.i <- nabor::knn(data = coords, query = coords_o.i, k = 1)
  
  # save the cleaned point coordiantes and corresponding network node Id as new sf columns
  uni_point$geometry <-   nodes[index_o.i$nn.idx,]$geometry
  uni_point$origin_node_id <-  nodes[index_o.i$nn.idx,]$nodeID
  uni_point <- st_drop_geometry(uni_point)
  uni_point <- uni_point[c('origin_node_id','geometry')]
  uni <- list(uni_area=uni_poly, uni_pt =uni_point)
  return(uni)
}


get_exposure<-function(activity_space_measure, exposure_measure, method="method"){
  #new_df_name <- data.frame(matrix(ncol = (ncol(exposure_measure) +ncol(activity_space_measure)), nrow = 3))
  #colnames(new_df_name) <- c("name",colnames(exposure_measure),"value.geometry")
  #n.values <- nrow(activity_space_measure)
  #for (i in 1:n.values){
    #temp <- st_intersection(activity_space_measure[i,],exposure_measure)
    #temp_mean <- 
    #  temp %>%
    #  group_by(name) %>% 
    #  summarise_all("mean")
    #df <-temp_mean
    #new_df_name <- dplyr::bind_rows(new_df_name,df)
    # remove any rows with missing data 
    #new_df_name<-new_df_name[!is.na(new_df_name$name),]
  #}
  new_df_name <- data.frame(matrix(ncol = (ncol(exposure_measure) +ncol(activity_space_measure)-1), nrow = 3))
  colnames(new_df_name) <- c("name",colnames(st_drop_geometry(exposure_measure)),"value.geometry")
  n.values <- nrow(activity_space_measure)
  for (i in 1:n.values){
    temp <- st_intersection(activity_space_measure[i,],exposure_measure)
    temp_mean <- 
      temp %>%
      group_by(name) %>% 
      summarise_all("mean")
    df <-temp_mean
    new_df_name <- rbind(new_df_name,df)
    # remove any rows with missing data 
    new_df_name<-new_df_name[!is.na(new_df_name$name),]
  }
  new_df_name["method"] <- method
  new_df_name <- st_as_sf(new_df_name, crs=4326)
  return(new_df_name)
}  
