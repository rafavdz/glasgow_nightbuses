# Impact night buses cuts


# Packages

# Allocate RAM to Java 
options(java.parameters = "-Xmx100G")
# Load R5R, uses V 1.0.1!
library(r5r)

library(sf)
library(tidyverse)
library(gtfstools)


# Read data ---------------------------------------------------------------


# Read Scotland GTFS
orignal_gtfs_path <- 'data/traveline/20230307/gtfs/S/S.gtfs.zip'
gtfs_original <- read_gtfs(orignal_gtfs_path)

# Identify night services
routes_night <- gtfs_original$routes %>% 
  filter(grepl("^N", route_short_name) & agency_id == 'FirtGlaw') %>% 
  pull(route_id)

# Examine night services
gtfs_night <- gtfs_original %>% 
  filter_by_route_id(route_id = routes_night, keep = TRUE)

gtfs_night$calendar %>% 
  View()

# Filter out night services
gtfs_reduced <- gtfs_original %>% 
  filter_by_route_id(route_id = routes_night, keep = FALSE)

# Write reduced GTFS version
reduced_gtfs_path <- 'data/traveline/20230307/gtfs/S_reduced.gtfs.zip'
write_gtfs(gtfs_reduced, reduced_gtfs_path)


# Create router -----------------------------------------------------------

# Original router
original_path <- 'router/original'
# Reduced router
reduced_path <- 'router/reduced'

# Create folders
dir.create(original_path, recursive = TRUE)
dir.create(reduced_path, recursive = TRUE)

# Supporting files
osm <- 'data/osm/scotland-latest.osm.pbf'
atoc <- 'data/atoc/20230304/ttis662.zip'

# Copy original files
c(osm, atoc, orignal_gtfs_path) %>% 
  lapply(file.copy, to = original_path)
# Copy reduced files
c(osm, atoc, reduced_gtfs_path) %>% 
  lapply(file.copy, to = reduced_path)


# Build network  ------------------------------------------------------------

# Build orginal network
original_core <- setup_r5(data_path = original_path, verbose = TRUE)
# Reduced network
reduced_core <- setup_r5(data_path = reduced_path, verbose = TRUE)

# Prepare OD points -------------------------------------------------------

# Read centroids
centroids <- st_read('data/SG_DataZoneCent_2011/SG_DataZone_Cent_2011.shp')
# Read TTWA
ttwa <- read_sf('data/ons/ttwa2011_pop.gpkg')

# Subset OD points
glasgow_area <- ttwa %>% 
  filter(ttwa11nm == "Glasgow")
glasgow_extended <- st_buffer(glasgow_area, 5e3)

# Get origins
origins <- centroids[glasgow_area,]
# Get destinations
destinations <- centroids[glasgow_extended,]

# Format OD points
od_points <- list(origins = origins, destinations = destinations) %>% 
  map(st_transform, crs = 4326) %>% 
  map(rename, id = DataZone)


# Routing parameters ------------------------------------------------------

# Routing parameters
mode <- c('WALK', 'TRANSIT')
dep_time <- as.POSIXct("2023-03-12 00:00:00")
max_duration <- 120
mx_rides <- 3
time_window <- 60
max_walk <- 20
percentiles <- c(25, 50, 75)


# Estimate routes ---------------------------------------------------------

ttm <-
  list(before = original_core, after = reduced_core) %>% 
  map(
    ~travel_time_matrix(
      r5r_core = .x, 
      origins = od_points$origins, 
      destinations = od_points$destinations, 
      mode = mode, 
      departure_datetime = dep_time,
      time_window =  time_window,
      percentiles = percentiles, 
      max_walk_time = max_walk, 
      max_rides = mx_rides, 
      verbose = TRUE
    )
  )

ttm <- ttm %>% 
  bind_rows(.id = "router")

# Write ttm
dir.create('output')
write_csv(ttm, 'output/compare_ttm.csv')

