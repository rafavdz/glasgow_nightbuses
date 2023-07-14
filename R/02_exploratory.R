

library(tidyverse)
library(sf)
library(ggrepel)


# Read data
# Read TTM
ttm <- read_csv('output/compare_ttm.csv')
# Read geometries
geometries <- st_read('data/infuse_lsoa_lyr_2011_clipped/infuse_lsoa_lyr_2011_clipped.shp')
# Read centroids
centroids <- st_read('data/SG_DataZoneCent_2011/SG_DataZone_Cent_2011.shp')
# Road network 
roadnetwork <- st_read('data/osm/scotland-latest.osm.pbf', layer = 'lines')

# Overall summary ---------------------------------------------------------


# Create summary by timecuts
ttm_summary <- ttm %>% 
  as.data.frame() %>% 
  group_by(from_id, router) %>% 
  summarise(
    destinations_30 = sum(ifelse(travel_time_p25 <= 30, dest_hh_cnt2011, 0)),
    destinations_45 = sum(ifelse(travel_time_p25 <= 45, dest_hh_cnt2011, 0)),
    destinations_60 = sum(ifelse(travel_time_p25 <= 60, dest_hh_cnt2011, 0)),
    destinations_90 = sum(ifelse(travel_time_p25 <= 90, dest_hh_cnt2011, 0)),
  )

# Restructure summary
ttm_summary <- ttm_summary %>% 
  pivot_longer(c(-from_id, -router), names_to = 'timecut') %>% 
  pivot_wider(names_from = router, values_from = value)

# Compute difference
ttm_summary <- ttm_summary %>% 
  mutate(
    difference_n = after - before, 
    difference_pct = difference_n / before * 100
  )

# Organise travel time data -----------------------------------------------

# Geoms only
geometries <- geometries %>% select(geo_code)
# LSOA names
lsoa_names <- centroids %>% 
  st_drop_geometry() %>% 
  select(DataZone, Name) %>% 
  janitor::clean_names()

# Join census data and geoms to ttm
ttm <- ttm %>%
  # Add destination data
  left_join(st_drop_geometry(centroids), by = c('to_id' = 'DataZone')) %>% 
  janitor::clean_names() %>% 
  rename_with(~ paste0('dest_', .x), name:northing) %>% 
  # Add origin name
  left_join(lsoa_names, by = c('from_id' = 'data_zone')) %>%
  rename(from_name = name) %>% 
  # Add geoms
  left_join(geometries, by = c('to_id' = 'geo_code')) %>%
  st_as_sf()

# Total households in area
total_hh <- ttm %>% 
  as.data.frame() %>% 
  select(to_id, dest_hh_cnt2011) %>% 
  distinct() %>% 
  summarise(total_hh_glasgow = sum(dest_hh_cnt2011))
# Access as percent of households
ttm_summary <- ttm_summary %>% 
  mutate(percetn_hh_60 = destinations_60 / total_hh$total_hh_glasgow)

# Isochrone map -----------------------------------------------------------

# Plot directory 
dir.create('plots')

# Selected origin to plot
selected_origns <- c('S01010275')

# Filter roadnetwork
roadnetwork <- roadnetwork %>% 
  filter(!highway %in% c('cycleway', 'footway', 'pedestrian')) %>% 
  st_transform(27700)

# Select percent of households for this area
pct_hh <- ttm_summary %>%
  filter(from_id == selected_origns)

# Map grid labels (header)
map_labels <- 
  c('Without First night services', 'With First night services')
map_labels <- paste(
  map_labels, '\n', 
  'Number of households reachable within 60 min:', 
  formatC(pct_hh$destinations_60, format="f", big.mark=",", digits=0),
  paste0('(', round(pct_hh$percetn_hh_60 *100, 0), '%)')
)
map_labels <- map_labels[c(2,1)]
# General labels
plot_tile <- 'Before and After the Last Stop: \nMapping Travel Time Changes in Glasgow Following the Planned Night Bus Service Cuts of July 2023.\n'
plot_subtitle <- 'Estimates departing on Saturday at midnight.\n'
plot_caption <- 
  c(
    '\n\nMap and estimates developed at the Urban Big Data Centre (UBDC) by Rafael Verduzco (Twitter: @raffverduzco)',
    'Source: Based on Traveline TNDS; Rail Delivery Group; 2011 Census.'
    )
plot_caption <- paste0(plot_caption, collapse = '\n')

# Annotation
city_centre <- centroids %>% 
  filter(DataZone == selected_origns[1]) %>% 
  mutate(Name = 'City centre')

# Subset geometries to map
tt_geoms <- ttm %>%
  filter(travel_time_p25 <= 60) %>% 
  filter(from_id %in% selected_origns[1]) %>% 
  mutate(
    router = factor(router, levels = c('before', 'after'), labels = map_labels)
  )

# Define map area
ttm_box <- st_bbox(tt_geoms)
# Crop roadnetwork
roads_clipped <- st_crop(roadnetwork, ttm_box)

# Map
city_centremap <-  tt_geoms%>% 
  ggplot() +
  geom_sf(data = roads_clipped, linewidth = 0.1, col = 'gray70') +
  geom_sf(aes(fill = travel_time_p25), col = NA, alpha = 0.7) +
  facet_wrap(~ router) +
  geom_sf(data = city_centre, size = 0.7) +
  geom_label_repel(
    data = city_centre,
    aes(Easting, Northing, label = Name),
    nudge_x = 5e3, nudge_y = 3e3, size = 2.3, 
  ) +
  labs(
    title = plot_tile,
    subtitle = plot_subtitle,
    fill = 'Travel time \n(minutes)',
    caption = plot_caption
  ) +
  scale_fill_viridis_b(direction = -1, breaks = seq(0, 90, by = 10)) +
  theme_void() +
  theme(
    legend.position = 'bottom', 
    legend.key.width = unit(1, 'cm'), 
    legend.key.height = unit(0.4, 'cm'), 
    axis.text = element_blank()
  )


ggsave(
  'plots/ischrone_centre.png', 
  city_centremap,
  width = 9, height = 6,
  dpi = 400,
  bg = 'white'
)


