# Purpose: Generate the distance between the centroids of all Italian provinces
# Inputs: NUTS3 Shapes: dir.gadm1_nuts3_counties_shapes
# Outputs: _output/it_distance_table.csv
# Date: 12/01/2020

library(tidyverse)
library(sp)
library(sf)

# Read in the detailed GADM shapes
shapes_it <- readRDS(dir.gadm1_nuts3_counties_shapes) %>% 
  filter(substr(key, 1, 2) == "IT") %>% 
  filter(substr(key, 1, 2) == "IT")

# Get the NUTS3 centroids
# See ?coordinates() for more info on centroid calculation
shapes_it <- as_Spatial(shapes_it) # Function is not available in sf, so we convert back to SpatialPolygonsDataFrame
centroids <- tibble( 
  nuts3 = shapes_it$key,
  centroid_x = coordinates(shapes_it)[,1],
  centroid_y = coordinates(shapes_it)[,2])

# Trasnform the centroids into a geometry set
all_centroids <- 
  centroids[,2:3] %>% 
  as.data.frame %>% 
  st_as_sf(coords = c(1,2)) %>% 
  st_geometry()

# Add the CRS type
st_crs(all_centroids) <- shapes_it@proj4string

# Now calculate all the distances (takes a few moments to run)
all_distances <- st_distance(all_centroids, all_centroids)

# Convert the matrix to table and meters to KM
it_dist_table <- all_distances %>% 
  as_tibble() %>%
  rename_all(function(x) shapes_it$key) %>% # rename the rows 
  mutate(from_nuts3 = shapes_it$key) %>%
  gather(key=to_nuts3, value=dist, -from_nuts3) %>% 
  mutate(dist = as.numeric(dist)) %>% 
  mutate(dist = dist/1000) %>% # convert to KM
  arrange(to_nuts3, from_nuts3)

write_csv(it_dist_table, "../_intermediate/it_distance_table.csv")

