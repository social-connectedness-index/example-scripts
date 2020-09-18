# install.packages("tidyverse")
# install.paackages("sf")
# install.packages("tigris")
# install.packages("raster")
library(tidyverse)
library(sf)
library(tigris)
library(raster)

### FILL THIS LINE BEFORE RUNNING
dir.sci_dat_county <- ""

# Read in the county-county SCI data
sci_dat <- read_tsv(dir.sci_dat_county)
sci_dat <- rename(sci_dat, sci=scaled_sci)

# Get the maps from the tigris package
counties_map <- counties(cb = TRUE) %>% 
  st_as_sf() %>% 
  st_transform(crs("+init=epsg:2163"))

states_map <- states(cb = TRUE) %>% 
  st_as_sf() %>% 
  st_transform(crs("+init=epsg:2163"))

counties_map <- counties_map %>% mutate(fips = paste0(STATEFP, COUNTYFP))

# Make a vector of regions to generate maps for
regions <- c("06075", # San Francisco County, USA
             "06029", # Kern County, USA
             "17031", # Cook County, USA
             "11001") # Washington, DC

# Create measures to scale up from the overall 20th percentile location pair
x1 <- quantile(sci_dat$sci, .2)
x2 <- x1 * 2
x3 <- x1 * 3
x5 <- x1 * 5
x10 <- x1 * 10
x25 <- x1 * 25
x100 <- x1 * 100

# Create the graph for each of the regions in the list of regions
for(i in 1:length(regions)){
  
  # Get the data for the ith region
  dat <- filter(sci_dat, user_loc == regions[i])
  
  # Merge with shape files
  dat_map <- 
    right_join(dat,
               counties_map,
               by=c("fr_loc"="fips")) %>% 
    st_as_sf
  
  # Create clean buckets for these levels
  dat_map <- dat_map %>% 
    mutate(sci_bkt = case_when(
      sci < x1 ~ "< 1x (Overall 20th percentile)",
      sci < x2 ~ "1-2x",
      sci < x3 ~ "2-3x",
      sci < x5 ~ "3-5x",
      sci < x10 ~ "5-10x",
      sci < x25 ~ "10-25x",
      sci < x100 ~ "25-100x",
      sci >= x100 ~ ">= 100x")) %>% 
    mutate(sci_bkt = factor(sci_bkt, levels=c("< 1x (Overall 20th percentile)", "1-2x", "2-3x", "3-5x",
                                              "5-10x", "10-25x", "25-100x", ">= 100x")))
  
  # Get the map of the region you are in
  curr_region_outline <- dat_map %>% 
    filter(fr_loc == regions[i])
  
  # Plot the data
  ggplot(dat_map) +
    geom_sf(aes(fill = sci_bkt), colour="#ADADAD", lwd=0) +
    geom_sf(data=curr_region_outline, fill="#A00000", colour="#A00000", size=0.4) +
    geom_sf(data=states_map, fill="transparent", colour="#A1A1A1", size=0.2) +
    labs(fill = "SCI") +
    theme_void() +
    scale_fill_brewer(palette = "GnBu", na.value="#F5F5F5", drop=FALSE) +
    theme(legend.title = element_blank(), 
          legend.text  = element_text(size = 8),
          legend.key.size = unit(0.8, "lines"),
          legend.position = "bottom", legend.box = "horizontal") +
    guides(fill = guide_legend(nrow = 1, title.hjust = 0.5)) +
    coord_sf(xlim = c(-2200000, 2700000), ylim = c(-2200000, 850000), expand = FALSE) 
  
  # Save output to the folder "output/county_output"
  ggsave(paste0("output/sci_", regions[i], ".jpg"),
         width = 6.5, height = 3.8, units = "in", dpi = 800, last_plot())
  
}