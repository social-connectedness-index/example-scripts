# install.packages("tidyverse")
# install.paackages("sf")
library(tidyverse)
library(sf)

# This function will plot within-country SCI for a specified country. Maps are saved in the directory
# specified by output_folder.

# We provide two example calls below.

# Inputs:
    # sci_dat = A tibble with SCI data. Must include columns user_loc, fr_loc, SCI, and additional columns that specify countries
    # user_country_col = A string that stores the name of the column that specifies the user_loc country (defaults to 'user_country')
    # fr_country_col = A string that stores name of the column that specifies the fr_loc country (defaults to 'fr_country')
    # shapefiles = An sf object that specifies the shapefiles to use
    # region_name_col = A string that stores the name of the column in 'shapefiles' that specifies the region name
    # country = The country code to filter to (matching the format of the data stored in user_country_col)
    # regions = A vector of regions within the country to generate maps for (if NULL will generate maps for every region)
    # output_folder = The folder to save the maps to
    # scale_from_ptile = The maps color buckets are created from scaling up from the Xth percentile of any region pair. This sets that X (defaults to 25).

# Outputs:
  # A set of maps for every region specified by regions (or every region within the entire country, if regions
  # is not specified). Maps are saved as: "{output_folder}/SCI_{region_name}.jpg"

plot_sci <- function(sci_dat, user_country_col="user_country", fr_country_col="fr_country",
                     shapefiles, region_name_col="name",
                     country, regions=NULL, output_folder,
                     scale_from_ptile=25){
  
  sym_user_country_col <- sym(user_country_col)
  sym_fr_country_col <- sym(fr_country_col)
  
  # Filter the SCI data to the country of choice
  country_sci <- sci_dat %>% 
    filter(!!sym_user_country_col == country, !!sym_fr_country_col == country)
  
  # Create measures by scaling up from the chosen percentile of all pairs
  x1 <- quantile(country_sci$sci, scale_from_ptile/100)
  x2 <- x1 * 2
  x3 <- x1 * 3
  x5 <- x1 * 5
  x10 <- x1 * 10
  x25 <- x1 * 25
  x100 <- x1 * 100
  
  if(is.null(regions)){
    regions <- unique(country_sci$user_loc)
  }

  for(i in 1:length(regions)){
    
    # Get the data for the ith region
    curr_region_code <- regions[i]
    dat <- filter(country_sci, user_loc == curr_region_code)
    
    # Merge with shape files
    dat_map <- 
      inner_join(dat,
                 shapefiles,
                 by=c("fr_loc"="key")) %>% 
      st_as_sf
    
    # Create clean buckets for these levels
    dat_map <- dat_map %>% 
      mutate(sci_bkt = case_when(
        sci < x1 ~ str_interp("< 1x (Country ${scale_from_ptile}th percentile)"),
        sci < x2 ~ "1-2x",
        sci < x3 ~ "2-3x",
        sci < x5 ~ "3-5x",
        sci < x10 ~ "5-10x",
        sci < x25 ~ "10-25x",
        sci < x100 ~ "25-100x",
        sci >= x100 ~ ">= 100x")) %>% 
      mutate(sci_bkt = factor(sci_bkt, levels=c(str_interp("< 1x (Country ${scale_from_ptile}th percentile)"),
                                                "1-2x", "2-3x", "3-5x", "5-10x", "10-25x", "25-100x", ">= 100x")))
    
    # Get the map of the region you are in
    curr_region_outline <- dat_map %>% 
      filter(fr_loc == curr_region_code)
    
    curr_region_name <- curr_region_outline[[region_name_col]]
    
    # Plot the data
    ggplot(st_transform(dat_map, "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) +
      geom_sf(aes(fill = sci_bkt), colour="#DADADA", size=0.15) +
      geom_sf(data=curr_region_outline, fill="#A00000", colour="#A00000", size=0.5) +
      labs(fill = "SCI", title = str_interp("${curr_region_code} (${curr_region_name})")) +
      theme_void() +
      scale_fill_brewer(palette = "GnBu", na.value="#F5F5F5", drop=FALSE) +
      theme(legend.title = element_blank(), 
            legend.text  = element_text(size = 8),
            legend.key.size = unit(0.8, "lines"),
            legend.position = "bottom", legend.box = "horizontal") +
      guides(fill = guide_legend(nrow = 1, title.hjust = 0.5))
    
    ggsave(str_interp("${output_folder}/sci_${regions[i]}.jpg"),
           width = 12, height = 7, units = "in", last_plot())
    
  }
  
}

##### Example code for running this function #######

### FILL THIS LINE BEFORE RUNNING
dir.sci_dat_gadm1_nuts3_counties <- ""

# Read in the detailed GADM SCI data (this dataset is quite large and 
# this line of code will likely take a minute or so)
sci_dat <- read_tsv(dir.sci_dat_gadm1_nuts3_counties)
sci_dat <- rename(sci_dat, sci=scaled_sci)

# Read in the detailed GADM shapes
shapes_in <- readRDS("../../gadm_based_shapefiles/rds_format/gadm1_nuts3_counties.Rds")

# Add the country codes to the SCI data
country_codes <- shapes_in %>% 
  st_drop_geometry() %>% 
  select(key, country)

sci_dat <- sci_dat %>% 
  inner_join(country_codes, by=c("user_loc"="key")) %>% 
  rename(user_country = country) %>% 
  inner_join(country_codes, by=c("fr_loc"="key"))  %>% 
  rename(fr_country = country)

# Example call 1: Germany with select regions
shapes_simple_DEU <- shapes_in %>%
  filter(country == "DEU") %>% 
  st_simplify(preserveTopology=TRUE, dTolerance = .0025)

plot_sci(sci_dat=sci_dat, shapefiles=shapes_simple_DEU,
         country="DEU", regions=c("DED52", "DED21", "DE111", "DE212"),
         output_folder="output/within_DEU")

# Example call 2: Hungary with all regions, scaling from 5th percentile
shapes_simple_HUN <- shapes_in %>%
  filter(country == "HUN") %>% 
  st_simplify(preserveTopology=TRUE, dTolerance = .0025)

plot_sci(sci_dat=sci_dat, shapefiles=shapes_simple_HUN,
         country="HUN",
         output_folder="output/within_HUN", scale_from_ptile=5)