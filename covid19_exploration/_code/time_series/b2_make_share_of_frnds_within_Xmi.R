# Purpose: For each county, use SCI data to estimate the share of each county's friends within 50/150 miles
# Inputs: 
#     US COVID Cases/Deaths time series data from: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/b6bf3226c922663a7aea4d2d7305edbae5d3fd14/csse_covid_19_data/csse_covid_19_time_series/
#     SCI Data: dir.sci_dat_county
#     _input/sf12010countydistancemiles.csv
# Outputs: 
#     _intermediate/share_within_x_miles.csv
# Date: 12/01/2020
# Steps:
#     1. Estimate share within X miles

library(tidyverse)

########################################
### 1. Estimate share within X miles ###
########################################

# Read in SCI data
sci_dat <- read_tsv(dir.sci_dat_county)
sci_dat <- rename(sci_dat, sci=scaled_sci) %>%
  mutate(user_loc=as.numeric(user_loc),
         fr_loc=as.numeric(fr_loc)) %>% 
  filter(user_loc <= 57000 & fr_loc <= 57000) %>%  # removes territories
  mutate(user_loc=str_pad(as.character(user_loc), 5, "left", "0"),
         fr_loc=str_pad(as.character(fr_loc), 5, "left", "0"))
  
# Read in population data
county_pops <- read_csv("../_input/ACS_17_5YR_DP05.csv") %>% 
  select(fips = GEO.id2, pop=HC01_VC03) %>% 
  mutate(fips = str_pad(as.character(fips), 5, "left", "0")) %>% 
  ungroup

# Read in county-county distance
county_county_dist <- read_csv("../_input/sf12010countydistancemiles.csv") 
  
# Calculate the estimated share of connections for each county in each other county.
# The key underlying assumption (FB penetration is roughly equal between counties) is reasonable
connections_dat <- sci_dat %>% 
  # Merge in distances
  left_join(county_county_dist, by=c("user_loc"="county1", "fr_loc"="county2")) %>% 
  # This filters places which are included in the NBER distance-distnace
  filter(
    user_loc %in% unique(county_county_dist$county1),
    fr_loc %in% unique(county_county_dist$county1)) %>% 
  # This data doesn't include self-loops. Add them here.
  mutate(mi_to_county = if_else(user_loc == fr_loc, 0, mi_to_county)) %>% 
  # Merge in pops
  inner_join(county_pops, by=c("user_loc"="fips")) %>% 
  rename(user_pop=pop) %>% 
  inner_join(county_pops, by=c("fr_loc"="fips")) %>% 
  rename(fr_pop=pop) %>% 
  group_by(user_loc) %>% 
  mutate(share_user_connections = (sci*fr_pop)/sum(sci*fr_pop)) %>% 
  ungroup

# Create final measures
final_within_dat <- connections_dat %>% 
  group_by(user_loc) %>% 
  summarise(
    share_within50 = sum(share_user_connections*(mi_to_county <= 50)),
    share_within150 = sum(share_user_connections*(mi_to_county <= 150))) %>% 
  ungroup

# Write output
write_csv(final_within_dat, "../_intermediate/share_within_x_miles.csv")
