# Purpose: Make our two "weighted" measures, 
#           Social Proximity to Cases and Physical Proximity to Cases
# Inputs: 
#     _input/ACS_17_5YR_DP05.csv
#     US COVID time series data from: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/64689f437b15e336aca4b65f0240576f5a52c091/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv
#     SCI DATA: county_county.tsv
# Outputs: 
#     _intermediate/sci_weighted_cases.csv
#     _intermediate/dist_weighted_cases.csv
# Date: 07/30/2020
# Steps:
#     1. Prep COVID Data
#     2. Generate Social Proximity to Cases
#     3. Generate Physical Proximity to Cases

library(tidyverse)


##############################
##### 1. Prep COVID Data #####
##############################

# Read in county pops, pulled from Census
county_pops <- read_csv("../_input/ACS_17_5YR_DP05.csv") %>% 
  select(fips = GEO.id2, label=`GEO.display-label`, pop=HC01_VC03) %>% 
  mutate(fips = str_pad(as.character(fips), 5, "left", "0")) %>% 
  # Match the Hopkins data use of a single NYC
  mutate(fips = if_else(fips %in% c("36005", "36047", "36081", "36085"), "36061", fips)) %>% 
  group_by(fips) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup

# Read in the time series COVID data
covid_dat <- 
  read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/64689f437b15e336aca4b65f0240576f5a52c091/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>% 
  filter(!is.na(FIPS)) %>%
  filter(iso2 == "US") %>% # filters out territories
  filter(FIPS < 80000) %>% # filters out 'unassigned' cases within states
  mutate(FIPS = as.numeric(FIPS)) %>% 
  select(-c("UID", "iso2", "iso3", "code3", "Admin2", "Country_Region",
            "Lat", "Long_", "Combined_Key")) %>% 
  # Make the data long
  gather(date, Confirmed, -c("FIPS", "Province_State")) %>% 
  # Convert to R's date type
  mutate(date = parse_date(date, "%m/%d/%y")) %>% 
  # Make a State FIPS
  mutate(FIPS = str_pad(as.character(FIPS), 5, "left", "0")) %>% 
  mutate(state_FIPS = substr(FIPS, 1, 2)) %>% 
  # Add in the pop data to get cases per 10k
  left_join(county_pops, by=c("FIPS"="fips")) %>% 
  mutate(Confirmed_per_10k = (Confirmed/pop) * 10000) %>% 
  # Filter to Mondays
  filter(wday(date) == 2) %>% 
  # Get every other Monday
  group_by(FIPS) %>% 
  arrange(FIPS, date) %>% 
  mutate(row_num = row_number()) %>% 
  filter(row_num %% 2 == 0) %>% 
  select(-row_num) %>% 
  ungroup %>% 
  # In the paper, our results run only through July 20, 2020. One could
  # add more periods by removing this last restriction.
  filter(date <= as.Date("2020-07-20"))

# Get a list of states.
states <- unique(covid_dat$state_FIPS)


#################################################
##### 2. Generate Social Proximity to Cases #####
#################################################

# Read in SCI data
sci_dat <- read_tsv(dir.sci_dat_county)
sci_dat <- rename(sci_dat, sci=scaled_sci)

# Match the Hopkins data use of a single NYC
sci_dat <- sci_dat %>% 
  mutate(user_loc = if_else(user_loc %in% c("36005", "36047", "36081", "36085"), "36061", user_loc)) %>% 
  mutate(fr_loc = if_else(fr_loc %in% c("36005", "36047", "36081", "36085"), "36061", fr_loc)) %>% 
  group_by(user_loc, fr_loc) %>% 
  summarise(sci = mean(sci)) %>% 
  ungroup %>% 
  filter(user_loc <= 57000 & fr_loc <= 57000) # removes territories

# Get the share of total SCI from each county
sci_dat <- sci_dat %>%
  group_by(user_loc) %>%
  mutate(total_sci = sum(sci)) %>%
  mutate(share_sci = sci/total_sci) %>%
  ungroup

final_dat <- NULL

# We will loop through user states when we make our weighted
# measures to avoid OOM'ing on a large join.
for(curr_state in states) {
  
  print(paste("Creating Social Proximity to Cases for Counties in State FIPS:", curr_state))
  
  curr_dat <- sci_dat %>% 
    filter(substr(user_loc, 1, 2) == curr_state) %>% 
    # Join in the COVID data
    inner_join(covid_dat, by=c("fr_loc"="FIPS")) %>% 
    # Collapse and make the final weighted measure
    group_by(user_loc, date) %>% 
    summarise(sci_weighted_cases = sum(Confirmed*share_sci),
              sci_weighted_cases_per_10k = sum(Confirmed_per_10k*share_sci)) %>% 
    ungroup
  
  if(is.null(final_dat)){
    final_dat <- curr_dat
  }
  else{
    final_dat <- bind_rows(final_dat, curr_dat)
  }
}

write_csv(final_dat, "../_intermediate/sci_weighted_cases.csv")


###################################################
##### 3. Generate Physical Proximity to Cases #####
###################################################

# Read in county-county distance from NBER
# https://data.nber.org/data/county-distance-database.html
county_county_dist <- read_csv("../_input/sf12010countydistancemiles.csv")

# Match the Hopkins data use of a single NYC
county_county_dist <- county_county_dist %>% 
  mutate(county1 = if_else(county1 %in% c("36005", "36047", "36081", "36085"), "36061", county1)) %>% 
  mutate(county2 = if_else(county2 %in% c("36005", "36047", "36081", "36085"), "36061", county2)) %>% 
  group_by(county1, county2) %>% 
  summarise(mi_to_county = mean(mi_to_county)) %>% 
  ungroup


final_dat <- NULL

# Again, we loop through user states when we make our weighted
# measures to avoid OOM'ing on a large join.
for(curr_state in states) {
  
  print(paste("Creating Physical Proximity to Cases for Counties in State FIPS:", curr_state))
  
  curr_dat <- filter(county_county_dist, substr(county1, 1, 2) == curr_state) %>% 
    # Join in the COVID data
    inner_join(covid_dat, by=c("county2"="FIPS")) %>% 
    # Collapse and make the final weighted measure
    group_by(county1, date) %>% 
    summarise(dist_weighted_cases = sum(Confirmed/(1+mi_to_county)),
              dist_weighted_cases_per_10k = sum(Confirmed_per_10k/(1+mi_to_county))) %>% 
    ungroup
  
  if(is.null(final_dat)){
    final_dat <- curr_dat
  }
  else{
    final_dat <- bind_rows(final_dat, curr_dat)
  }
}

write_csv(final_dat, "../_intermediate/dist_weighted_cases.csv")
