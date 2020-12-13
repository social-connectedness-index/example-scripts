# Purpose: Make our two "weighted" measures, 
#           Social Proximity to Cases and Physical Proximity to Cases
# Inputs: 
#     _input/ACS_17_5YR_DP05.csv
#     US COVID Cases/Deaths time series data from: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/b6bf3226c922663a7aea4d2d7305edbae5d3fd14/csse_covid_19_data/csse_covid_19_time_series/
#     SCI Data: dir.sci_dat_county
#     _input/sf12010countydistancemiles.csv
#     LEX Data: dir.lex_dat
# Outputs: 
#     _intermediate/time_series_covid_cases_deaths_cleaned.csv
#     _intermediate/sci_weighted_cases.csv
#     _intermediate/dist_weighted_cases.csv
#     _intermediate/lex_weighted_cases_deaths.csv
# Date: 12/01/2020
# Steps:
#     1. Prep the datasets
#     2. Generate Social Proximity to Cases
#     3. Generate Physical Proximity to Cases
#     4. Generate LEX Proximity to Cases

library(tidyverse)
library(lubridate)


################################
##### 1. Prep the datasets #####
################################

# Read in county pops, pulled from Census
county_pops <- read_csv("../_input/ACS_17_5YR_DP05.csv") %>% 
  select(fips = GEO.id2, label=`GEO.display-label`, pop=HC01_VC03) %>% 
  mutate(fips = str_pad(as.character(fips), 5, "left", "0")) %>% 
  ungroup

# A little function to clean the time series JH data that we will use for both cases and deaths
clean_jh_time_series <- function(path){
  
  dat <- read_csv(path) %>% 
    filter(!is.na(FIPS)) %>%
    filter(iso2 == "US") %>% # filters out territories
    filter(FIPS < 80000) %>% # filters out 'unassigned' cases within states
    mutate(FIPS = as.numeric(FIPS)) %>% 
    select(FIPS, Province_State, ends_with("/20")) %>% 
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
    # In the paper, our results run only through November 09, 2020. One could
    # add more periods by removing this last restriction.
    filter(date <= as.Date("2020-11-02"))
  
  dat
}

covid_dat <- clean_jh_time_series("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/b6bf3226c922663a7aea4d2d7305edbae5d3fd14/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
deaths_dat <- clean_jh_time_series("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/b6bf3226c922663a7aea4d2d7305edbae5d3fd14/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")

covid_cases_and_deaths <- covid_dat %>% 
  rename(cases=Confirmed, cases_per_10k=Confirmed_per_10k) %>% 
  inner_join(select(deaths_dat, FIPS, date, Confirmed, Confirmed_per_10k), by=c("FIPS","date")) %>% 
  rename(deaths=Confirmed, deaths_per_10k=Confirmed_per_10k)

write_csv(covid_cases_and_deaths, "../_intermediate/time_series_covid_cases_deaths_cleaned.csv")

# Read in SCI data
sci_dat <- read_tsv(dir.sci_dat_county)
sci_dat <- rename(sci_dat, sci=scaled_sci) %>%
  mutate(user_loc=as.numeric(user_loc),
         fr_loc=as.numeric(fr_loc)) %>% 
  filter(user_loc <= 57000 & fr_loc <= 57000) %>%  # removes territories
  mutate(user_loc=str_pad(as.character(user_loc), 5, "left", "0"),
         fr_loc=str_pad(as.character(fr_loc), 5, "left", "0"))

# Get the share of total SCI from each county
sci_dat <- sci_dat %>%
  group_by(user_loc) %>%
  mutate(total_sci = sum(sci)) %>%
  mutate(share_sci = sci/total_sci) %>%
  ungroup

# Get a list of states.
states <- unique(covid_cases_and_deaths$state_FIPS)


#################################################
##### 2. Generate Social Proximity to Cases #####
#################################################

final_dat <- NULL

# We will loop through user states when we make our weighted
# measures to avoid OOM'ing on a large join.
for(curr_state in states) {
  
  print(paste("Creating Social Proximity to Cases for Counties in State FIPS:", curr_state))
  
  curr_dat <- sci_dat %>% 
    filter(substr(user_loc, 1, 2) == curr_state) %>% 
    # Join in the COVID data
    inner_join(covid_cases_and_deaths, by=c("fr_loc"="FIPS")) %>% 
    # Collapse and make the final weighted measure
    group_by(user_loc, date) %>% 
    summarise(sci_weighted_cases = sum(cases*share_sci),
              sci_weighted_cases_per_10k = sum(cases_per_10k*share_sci),
              sci_weighted_deaths = sum(deaths*share_sci),
              sci_weighted_deaths_per_10k = sum(deaths_per_10k*share_sci)) %>% 
    ungroup
  
  if(is.null(final_dat)){
    final_dat <- curr_dat
  }
  else{
    final_dat <- bind_rows(final_dat, curr_dat)
  }
}

write_csv(final_dat, "../_intermediate/sci_weighted_cases_deaths.csv")


###################################################
##### 3. Generate Physical Proximity to Cases #####
###################################################

# Read in county-county distance from NBER
# https://data.nber.org/data/county-distance-database.html
county_county_dist <- read_csv("../_input/sf12010countydistancemiles.csv")

final_dat <- NULL

# Again, we loop through user states when we make our weighted
# measures to avoid OOM'ing on a large join.
for(curr_state in states) {
  
  print(paste("Creating Physical Proximity to Cases for Counties in State FIPS:", curr_state))
  
  curr_dat <- filter(county_county_dist, substr(county1, 1, 2) == curr_state) %>% 
    # Join in the COVID data
    inner_join(covid_cases_and_deaths, by=c("county2"="FIPS")) %>% 
    # Collapse and make the final weighted measure
    group_by(county1, date) %>% 
    summarise(dist_weighted_cases = sum(cases/(1+mi_to_county)),
              dist_weighted_cases_per_10k = sum(cases_per_10k/(1+mi_to_county)),
              dist_weighted_deaths = sum(deaths/(1+mi_to_county)),
              dist_weighted_deaths_per_10k = sum(deaths_per_10k/(1+mi_to_county))) %>% 
    ungroup
  
  if(is.null(final_dat)){
    final_dat <- curr_dat
  }
  else{
    final_dat <- bind_rows(final_dat, curr_dat)
  }
}

write_csv(final_dat, "../_intermediate/dist_weighted_cases_deaths.csv")


##############################################
##### 4. Generate LEX Proximity to Cases #####
##############################################

# Get a list of weeks (NOTE: they started releasing LEX only on Saturdays and Wednesdays
# in August. Starting 2020-08-31 we will use the Saturday before the Monday of interest)
weeks <- as.character(seq(as.Date("2020-02-03"), as.Date("2020-10-26"), by="2 weeks"))

final_dat <- NULL

# Because the network measure here varies by time,
# we will loop through weeks (instead of states)
for(week in weeks) {
  
  print(paste("Creating LEX Proximity to Cases on Date:", week))
  
  # Make the Monday -> earlier Saturday adjustment for the end of the time series
  date_to_read <- if_else(as.Date(week) >= as.Date("2020-08-31"),
                          as.character(as.Date(week) - 2),
                          week)
  
  # Because of an interesting Mac OS problem with gzipped files we download and unzip these data into an external
  # folder and set the directory in 1_run_all_R_scripts.R (instead of reading straight from github)
  # You can find the original data at: https://github.com/COVIDExposureIndices/COVIDExposureIndices/tree/master/lex_data
  lex_dat <- read_csv(str_interp("${dir.lex_dat}/county_lex_${date_to_read}.csv")) %>% 
    rename(county1=COUNTY_PRE) %>% 
    gather(county2, value, -county1)
  
  # Get the share of total LEX from each county
  lex_dat <- lex_dat %>%
    group_by(county1) %>%
    mutate(total_lex = sum(value)) %>%
    mutate(share_lex = value/total_lex) %>%
    ungroup
  
  curr_dat <- covid_cases_and_deaths %>% 
    filter(date == as.Date(week)) %>% 
    # Join in the LEX data
    inner_join(lex_dat, by=c("FIPS"="county2")) %>% 
    # Collapse and make the final weighted measure
    group_by(county1, date) %>% 
    summarise(lex_weighted_cases = sum(cases*share_lex),
              lex_weighted_cases_per_10k = sum(cases_per_10k*share_lex),
              lex_weighted_deaths = sum(deaths*share_lex),
              lex_weighted_deaths_per_10k = sum(deaths_per_10k*share_lex)) %>% 
    ungroup
  
  if(is.null(final_dat)){
    final_dat <- curr_dat
  }
  else{
    final_dat <- bind_rows(final_dat, curr_dat)
  }
}

write_csv(final_dat, "../_intermediate/lex_weighted_cases_deaths.csv")
