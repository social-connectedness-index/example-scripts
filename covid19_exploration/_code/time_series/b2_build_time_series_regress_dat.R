# Purpose: Build the regression table for the time series analyses
#           and make maps of all time series measures
# Inputs: 
#     US COVID time series data from: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv
#     _intermediate/sci_weighted_cases.csv
#     _intermediate/dist_weighted_cases.csv
# Outputs: 
#     _intermediate/time_series_regress_dat.csv
# Date: 07/30/2020
# Steps:
#     1. Prep all data sources
#     2. Join all data sources

library(tidyverse)
library(lubridate)
library(sf)
library(tigris)


####################################
##### 1. Prep all data sources #####
####################################

# Read in baseline COVID data
cases <- 
  read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>% 
  filter(!is.na(FIPS)) %>% 
  filter(iso2 == "US") %>% # filters out territories
  filter(FIPS < 80000) %>% # filters out 'unassigned' cases within states
  mutate(FIPS = as.numeric(FIPS)) %>% 
  select(-c("UID", "iso2", "iso3", "code3", "Admin2", "Country_Region",
            "Lat", "Long_", "Combined_Key")) %>% 
  # Make the data long
  gather(date, Confirmed, -c("FIPS", "Province_State")) %>% 
  # Convert to R's date type
  mutate(date = parse_date(date, "%m/%d/%y"))

# Filter data to every other Monday and
# make the change in cases from the last period.
cases_change <- cases %>% 
  # Filter to Mondays
  filter(wday(date) == 2) %>% 
  # Get every other Monday
  group_by(FIPS) %>% 
  arrange(FIPS, date) %>% 
  mutate(row_num = row_number()) %>% 
  filter(row_num %% 2 == 0) %>% 
  select(-row_num) %>% 
  # In cases where the number of total cases somehow went down, we make the change zero.
  mutate(chg_cases = if_else(Confirmed - lag(Confirmed) < 0, 0, Confirmed - lag(Confirmed))) %>% 
  ungroup

# Read in SCI-weighted cases (Social Proximity to Cases)
# Built in b1_make_weighted_measures.R
sci_weighted_cases <- read_csv("../_intermediate/sci_weighted_cases.csv")

# Read in distance-weighted cases (Physical Proximity to Cases)
# Built in b1_make_weighted_measures.R
dist_weighted_cases <- read_csv("../_intermediate/dist_weighted_cases.csv")

# Join the two weighted cases measures
weighted_cases_dat <- sci_weighted_cases %>% 
  inner_join(dist_weighted_cases, by=c("user_loc"="county1", "date"="date"))

# Make the changes in weighted cases
weighted_cases_change <- weighted_cases_dat %>%
  mutate(user_loc = as.numeric(user_loc)) %>% 
  group_by(user_loc) %>% 
  # Change SWC per 10k
  mutate(change_swc_10k = sci_weighted_cases_per_10k - lag(sci_weighted_cases_per_10k)) %>% 
  mutate(change_swc_10k = if_else(change_swc_10k < 0, 0, change_swc_10k)) %>%   # If the total somehow went down, make the change zero
  # Change DWC per 10k
  mutate(change_dwc_10k = dist_weighted_cases_per_10k - lag(dist_weighted_cases_per_10k)) %>% 
  mutate(change_dwc_10k = if_else(change_dwc_10k < 0, 0, change_dwc_10k)) %>%  # If the total somehow went down, make the change zero
  ungroup

# Read in county pops, pulled from Census
county_pops <- read_csv("../_input/ACS_17_5YR_DP05.csv") %>% 
  select(fips = GEO.id2, label=`GEO.display-label`, pop=HC01_VC03) %>% 
  # Match the Hopkins data use of a single NYC
  mutate(fips = if_else(fips %in% c(36005, 36047, 36081, 36085), 36061, fips)) %>% 
  group_by(fips) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup

# Read in county data from Opportunity Insights data library
# https://opportunityinsights.org/data/
# See: Neighborhood Characteristics by County
oi_covariates <- read_csv("../_input/cty_covariates_oi.csv") %>% 
  mutate(county=as.character(county)) %>% 
  mutate(county=case_when(
    str_length(county) == 1 ~ paste0("00", county),
    str_length(county) == 2 ~ paste0("0", county),
    TRUE ~ county
  )) %>% 
  mutate(fips = as.numeric(paste0(state, county))) %>% 
  select(fips, med_hhinc2016, popdensity2010) %>% 
  # Match the Hopkins data use of a single NYC.
  # Here we use the mean of NYC county measures of median household income
  # and pop density to stand in place for the "true" city-wide measures.
  mutate(fips = if_else(fips %in% c(36005, 36047, 36081, 36085), 36061, fips)) %>% 
  group_by(fips) %>% 
  summarise(med_hhinc2016 = mean(med_hhinc2016),
            popdensity2010 = mean(popdensity2010)) %>% 
  ungroup


#######################################
##### 2. Combine all data sources #####
#######################################

regress_dat <- cases_change %>% 
  inner_join(weighted_cases_change, by=c("FIPS"="user_loc", "date"="date")) %>% 
  # This starts us at case change from 2020-03-02 to 2020-03-16,
  # the first period in which the US had a notable number of cases
  filter(date >= as.Date("2020-03-16")) %>% 
  rename(county_fips = FIPS) %>% 
  left_join(oi_covariates, by=c("county_fips"="fips")) %>%
  left_join(county_pops, by=c("county_fips"="fips")) %>% 
  # Make baseline controls
  mutate(med_hhinc2016_10k = med_hhinc2016/10000,
         popdensity2010_10k = popdensity2010/10000,
         chg_cases_10k = (chg_cases/pop) * 10000) %>% 
  group_by(county_fips) %>% 
  arrange(county_fips, date) %>% 
  # Make our 'weighted' controls
  mutate(l1_chg_swc_10k = lag(change_swc_10k),
         l2_chg_swc_10k = lag(change_swc_10k, 2),
         l1_chg_dwc_10k = lag(change_dwc_10k, 1),
         l2_chg_dwc_10k = lag(change_dwc_10k, 2),
         l1_chg_cases_10k = lag(chg_cases_10k, 1),
         l2_chg_cases_10k = lag(chg_cases_10k, 2)) %>% 
  ungroup %>% 
  # Make logged versions of all measures
  mutate(log_chg_cases_10k = log(chg_cases_10k + 1),
         log_chg_swc_10k = log(change_swc_10k + 1),
         log_chg_dwc_10k = log(change_dwc_10k + 1),
         log_l1_chg_swc_10k = log(l1_chg_swc_10k + 1), 
         log_l2_chg_swc_10k = log(l2_chg_swc_10k + 1),
         log_l1_chg_dwc_10k = log(l1_chg_dwc_10k + 1),
         log_l2_chg_dwc_10k = log(l2_chg_dwc_10k + 1),
         log_l1_chg_cases_10k = log(l1_chg_cases_10k + 1),
         log_l2_chg_cases_10k = log(l2_chg_cases_10k + 1)
  ) %>% 
  # For simplicity later on we convert dates to week numbers
  mutate(week_num = week(date))
  

write_csv(regress_dat, "../_intermediate/time_series_regress_dat.csv")