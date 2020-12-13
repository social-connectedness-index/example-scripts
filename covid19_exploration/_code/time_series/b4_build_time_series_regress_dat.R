# Purpose: Build the regression table for the time series analyses
#           and make maps of all time series measures
# Inputs: 
#     _intermediate/cases_and_deaths.csv
#     _intermediate/sci_weighted_cases.csv
#     _intermediate/dist_weighted_cases.csv
#     _intermediate/lex_weighted_cases_deaths.csv
#     _intermediate/share_within_x_miles.csv
#     _intermediate/google_trends_measures.csv
# Outputs: 
#     _intermediate/time_series_regress_dat.csv
#     _output/time_series_maps/${outcome}_${date}.jpg
# Date: 12/01/2020
# Steps:
#     1. Prep all data sources
#     2. Join all data sources
#     3. Create maps of all measures

library(tidyverse)
library(lubridate)
library(sf)
library(tigris)

# Throughout we will analyze changes in cases at the 2 week level and 
# changes in COVID deaths (which take longer to manifest) at the 4 week level.
# Our final table will have observations for every 2 week period, but the changes
# measures for deaths will be based on 4 week changes.
# In the regression / prediction analyses, we will only use deaths every 4 weeks.

####################################
##### 1. Prep all data sources #####
####################################

# Read in baseline COVID data
cases_and_deaths <- read_csv("../_intermediate/time_series_covid_cases_deaths_cleaned.csv")

# Make the change in cases/deaths from the last period.
cases_deaths_change <- cases_and_deaths %>% 
  mutate(FIPS = as.numeric(FIPS)) %>% 
  # In cases where the number of total cases somehow went down, we make the change zero.
  mutate(chg_cases = if_else(cases - lag(cases) < 0, 0, cases - lag(cases))) %>% 
  mutate(chg_deaths_4wk = if_else(deaths - lag(deaths, 2) < 0, 0, deaths - lag(deaths, 2))) %>% 
  ungroup

# Read in SCI-weighted cases (Social Proximity to Cases/Deaths)
# Built in b1_make_weighted_measures.R
sci_weighted_cases_deaths <- read_csv("../_intermediate/sci_weighted_cases_deaths.csv")
# Read in distance-weighted cases (Physical Proximity to Cases/Deaths)
# Built in b1_make_weighted_measures.R
dist_weighted_cases_deaths <- read_csv("../_intermediate/dist_weighted_cases_deaths.csv")

# Read in LEX-weighted cases (LEX Proximity to Cases/Deaths)
# Built in b1_make_weighted_measures.R
lex_weighted_cases_deaths <- read_csv("../_intermediate/lex_weighted_cases_deaths.csv")

# Join the three weighted cases measures
weighted_cases_deaths_dat <- sci_weighted_cases_deaths %>% 
  inner_join(dist_weighted_cases_deaths, by=c("user_loc"="county1", "date"="date")) %>% 
  # We want counties with SCI and dist, but LEX will be a smaller group of counties
  left_join(lex_weighted_cases_deaths, by=c("user_loc"="county1", "date"="date"))

# Make the changes in weighted cases
weighted_cases_deaths_change <- weighted_cases_deaths_dat %>%
  mutate(user_loc = as.numeric(user_loc)) %>% 
  group_by(user_loc) %>% 
  # Change SWC/D per 10k
  mutate(change_swc_10k = sci_weighted_cases_per_10k - lag(sci_weighted_cases_per_10k)) %>% 
  mutate(change_swc_10k = if_else(change_swc_10k < 0, 0, change_swc_10k)) %>%   # If the total somehow went down, make the change zero
  mutate(change_swd_10k_4wk = sci_weighted_deaths_per_10k - lag(sci_weighted_deaths_per_10k, 2)) %>% 
  mutate(change_swd_10k_4wk = if_else(change_swd_10k_4wk < 0, 0, change_swd_10k_4wk)) %>%   # If the total somehow went down, make the change zero
  # Change DWC/D per 10k
  mutate(change_dwc_10k = dist_weighted_cases_per_10k - lag(dist_weighted_cases_per_10k)) %>% 
  mutate(change_dwc_10k = if_else(change_dwc_10k < 0, 0, change_dwc_10k)) %>%  # If the total somehow went down, make the change zero
  mutate(change_dwd_10k_4wk = dist_weighted_deaths_per_10k - lag(dist_weighted_deaths_per_10k, 2)) %>% 
  mutate(change_dwd_10k_4wk = if_else(change_dwd_10k_4wk < 0, 0, change_dwd_10k_4wk)) %>%  # If the total somehow went down, make the change zero
  # Change LWC/D per 10k
  mutate(change_lwc_10k = lex_weighted_cases_per_10k - lag(lex_weighted_cases_per_10k)) %>% 
  mutate(change_lwc_10k = if_else(change_lwc_10k < 0, 0, change_lwc_10k)) %>%  # If the total somehow went down, make the change zero
  mutate(change_lwd_10k_4wk = lex_weighted_deaths_per_10k - lag(lex_weighted_deaths_per_10k, 2)) %>% 
  mutate(change_lwd_10k_4wk = if_else(change_lwd_10k_4wk < 0, 0, change_lwd_10k_4wk)) %>%  # If the total somehow went down, make the change zero
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
  select(fips, med_hhinc2016, popdensity2010)

# Read in the share of friends of within X miles data
share_within_x_miles <- read_csv("../_intermediate/share_within_x_miles.csv") %>% 
  mutate(user_loc = as.numeric(user_loc))

# Read in Google Trends data
gtrends_covariates <- read_csv("../_intermediate/google_trends_measures.csv")

#######################################
##### 2. Combine all data sources #####
#######################################

regress_dat <- cases_deaths_change %>% 
  inner_join(weighted_cases_deaths_change, by=c("FIPS"="user_loc", "date"="date")) %>% 
  # This starts us at case change from 2020-03-02 to 2020-03-16,
  # the first period in which the US had a notable number of cases
  filter(date >= as.Date("2020-03-16")) %>% 
  rename(county_fips = FIPS) %>% 
  left_join(oi_covariates, by=c("county_fips"="fips")) %>%
  left_join(share_within_x_miles, by=c("county_fips"="user_loc")) %>% 
  left_join(gtrends_covariates, by=c("county_fips","date")) %>% 
  # Make baseline controls
  mutate(med_hhinc2016_10k = med_hhinc2016/10000,
         popdensity2010_10k = popdensity2010/10000,
         chg_cases_10k = (chg_cases/pop) * 10000,
         chg_deaths_10k_4wk = (chg_deaths_4wk/pop) * 10000) %>% 
  group_by(county_fips) %>% 
  arrange(county_fips, date) %>% 
  # Make the lagged versions of our 'weighted' controls
  # First, for cases
  mutate(l1_chg_swc_10k = lag(change_swc_10k),
         l2_chg_swc_10k = lag(change_swc_10k, 2),
         l1_chg_dwc_10k = lag(change_dwc_10k, 1),
         l2_chg_dwc_10k = lag(change_dwc_10k, 2),
         l1_chg_lwc_10k = lag(change_lwc_10k, 1),
         l2_chg_lwc_10k = lag(change_lwc_10k, 2),
         l1_chg_cases_10k = lag(chg_cases_10k, 1),
         l2_chg_cases_10k = lag(chg_cases_10k, 2)) %>% 
  # Then, for deaths
  # Here we want to lag 2 and 4 2-week periods because we calculate
  # changes at the 4 week level
  mutate(l2_chg_swd_10k_4wk = lag(change_swd_10k_4wk, 2),
         l4_chg_swd_10k_4wk = lag(change_swd_10k_4wk, 4),
         l2_chg_dwd_10k_4wk = lag(change_dwd_10k_4wk, 2),
         l4_chg_dwd_10k_4wk = lag(change_dwd_10k_4wk, 4),
         l2_chg_lwd_10k_4wk = lag(change_lwd_10k_4wk, 2),
         l4_chg_lwd_10k_4wk = lag(change_lwd_10k_4wk, 4),
         l2_chg_deaths_10k_4wk = lag(chg_deaths_10k_4wk, 2),
         l4_chg_deaths_10k_4wk = lag(chg_deaths_10k_4wk, 4)) %>% 
  ungroup %>% 
  # Make logged versions of all measures
  # First, for cases
  mutate(log_chg_cases_10k = log(chg_cases_10k + 1),
         log_l1_chg_cases_10k = log(l1_chg_cases_10k + 1),
         log_l2_chg_cases_10k = log(l2_chg_cases_10k + 1),
         log_chg_swc_10k = log(change_swc_10k + 1),
         log_l1_chg_swc_10k = log(l1_chg_swc_10k + 1), 
         log_l2_chg_swc_10k = log(l2_chg_swc_10k + 1),
         log_chg_dwc_10k = log(change_dwc_10k + 1),
         log_l1_chg_dwc_10k = log(l1_chg_dwc_10k + 1),
         log_l2_chg_dwc_10k = log(l2_chg_dwc_10k + 1),
         log_chg_lwc_10k = log(change_lwc_10k + 1),
         log_l1_chg_lwc_10k = log(l1_chg_lwc_10k + 1),
         log_l2_chg_lwc_10k = log(l2_chg_lwc_10k + 1)
  ) %>% 
  # Then, for deaths
  mutate(log_chg_deaths_10k_4wk = log(chg_deaths_10k_4wk + 1),
         log_l2_chg_deaths_10k_4wk = log(l2_chg_deaths_10k_4wk + 1),
         log_l4_chg_deaths_10k_4wk = log(l4_chg_deaths_10k_4wk + 1),
         log_chg_swd_10k_4wk = log(change_swd_10k_4wk + 1),
         log_l2_chg_swd_10k_4wk = log(l2_chg_swd_10k_4wk + 1), 
         log_l4_chg_swd_10k_4wk = log(l4_chg_swd_10k_4wk + 1),
         log_chg_dwd_10k_4wk = log(change_dwd_10k_4wk + 1),
         log_l2_chg_dwd_10k_4wk = log(l2_chg_dwd_10k_4wk + 1),
         log_l4_chg_dwd_10k_4wk = log(l4_chg_dwd_10k_4wk + 1),
         log_chg_lwd_10k_4wk = log(change_lwd_10k_4wk + 1),
         log_l2_chg_lwd_10k_4wk = log(l2_chg_lwd_10k_4wk + 1),
         log_l4_chg_lwd_10k_4wk = log(l4_chg_lwd_10k_4wk + 1)
  ) %>% 
  # For simplicity later on we convert dates to week numbers
  mutate(week_num = week(date))

write_csv(regress_dat, "../_intermediate/time_series_regress_dat.csv")

