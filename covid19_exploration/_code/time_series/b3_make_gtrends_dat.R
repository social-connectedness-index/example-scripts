# Purpose: Build the Google symptom search-based weekly time series data
# Inputs: 
#     _input/state_names.csv
#     Google symptom search data: https://raw.githubusercontent.com/google-research/open-covid-19-data/6999c225f8bf931e68de2f4d757e2c4a90569ca2/data/exports/search_trends_symptoms_dataset/United%20States%20of%20America/subregions/
#     US COVID Cases/Deaths time series data from: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/b6bf3226c922663a7aea4d2d7305edbae5d3fd14/csse_covid_19_data/csse_covid_19_time_series/
#     SCI Data: dir.sci_dat_county
#     _input/sf12010countydistancemiles.csv
#     LEX Data: dir.lex_dat
# Outputs: 
#     _intermediate/google_trends_measures.csv
# Date: 12/01/2020
# Steps:
#     1. Read in the Google Symptom Search Trends Data
#     2. Clean and create final measures

library(tidyverse)
library(lubridate)


########################################################
### 1. Read in the Google Symptom Search Trends Data ###
########################################################

# Read in a list of state names that we can use to iterate through the Google github directories
state_names <- read_csv("../_input/state_names.csv", col_names = F)$X1

# Read in Google trends COVID data
# https://github.com/google-research/open-covid-19-data/tree/master/data/exports/search_trends_symptoms_dataset
# See also: https://blog.google/technology/health/using-symptoms-search-trends-inform-covid-19-research/

final_gtrends_daily <- NULL
final_gtrends_weekly <- NULL
for(i in 1:length(state_names)){
  
  print(str_interp("Grabbing Google symptom seach trends for state ${i} of ${length(state_names)}"))
  
  # Multi-word states are treated differently in different parts of the URL
  state1 <- str_replace(state_names[i], " ", "%20")
  state2 <- str_replace(state_names[i], " ", "_")
  
  # Google doesn't create weekly data for every county. Instead, they create the weekly data ONLY
  # when it is not possible to create the daily data. This means that we have to read in both
  # the daily and weekly data and construct the weekly data ourselves in many cases.
  google_dat_url_daily <- str_interp("https://raw.githubusercontent.com/google-research/open-covid-19-data/6999c225f8bf931e68de2f4d757e2c4a90569ca2/data/exports/search_trends_symptoms_dataset/United%20States%20of%20America/subregions/${state1}/2020_US_${state2}_daily_symptoms_dataset.csv")
  google_dat_url_weekly <- str_interp("https://raw.githubusercontent.com/google-research/open-covid-19-data/6999c225f8bf931e68de2f4d757e2c4a90569ca2/data/exports/search_trends_symptoms_dataset/United%20States%20of%20America/subregions/${state1}/2020_US_${state2}_weekly_symptoms_dataset.csv")
  
  curr_gtrends_daily <- read_csv(google_dat_url_daily) %>% 
    select(
      county_fips=sub_region_2_code,
      date,
      gtrends_fever=`symptom:Fever`,
      gtrends_cough=`symptom:Cough`,
      gtrends_fatigue=`symptom:Fatigue`) %>% 
    filter(
      date >= as.Date("2020-02-03"),
      date <= as.Date("2020-11-02"),
      !is.na(county_fips)) %>% 
    mutate(county_fips = as.numeric(county_fips))
  
  curr_gtrends_weekly <- read_csv(google_dat_url_weekly) %>% 
    select(
      county_fips=sub_region_2_code,
      date,
      gtrends_fever=`symptom:Fever`,
      gtrends_cough=`symptom:Cough`,
      gtrends_fatigue=`symptom:Fatigue`) %>% 
    filter(
      date >= as.Date("2020-02-03"),
      date <= as.Date("2020-11-02"),
      !is.na(county_fips)) %>% 
    mutate(county_fips = as.numeric(county_fips))
  
  if(is.null(final_gtrends_daily)){ final_gtrends_daily <- curr_gtrends_daily }
  else{ final_gtrends_daily <- bind_rows(final_gtrends_daily, curr_gtrends_daily) }
  
  if(is.null(final_gtrends_weekly)){ final_gtrends_weekly <- curr_gtrends_weekly }
  else{ final_gtrends_weekly <- bind_rows(final_gtrends_weekly, curr_gtrends_weekly) }
  
}


##########################################
### 2. Clean and create final measures ###
##########################################

# Here we make a few cleaning decisions:
# 1. If a county has all NA Values for each symptom in the daily data we use the weekly data
# 2. If the county also has all NA values for each symptom in the weekly data we exclude it
# 3. For the counties with daily data, we take the weekly average of non-missing daily values
# 4. Our metric of interest is percent change between every other week (e.g. week 1 -> 3, 3 -> 5, etc.)
#       we provide a specific example of how this logic matches the case data timing in comments below.
# 5. If a county is missing more than 50% of time periods (because they have no data for
#       particular weeks we use) we exclude them. For counties with less than 50% missing data, we impute
#       zero change in the missing periods.

# This cleans up the daily data and aggregates it to weekly averages
daily_to_week_cleaned <- final_gtrends_daily %>%
  # Filter out the counties with all NA values
  group_by(county_fips) %>% 
  mutate(
    all_fever_na = is.na(mean(gtrends_fever, na.rm=T)),
    all_cough_na = is.na(mean(gtrends_cough, na.rm=T)),
    all_fatigue_na = is.na(mean(gtrends_fatigue, na.rm=T))) %>% 
  ungroup %>% 
  filter(!all_fever_na, !all_cough_na, !all_fatigue_na) %>% 
  select(-all_fever_na, -all_cough_na, -all_fatigue_na) %>% 
  # Make these measures weekly averages
  mutate(week_num = isoweek(date)) %>% 
  group_by(county_fips, week_num) %>% 
  summarise(
    date = min(date),
    gtrends_fever = mean(gtrends_fever, na.rm=T),
    gtrends_cough = mean(gtrends_cough, na.rm=T),
    gtrends_fatigue = mean(gtrends_fatigue, na.rm=T)) %>% 
  ungroup %>% 
  select(-week_num)

# This cleans up the weekly data
weekly_cleaned <- final_gtrends_weekly %>% 
  # Filter out the counties with all NA values
  group_by(county_fips) %>% 
  mutate(
    all_fever_na = is.na(mean(gtrends_fever, na.rm=T)),
    all_cough_na = is.na(mean(gtrends_cough, na.rm=T)),
    all_fatigue_na = is.na(mean(gtrends_fatigue, na.rm=T))) %>% 
  ungroup %>% 
  filter(!all_fever_na, !all_cough_na, !all_fatigue_na) %>% 
  # Filter out counties that are in the daily data already (as of 12-01,
  # this does no additional filtering because Google has not mixed reporting in this way)
  filter(!county_fips %in% unique(daily_to_week_cleaned$county_fips)) %>% 
  select(-all_fever_na, -all_cough_na, -all_fatigue_na)

# Now we bind the two data together and create our "nowcasting" metric of interest.
# The one period lag here is because our regression dataset will measure by period ENDING on a certain date,
# whereas 
cleaned_combined <- bind_rows(weekly_cleaned, daily_to_week_cleaned) %>% 
  mutate(
    pchg_gogl_fever = (lag(gtrends_fever, 1) - lag(gtrends_fever, 3)) / lag(gtrends_fever, 3),
    pchg_gogl_cough = (lag(gtrends_cough, 1) - lag(gtrends_cough, 3)) / lag(gtrends_cough, 3),
    pchg_gogl_fatigue = (lag(gtrends_fatigue, 1) - lag(gtrends_fatigue, 3)) / lag(gtrends_fatigue, 3),
  ) %>% 
  # Get every other week to match our regressiond data
  group_by(county_fips) %>% 
  arrange(county_fips, date) %>% 
  mutate(row_num = row_number()) %>% 
  filter(row_num %% 2 == 1) %>% 
  select(-row_num)

# Now we create our final measures. The timing here is a bit tricky.

# In the final data the row labeled 2020-04-13, for example,
# is change cases March 31 -> April 13. We want to have fever seaches w/ dates:

# Nowcast:
#   pchg_gogl_fever = March 23-March 29 -> April 6-April 12 (the nowcast created in the last step)
# Forward looking prediction:
#   l1_pchg_gogl_fever = March 9-March 15  -> March 23-March 29 (the prediction measure added in this step)

gtrends_output <- cleaned_combined %>% 
  filter(date >= as.Date("2020-03-30")) %>%  # 2020-04-13 will be the first in the regression, this is the one period lag
  group_by(county_fips) %>% 
  # Some counties are missing periods. If the county has data for >= 50% of periods for all 3 symptoms
  # in our final step we also impute missing values as zero change. Otherwise here we drop.
  mutate(
    fever_share_non_na = sum(!is.na(pchg_gogl_fever))/n(),
    cough_share_non_na = sum(!is.na(pchg_gogl_cough))/n(),
    fatigue_share_non_na = sum(!is.na(pchg_gogl_fatigue))/n()
  ) %>% 
  ungroup %>% 
  filter(fever_share_non_na >= .5, cough_share_non_na >= .5, fatigue_share_non_na >= .5) %>% 
  mutate(
    pchg_gogl_fever = if_else(is.na(pchg_gogl_fever), 0, pchg_gogl_fever),
    pchg_gogl_cough = if_else(is.na(pchg_gogl_cough), 0, pchg_gogl_cough),
    pchg_gogl_fatigue = if_else(is.na(pchg_gogl_fatigue), 0, pchg_gogl_fatigue)
  ) %>% 
  ungroup %>% 
  mutate(
    l1_pchg_gogl_fever = lag(pchg_gogl_fever, 1),
    l1_pchg_gogl_cough = lag(pchg_gogl_cough, 1),
    l1_pchg_gogl_fatigue = lag(pchg_gogl_fatigue, 1))

write_csv(gtrends_output, "../_intermediate/google_trends_measures.csv")


# In the text we describe how many counties have X% imputed values
test <- gtrends_output %>% 
  group_by(county_fips) %>% 
  slice(1)

n_impute25 <- test %>% 
  filter(fever_share_non_na >= .75, cough_share_non_na >= .25, fatigue_share_non_na >= .25) %>% 
  nrow

n_impute5 <- test %>% 
  filter(fever_share_non_na >= .95, cough_share_non_na >= .95, fatigue_share_non_na >= .95) %>% 
  nrow

n_impute25/nrow(test)
n_impute5/nrow(test)
