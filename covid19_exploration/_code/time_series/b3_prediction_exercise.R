# Purpose: Predict future cases using lagged weighted measures
# Inputs: 
#     _intermediate/time_series_regress_dat.csv
# Outputs: 
#     _output/tables/raw/rmse_different_models.csv
# Date: 07/30/2020
# Steps:
#     1. Simple Prediction Exercise

library(tidyverse)
library(lubridate)
library(randomForest)

##########################################
##### 1. Simple Prediction Exercise #####
##########################################

# Read in file created in b2_build_time_series_regress_dat.R
dat_in <- read_csv("../_intermediate/time_series_regress_dat.csv")

# We need two weeks of lagged data to train the model
regress_dat_pred <- filter(dat_in, week_num >= 15)

# Set a seed for replicable results
set.seed(17)

# Now make a prediction for the NEXT two week period, using
# the lagged data looking two weeks in the past.
final_tibble <- NULL
for(i in seq(17, 29, by=2)){
  
  # We train the model using ALL weeks before our week of interest
  train_dat <- filter(regress_dat_pred, week_num < i)
  # We the test the model using the next week
  test_dat <- filter(regress_dat_pred, week_num == i)
  
  # Hack to store the string of the date for writing the map
  curr_date_str <- as.character(as.Date(unique(test_dat$date), origin = lubridate::origin))
  print(paste("Making Predictions for Period Ending on:", curr_date_str))
  
  # Linear Model with Social Proximity to Capital
  glm_with_sci <- glm(log_chg_cases_10k ~ log_l1_chg_swc_10k + log_l2_chg_swc_10k + log_l1_chg_dwc_10k + log_l2_chg_dwc_10k + 
                        log_l1_chg_cases_10k + log_l2_chg_cases_10k + med_hhinc2016_10k + popdensity2010_10k,
                      data=train_dat)
  
  # Linear Model without Social Proximity to Capital
  glm_without_sci <- glm(log_chg_cases_10k ~ log_l1_chg_dwc_10k + log_l2_chg_dwc_10k + 
                           log_l1_chg_cases_10k + log_l2_chg_cases_10k + med_hhinc2016_10k + popdensity2010_10k,
                         data=train_dat)
  
  # Random Forest with Social Proximity to Capital
  # We use all the 'out of the box' tuning parameters (ntree = 500, mtry=x/3)
  rf_with_sci <- randomForest(log_chg_cases_10k ~ log_l1_chg_swc_10k + log_l2_chg_swc_10k + log_l1_chg_dwc_10k + log_l2_chg_dwc_10k + 
                                log_l1_chg_cases_10k + log_l2_chg_cases_10k + med_hhinc2016_10k + popdensity2010_10k,
                              data=train_dat)
  
  # Random Forest without Social Proximity to Capital
  rf_without_sci <- randomForest(log_chg_cases_10k ~ log_l1_chg_dwc_10k + log_l2_chg_dwc_10k + 
                                   log_l1_chg_cases_10k + log_l2_chg_cases_10k + med_hhinc2016_10k + popdensity2010_10k,
                                 data=train_dat)
  
  # Predict using linear models
  test_dat$sci_glm_pred <- predict(glm_with_sci, test_dat, type="response")
  test_dat$no_sci_glm_pred <- predict(glm_without_sci, test_dat, type="response")
  
  # Predict using random forest models
  test_dat$sci_rf_pred <- predict(rf_with_sci, test_dat)
  test_dat$no_sci_rf_pred <- predict(rf_without_sci, test_dat)
  
  # Calculate all the relevant RMSEs
  rmse_glm_sci <- sqrt(mean((test_dat$log_chg_cases_10k - test_dat$sci_glm_pred)^2))
  rmse_glm_no_sci <- sqrt(mean((test_dat$log_chg_cases_10k - test_dat$no_sci_glm_pred)^2))
  rmse_rf_sci <- sqrt(mean((test_dat$log_chg_cases_10k - test_dat$sci_rf_pred)^2))
  rmse_rf_no_sci <- sqrt(mean((test_dat$log_chg_cases_10k - test_dat$no_sci_rf_pred)^2))
  
  # Fill a tibble with current RMSEs
  curr_tibble <- tibble(week_num = i,
                        rmse_glm_sci = rmse_glm_sci, rmse_glm_no_sci = rmse_glm_no_sci,
                        rmse_rf_sci = rmse_rf_sci, rmse_rf_no_sci = rmse_rf_no_sci)
  
  # Add it to the master tibble
  if(is.null(final_tibble)){
    final_tibble <- curr_tibble
  }
  else{
    final_tibble <- bind_rows(final_tibble, curr_tibble)
  }
}

# Write to file
final_tibble %>% 
  mutate(
    diff_glm_rmse = rmse_glm_sci - rmse_glm_no_sci,
    diff_rf_rmse = rmse_rf_sci - rmse_rf_no_sci,) %>% 
  select(rmse_glm_no_sci, rmse_glm_sci, diff_glm_rmse, rmse_rf_no_sci, rmse_rf_sci, diff_rf_rmse) %>% 
  write_csv("../_output/tables/raw/rmse_different_models.csv")