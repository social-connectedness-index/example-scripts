# Purpose: Create the out-of-sample hotspot propogation prediction exercise
#           (training the model on Lodi and using it to predict from Westchester)
# Inputs: 
#       _intermediate/westchester_sci_covariates.csv
#       _intermediate/lodi_sci_covariates.csv
#       _input/italy_xw.csv
#       Italy COVID data from: https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province-20200330.csv
# Outputs:
#     _intermediate/westchester_sci_covariates.csv
#     _intermediate/lodi_sci_covariates.csv"
# Date: 12/01/2020

# Steps:
#     1. Generate SCI/cases data
#     2. Make out-of-sample predictions

library(tidyverse)
library(randomForest)


##################################
### 1. Generate SCI/cases data ###
##################################

# The data we constructed in a2_prep_data_for_binscatters.R uses Lodi case data
# as of March 30. We instead want to imagine a hypothetical in which we know 
# Westchester is a hotspot on March 10, and we want to use the information we have
# about COVID spread from Lodi to predict which counties are most at risk from spread
# from Westchester. To do this, we need to replace the Lodi case data with March 10 case data.

# Read in the Lodi SCI data
lodi_sci_dat_march30 <- read_csv("../_intermediate/lodi_sci_covariates.csv")

# Read in the Italy COVID data
it_dat <- read_csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province-20200310.csv")

# Read in the Italy province code -> NUTS3 crosswalk.
# This was constructed by hand.
# NOTE: This crosswalk maps all of Sardinia to the region level.
# One must group resulting data by ITG2 after using this.
it_xw <- read_csv("../_input/italy_xw.csv")

# Crosswalk to get COVID at NUTS3 level
it_covid_nuts3 <- 
  left_join(it_dat, it_xw, by=c("sigla_provincia"="Code")) %>% 
  filter(denominazione_provincia != "In fase di definizione/aggiornamento") %>% 
  select(nuts3_code = key, total_cases = totale_casi) %>% 
  # We group together Sardinia (this is done by the crosswalk)
  group_by(nuts3_code) %>% 
  summarise(total_cases = sum(total_cases))

# Finally, we replace the March 30 Lodi case data with the March 10 Lodi case data
lodi_sci_dat <- lodi_sci_dat_march30 %>% 
  select(-total_cases, -cases_per_10k) %>% 
  left_join(it_covid_nuts3, by="nuts3_code") %>% 
  mutate(cases_per_10k = (total_cases/pop_2018)*10000)

# Read in Westchester SCI data (March 30)
westchester_sci_dat <- read_csv("../_intermediate/westchester_sci_covariates.csv")

#########################################
### 2. Make out-of-sample predictions ###
#########################################

# We are going to normalize the LHS and RHS measures by subtracting the mean
# and dividing by the standard deviation. This is going to give us a couple nice properties:
# (1) Overall level differences in the LHS measure won't matter, instead we focus on relative risk
# (2) On the RHS we do some degree of controlling for the very different geographies of Italy and the US
#       (which would make non-normalized predictions non-sensical)

# Property (2) should not be interpreted as a complete solution to the problem that
# there is, for example, no pair of Italian regions with a qualitively comparable relationship
# to the relationship between a rural New Mexico county and Westchester. This is a limitation
# of this exercise.
normalize <- function(x){
  (x - mean(x))/ sd(x)
}

# We use this function to make datasets with analogous variables.
# This will allow us to train with Lodi and test with Westchester.
lodi_cleaned <- lodi_sci_dat %>% 
  mutate(
    log_sci = log(sci),
    log_dist = log(dist)) %>% 
  select(nuts3_code, cases_per_10k, log_sci, popdensity=pop_per_km, dist, log_dist, income=gdp_per_hab) %>% 
  # As in our baseline exercise, we exclude nearby counties
  filter(!is.na(dist), dist >= 50 * 1.60934) %>% 
  mutate_if(is.numeric, normalize)

westchester_cleaned <- westchester_sci_dat %>% 
  mutate(
    log_sci = log(sci),
    log_dist = log(dist)) %>% 
  select(county_fips, cases_per_10k, log_sci, popdensity=popdensity2010, dist, log_dist, income=med_hhinc2016) %>% 
  # As in our baseline exercise, we exclude nearby counties
  filter(!is.na(dist), !is.na(cases_per_10k), dist >= 50) %>% 
  mutate(county_fips = as.character(county_fips)) %>% 
  mutate_if(is.numeric, normalize)

# These our are very simple models, w/ and w/o log(SCI)
baseline_model <- "cases_per_10k ~ popdensity + log_dist + income"
sci_model <- paste(baseline_model, "log_sci", sep = " + ")

# First we train the models using lodi_cleaned
glm_model_wo_sci <- lm(as.formula(baseline_model), dat=lodi_cleaned)
rf_model_wo_sci <- randomForest(as.formula(baseline_model), data=lodi_cleaned)
glm_model_w_sci <- lm(as.formula(sci_model), dat=lodi_cleaned)
rf_model_w_sci <- randomForest(as.formula(sci_model), data=lodi_cleaned)

# Then we predict on westchester_cleaned, using these models 
westchester_cleaned$glm_pred_wo_sci <- predict(glm_model_wo_sci, westchester_cleaned, type="response")
westchester_cleaned$rf_pred_wo_sci <- predict(rf_model_wo_sci, westchester_cleaned, type="response")
westchester_cleaned$glm_pred_w_sci <- predict(glm_model_w_sci, westchester_cleaned, type="response")
westchester_cleaned$rf_pred_w_sci <- predict(rf_model_w_sci, westchester_cleaned, type="response")

# We construct our first measure of accuracy: RMSEs
rmse_glm_wo_sci <- sqrt(mean((westchester_cleaned$cases_per_10k - westchester_cleaned$glm_pred_wo_sci)^2))
rmse_glm_w_sci <- sqrt(mean((westchester_cleaned$cases_per_10k - westchester_cleaned$glm_pred_w_sci)^2))
rmse_rf_wo_sci <- sqrt(mean((westchester_cleaned$cases_per_10k - westchester_cleaned$rf_pred_wo_sci)^2))
rmse_rf_w_sci <- sqrt(mean((westchester_cleaned$cases_per_10k - westchester_cleaned$rf_pred_w_sci)^2))

# We construct our second measure of accuracy: Rank-Rank Correlation
westchester_cleaned$true_rank <- rank(desc(westchester_cleaned$cases_per_10k), ties.method = "first")
westchester_cleaned$glm_wo_sci_rank <- rank(desc(westchester_cleaned$glm_pred_wo_sci), ties.method = "first")
westchester_cleaned$glm_w_sci_rank <- rank(desc(westchester_cleaned$glm_pred_w_sci), ties.method = "first")
westchester_cleaned$rf_wo_sci_rank <- rank(desc(westchester_cleaned$rf_pred_wo_sci), ties.method = "first")
westchester_cleaned$rf_w_sci_rank <- rank(desc(westchester_cleaned$rf_pred_w_sci), ties.method = "first")

cor_glm_wo_sci <- cor(westchester_cleaned$true_rank, westchester_cleaned$glm_wo_sci_rank)
cor_glm_w_sci <- cor(westchester_cleaned$true_rank, westchester_cleaned$glm_w_sci_rank)
cor_rf_wo_sci <- cor(westchester_cleaned$true_rank, westchester_cleaned$rf_wo_sci_rank)
cor_rf_w_sci <- cor(westchester_cleaned$true_rank, westchester_cleaned$rf_w_sci_rank)

# Finally, we bring it all together in an output tibble
final_tibble <- tibble(
  
  wo_sci_glm = c(rmse_glm_wo_sci, cor_glm_wo_sci),
  w_sci_glm = c(rmse_glm_w_sci, cor_glm_w_sci),
  diff_glm = c(rmse_glm_w_sci - rmse_glm_wo_sci, cor_glm_w_sci - cor_glm_wo_sci),
  
  wo_sci_rf = c(rmse_rf_wo_sci, cor_rf_wo_sci),
  w_sci_rf = c(rmse_rf_w_sci, cor_rf_w_sci),
  diff_rf = c(rmse_rf_w_sci - rmse_rf_wo_sci, cor_rf_w_sci - cor_rf_wo_sci)
  
)

write_csv(final_tibble, "../_output/tables/raw/train_it_test_us.csv")

