# Purpose: Predict future cases using lagged weighted measures
# Inputs: 
#     _code/time_series/bX_prediction_exercise_helper_funs.R
#     _intermediate/time_series_regress_dat.csv
# Outputs: 
#     _output/tables/raw/pred_exercise1_${curr_lhs}.csv
#     _output/tables/raw/pred_exercise2_${curr_lhs}.csv
#     _output/tables/raw/pred_exercise3_${curr_lhs}.csv
# Date: 12/03/2020
# Steps:
#     1. Specify the models
#     2. Train the models (TIME INTENSIVE)
#     3A. Prediction exercise A - Baseline
#     3B. Prediction exercise B - Best available model
#     3C. Prediction exercise C - Only complete cases

library(tidyverse)
library(lubridate)
library(randomForest)

source("time_series/bX_prediction_exercise_helper_funs.R")

# In the end this file will train:
# 8 case model specifications * 14 weeks = 112 models
# 8 deaths model specifications * 6 weeks = 48 models

# Traing all 160 models is very computationally expensive and will take time to run

#################################
##### 1. Specify the models #####
#################################

# These are the pieces of different formulas we will want to use
baseline_cases <- "log_chg_cases_10k ~ log_l1_chg_dwc_10k + log_l2_chg_dwc_10k + 
                    log_l1_chg_cases_10k + log_l2_chg_cases_10k + med_hhinc2016_10k + popdensity2010_10k"

baseline_deaths <- "log_chg_deaths_10k_4wk ~ log_l2_chg_dwd_10k_4wk + log_l4_chg_dwd_10k_4wk +
                      log_l2_chg_deaths_10k_4wk + log_l4_chg_deaths_10k_4wk + med_hhinc2016_10k + popdensity2010_10k"

lex_cases_vars <- "log_l1_chg_lwc_10k + log_l2_chg_lwc_10k"

lex_deaths_vars <- "log_l2_chg_lwd_10k_4wk + log_l4_chg_lwd_10k_4wk"
  
sci_cases_vars <- "log_l1_chg_swc_10k + log_l2_chg_swc_10k"

sci_deaths_vars <- "log_l2_chg_swd_10k_4wk + log_l4_chg_swd_10k_4wk"

google_vars <- "l1_pchg_gogl_fever + l1_pchg_gogl_cough + l1_pchg_gogl_fatigue"


# Using these pieces, construct the specification formulas we will need

# Within each of these lists, the LHS MUST be consistent
cases_specs <- list(
  baseline = baseline_cases,
  baseline_lex = paste(baseline_cases, lex_cases_vars, sep = " + "),
  baseline_google = paste(baseline_cases, google_vars, sep = " + "),
  baseline_lex_google = paste(baseline_cases, lex_cases_vars, google_vars, sep = " + "),
  baseline_sci = paste(baseline_cases, sci_cases_vars, sep = " + "),
  baseline_lex_sci = paste(baseline_cases, lex_cases_vars, sci_cases_vars, sep = " + "),
  baseline_google_sci = paste(baseline_cases, google_vars, sci_cases_vars, sep = " + "),
  baseline_lex_google_sci = paste(baseline_cases, lex_cases_vars, google_vars, sci_cases_vars, sep = " + ")
)

deaths_specs <- list(
  baseline = baseline_deaths,
  baseline_lex = paste(baseline_deaths, lex_deaths_vars, sep = " + "),
  baseline_google = paste(baseline_deaths, google_vars, sep = " + "),
  baseline_lex_google = paste(baseline_deaths, lex_deaths_vars, google_vars, sep = " + "),
  baseline_sci = paste(baseline_deaths, sci_deaths_vars, sep = " + "),
  baseline_lex_sci = paste(baseline_deaths, lex_deaths_vars, sci_deaths_vars, sep = " + "),
  baseline_google_sci = paste(baseline_deaths, google_vars, sci_deaths_vars, sep = " + "),
  baseline_lex_google_sci = paste(baseline_deaths, lex_deaths_vars, google_vars, sci_deaths_vars, sep = " + ")
)

# Compile the specs to use - names MUST be the LHS variable
specs_to_use <- list(
  log_chg_cases_10k = cases_specs,
  log_chg_deaths_10k_4wk = deaths_specs)



###########################
### 2. Train the models ### 
###########################

# Read in file created in b2_build_time_series_regress_dat.R
dat_in <- read_csv("../_intermediate/time_series_regress_dat.csv")

# We need two weeks of lagged data to train the model
regress_dat_pred <- filter(dat_in, week_num >= 15)

# Set a seed for replicable results
set.seed(17)

# The list 'final_models' will store the corresponding final models
final_models <- vector("list", length(specs_to_use))
names(final_models) <- names(specs_to_use)

# Now for each LHS, each specification, each week train using all periods in the past

#### For each of the two LHS ####
for(curr_lhs in names(specs_to_use)){
  
  all_specs <- specs_to_use[[curr_lhs]]
  
  
  #### For each of the specifications with that LHS ####
  for(i in 1:length(all_specs)){
    
    curr_spec_form <- all_specs[[i]]
    curr_spec_name <- names(all_specs)[i]
    
    # This will store the actual trained models
    curr_spec_models <- list()
    

    #### For each of the time periods ####
    for(j in seq(17, 43, by=2)){
      
      dat_to_use <- regress_dat_pred
      
      # With the lex models, we also need to filter out where lex is missing
      if(grepl("_lex", curr_spec_name)){
        dat_to_use <- dat_to_use %>% 
          filter(!is.na(log_l1_chg_lwc_10k),
                 !is.na(log_l2_chg_lwc_10k))
      }
      # With the google models, we also need to filter out where google is missing
      if(grepl("_google", curr_spec_name)){
        dat_to_use <- dat_to_use %>%
          filter(!is.na(l1_pchg_gogl_fever),
                 !is.na(l1_pchg_gogl_cough),
                 !is.na(l1_pchg_gogl_fatigue))
      }
      
      # With the deaths models, each observation here is based on 4-week changes, but we have bi-weekly observations.
      # To get observations every 4 weeks we filter to mod(week_num,4) == 1 -> 19, 23, 27, 31, ...
      # We start training wuth week 19 data because we now need eight weeks of lagged data for these regressions,
      # but the first period in which we will be able to do a test exercise is week 23.
      if(grepl("deaths", curr_lhs)){

        # Skips if its not a relevant week
        if(j == 19 | j %% 4 != 3){next}
        
        # Ensures our training periods are non-overlapping (and match the test periods)
        dat_to_use <- filter(dat_to_use, week_num >= 19, week_num %% 4 == 3)
      }
      
      print(paste("Training model for", curr_lhs, "with", curr_spec_name, "specification in week num", j))
      
      # We train the model using every period before our period of interest
      train_dat <- filter(dat_to_use, week_num < j)
      
      # Here we train the actual model
      curr_week_models <- build_models(train_dat, curr_spec_form)
      
      # And add it to all the other weeks for this specification
      curr_spec_models[[as.character(j)]] <- curr_week_models
      
    }
    
    # Add the current model to the overall list
    final_models[[curr_lhs]][[curr_spec_name]] <- curr_spec_models
  }
}



#########################################
# 3A. Prediction exercise A - Baseline #
#########################################

for(i in 1:length(final_models)){
  
  final_rmses <- NULL
  curr_models <- final_models[[i]]
  curr_lhs <- names(final_models)[i]
  
  for(j in seq(17, 43, by=2)){
      
      # Skip weeks where there are no deaths models
      if(grepl("deaths", curr_lhs)){
        if(j == 19 | j %% 4 != 3){next}
      }
      
      # First get the week of interest
      test_dat <- filter(regress_dat_pred, week_num == j)
      
      # Predict using the WITHOUT SCI model
      rmse_rf_wo_sci <- get_rmse(
        test_dat[[curr_lhs]],
        make_preds(curr_models, "baseline", j, "rf", test_dat)
      )
      
      # Predict using the WITH SCI model
      rmse_rf_w_sci <- get_rmse(
        test_dat[[curr_lhs]],
        make_preds(curr_models, "baseline_sci", j, "rf", test_dat)
      )
      
      # Add to a final tibble of RMSEs
      final_rmses <-
        add_to_tibble(final_rmses, j, rmse_rf_wo_sci, rmse_rf_w_sci)
    }
  
  write_csv(final_rmses, str_interp("../_output/tables/raw/pred_exercise1_${curr_lhs}.csv"))
}



####################################################
# 3B. Prediction exercise B - Best available model #
####################################################

for(i in 1:length(final_models)){
  
  final_rmses <- NULL
  curr_models <- final_models[[i]]
  curr_lhs <- names(final_models)[i]

  for(j in seq(17, 43, by=2)){
    
    # Skip weeks where there are no deaths models
    if(grepl("deaths", curr_lhs)){
      if(j == 19 | j %% 4 != 3){next}
    }
    
    # First get the week of interest
    dat_to_use <- filter(regress_dat_pred, week_num == j)
    
    dat_to_use <- dat_to_use %>% 
      mutate(
        has_lex = !is.na(log_l1_chg_lwc_10k) & !is.na(log_l2_chg_lwc_10k),
        has_google = !is.na(l1_pchg_gogl_fever) & !is.na(l1_pchg_gogl_cough) & !is.na(l1_pchg_gogl_fatigue)) 
      
    # Then make samples based on data availability
    test_dat_baseline <- dat_to_use %>% 
      filter(!has_lex & !has_google)
    
    test_dat_lex <- dat_to_use %>% 
      filter(has_lex & !has_google)
    
    test_dat_google <- dat_to_use %>% 
      filter(!has_lex & has_google)
    
    test_dat_lex_google <- dat_to_use %>% 
      filter(has_lex, has_google)
    
    # Predict using the WITHOUT SCI models
    test_dat_baseline$rf_pred_wo_sci <- make_preds(curr_models, "baseline", j, "rf", test_dat_baseline)
    test_dat_lex$rf_pred_wo_sci <- make_preds(curr_models, "baseline_lex", j, "rf", test_dat_lex)
    test_dat_google$rf_pred_wo_sci <- make_preds(curr_models, "baseline_google", j, "rf", test_dat_google)
    test_dat_lex_google$rf_pred_wo_sci <- make_preds(curr_models, "baseline_lex_google", j, "rf", test_dat_lex_google)
    
    # Predict using the WITH SCI models
    test_dat_baseline$rf_pred_w_sci <- make_preds(curr_models, "baseline_sci", j, "rf", test_dat_baseline)
    test_dat_lex$rf_pred_w_sci <- make_preds(curr_models, "baseline_lex_sci", j, "rf", test_dat_lex)
    test_dat_google$rf_pred_w_sci <- make_preds(curr_models, "baseline_google_sci", j, "rf", test_dat_google)
    test_dat_lex_google$rf_pred_w_sci <- make_preds(curr_models, "baseline_lex_google_sci", j, "rf", test_dat_lex_google)
    
    test_dat <- bind_rows(
      test_dat_baseline,
      test_dat_lex,
      test_dat_google,
      test_dat_lex_google
    )
    
    # Calculate the RMSEs
    rmse_rf_wo_sci <- get_rmse(test_dat[[curr_lhs]], test_dat$rf_pred_wo_sci)
    rmse_rf_w_sci <- get_rmse(test_dat[[curr_lhs]], test_dat$rf_pred_w_sci)
    
    # Add to a final tibble of RMSEs
    final_rmses <-
      add_to_tibble(final_rmses, j, rmse_rf_wo_sci, rmse_rf_w_sci)
  }
  
  write_csv(final_rmses, str_interp("../_output/tables/raw/pred_exercise2_${curr_lhs}.csv"))
}



###################################################
# 3C. Prediction exercise C - Only complete cases #
###################################################

for(i in 1:length(final_models)){
  
  final_rmses <- NULL
  curr_models <- final_models[[i]]
  curr_lhs <- names(final_models)[i]
  
  for(j in seq(17, 43, by=2)){
    
    # Skip weeks where there are no deaths models
    if(grepl("deaths", curr_lhs)){
      if(j == 19 | j %% 4 != 3){next}
    }
    
    # Get the week of interest and only the counties with complete data
    test_dat <- regress_dat_pred %>% 
      filter(week_num == j) %>% 
      filter(!is.na(log_l1_chg_lwc_10k),
             !is.na(log_l2_chg_lwc_10k)) %>% 
      filter(!is.na(l1_pchg_gogl_fever),
             !is.na(l1_pchg_gogl_cough),
             !is.na(l1_pchg_gogl_fatigue)) 
      
    # Predict using the WITHOUT SCI models
    rmse_rf_wo_sci <- get_rmse(
      test_dat[[curr_lhs]],
      make_preds(curr_models, "baseline_lex_google", j, "rf", test_dat)
    )

    # Predict using the WITH SCI models
    rmse_rf_w_sci <- get_rmse(
      test_dat[[curr_lhs]],
      make_preds(curr_models, "baseline_lex_google_sci", j, "rf", test_dat)
    )
    
    # Add to a final tibble of RMSEs
    final_rmses <-
      add_to_tibble(final_rmses, j, rmse_rf_wo_sci, rmse_rf_w_sci)
  }
  
  write_csv(final_rmses, str_interp("../_output/tables/raw/pred_exercise3_${curr_lhs}.csv"))
}

