# This file includes a set of helper functions for b3_prediction_exercise.R

# Purpose: A helper function to build models. You could change the types of models
#           or model hyper-parameters by modifying this function.
# Inputs: Training data, the model formula
# Outputs: A list of two trained models (1=GLM, 2=RandomForest)
build_models <- function(train_dat, model_form){
  
  # GLM-based predictions
  glm_model <- glm(as.formula(model_form), data=train_dat)
  
  # Random Forest-based predictions
  # We use all the 'out of the box' tuning parameters (ntree = 500, mtry=x/3)
  # The default nodesize will be extremely small for bigger datasets. For tractability
  # we instead set it to 0.1% of the training data size
  rf_model <- randomForest(as.formula(model_form), nodesize=ceiling(nrow(train_dat)/1000), data=train_dat)
  
  # Add the current week to the list for this model 
  curr_week_models <- list()
  curr_week_models[["glm"]] <- glm_model
  curr_week_models[["rf"]] <- rf_model
  
  curr_week_models
}


# Purpose: A helper function to make predictions.
# Inputs:
#     models = A list of models indexed by specification name, week, model type (glm or rf)
#     spec_name = The specification name
#     model_week = The week for which this models is making predictions for
#     model_type = "glm" or "rf" (for random forest)
#     dat_to_use = The test data to use
# Outputs: A list of two trained models (1=GLM, 2=RandomForest)
make_preds <- function(models, spec_name, model_week, model_type, dat_to_use){
  
  preds <- NULL
  
  if(model_type == "glm"){
    preds <- predict(models[[spec_name]][[as.character(model_week)]][["glm"]], dat_to_use, type="response")
  }
  
  if(model_type == "rf"){
    preds <- predict(models[[spec_name]][[as.character(model_week)]][["rf"]], dat_to_use)
  }
  
  preds
}

# Purpose: A very simple helper function to calculate RMSEs using specified vectors
# Inputs: A vector of 'truth' and a vector of 'predictions'
# Outputs: A numeric that is the RMSE
get_rmse <- function(truth, pred, dat){
  to_return <- sqrt(mean((truth - pred)^2))
}


# Purpose: A helper function to construt the final tibble for output to csv
# Inputs:
#     final_tibble = The final tibble you want to add to. If NULL will create new tibble.
#     week_num = The week of year
#     rmse_rf_wo_sci = The RMSE from a RF model w/o an SCI-based predictor
#     rmse_rf_w_sci = The RMSE from a RF model w/ an SCI-based predictor
# Outputs: A tibble with the new week appended or, if the final_tibble passed in
#           is NULL, a new tibble with one week
add_to_tibble <- function(final_tibble, week_num,
                          rmse_rf_wo_sci, rmse_rf_w_sci){
  
  curr_tibble <- tibble(
    week_num = week_num,
    rf_wo_sci = rmse_rf_wo_sci,
    rf_w_sci = rmse_rf_w_sci,
    rf_diff_from_sci = rf_w_sci - rf_wo_sci
  )
  
  if(is.null(final_tibble)){
    final_tibble <- curr_tibble
  }
  else{
    final_tibble <- bind_rows(final_tibble, curr_tibble)
  }
  
  final_tibble
}
