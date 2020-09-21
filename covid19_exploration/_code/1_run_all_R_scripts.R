# Purpose: This driver file is a simple interface to run
# all of the R files in this folder.

# Date: 07/30/2020

require(tidyverse)
require(sp)
require(sf)
require(tigris)
require(lubridate)
require(randomForest)

############################################################
#### A. Make the results for earliest hotspots in US/IT ####
############################################################

### FILL THESE LINES BEFORE RUNNING
dir.sci_dat_county <- ""
dir.sci_dat_gadm1_nuts3_counties <- ""

# A1: Generate the distance between the centroids of all Italian provinces
source("early_exposure/a1_gen_italy_distances.R")

# A2: Prepare COVID and SCI data for binscatters and maps
source("early_exposure/a2_prep_data_for_binscatters.R")

# A3: Make the SCI to intital hotspots and early COVID maps
source("early_exposure/a3_make_maps.R")

# A4 is a Stata file run from run_all_stata_scripts.do

rm(list=ls())

############################################
#### B. Make time series results for US ####
############################################

### FILL THIS LINE BEFORE RUNNING
dir.sci_dat_county <- ""

# B1: Make the two case weighted measures of interest (RAM intensive!)
source("time_series/b1_make_weighted_measures.R")

# B2: Build the regression table for the time series analyses
source("time_series/b2_build_time_series_regress_dat.R")

# B3: Conduct the prediction exercise using the weighted measures (RAM intensive!)
source("time_series/b3_prediction_exercise.R")

# B4 is a Stata file run from run_all_stata_scripts.do