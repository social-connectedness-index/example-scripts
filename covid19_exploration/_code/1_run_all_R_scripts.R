# Purpose: This driver file is a simple interface to run
# all of the R files in this folder.

# Date: 12/01/2020

require(tidyverse)
require(sp)
require(sf)
require(tigris)
require(totalcensus)
require(lfe)
require(ggrepel)
require(lubridate)
require(randomForest)

############################################################
#### A. Make the results for earliest hotspots in US/IT ####
############################################################

###### FILL THESE LINES BEFORE RUNNING ###### 

# The path to your county-county SCI data from: https://data.humdata.org/dataset/social-connectedness-index
dir.sci_dat_county <- ""

# The path to your gadm1_nuts3_counties SCI data from: https://data.humdata.org/dataset/social-connectedness-index
dir.sci_dat_gadm1_nuts3_counties <- ""

# The path to your gadm1_nuts3_counties shape file from: https://github.com/social-connectedness-index/example-scripts/tree/master/gadm_based_shapefiles/rds_format
dir.gadm1_nuts3_counties_shapes <- ""

################################################


# A1: Generate the distance between the centroids of all Italian provinces
source("early_exposure/a1_gen_italy_distances.R")

# A2: Prepare COVID and SCI data for binscatters and maps
source("early_exposure/a2_prep_data_for_binscatters.R")

# A3: Make the SCI to intital hotspots and early COVID maps
source("early_exposure/a3_make_maps.R")

# A4: Run the placebo test (time intensive!)
source("early_exposure/a4_placebo_test.R")

# A5: Make the out-of-sample hotspot test -- train on Lodi, test on Westchester
source("early_exposure/a5_hotspot_out_of_sample.R")

# A6 is a Stata file run from run_all_stata_scripts.do

rm(list=ls())



############################################
#### B. Make time series results for US ####
############################################

###### FILL THESE LINES BEFORE RUNNING ###### 

# The path to your county-county SCI data from: https://data.humdata.org/dataset/social-connectedness-index
dir.sci_dat_county <- ""

# The path to a folder that stores LEX data from: https://github.com/COVIDExposureIndices/COVIDExposureIndices
# The dates used are every other Monday from 2020-02-03 to 2020-08-17, and every other Saturday from 2020-08-29 to 2020-10-24
dir.lex_dat <- ""

################################################


# B1: Make the two case weighted measures of interest (RAM intensive!)
source("time_series/b1_make_weighted_measures.R")

# B2: Use SCI data to estimate the share of each county's friends within X miles
source("time_series/b2_make_share_of_frnds_within_Xmi.R")

# B3: Build the Google symptom search based weekly time series data
source("time_series/b3_make_gtrends_dat.R")

# B4: Build the regression table for the time series analyses
source("time_series/b4_build_time_series_regress_dat.R")

# B5: Conduct the prediction exercise using the weighted measures (extremely RAM /time intensive!)
source("time_series/b5_prediction_exercise.R")

# B6 is a Stata file run from run_all_stata_scripts.do

# The file "bX_prediction_exercise_helper_funs.R" contains a set 
# of helper functions that are used within B5