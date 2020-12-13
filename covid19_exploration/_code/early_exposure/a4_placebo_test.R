# Purpose: Generate "placebo" analysis to benchmark the incremental R^2 from adding SCI to Westchester
# Inputs: 
#     _input/sf12010countydistancemiles.csv
#     _input/ACS_17_5YR_DP05.csv
#     US COVID data from: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-30-2020.csv
#     SCI Data: dir.sci_dat_county
#     _input/NCHSURCodes2013.csv
# Outputs:
#     _output/placebo_test_baseline.png
#     _output/placebo_test_150mi_exclusion.png
# Date: 12/01/2020
# Steps:
#     0. Generate max distance from NY to county in NY CBSA
#     1. Prep Data
#     2. Run Placebo Analysis
#     3. Make Final Placebo Plots


library(tidyverse)
library(totalcensus)
library(lfe)
library(ggrepel)


#############################################################
### 0. Generate max distance from NY to county in NY CSA ###
#############################################################

# In the paper we cite how far counties are from NYC and Westchester within the NY-area
# CSA. Here we generate that information.

# Read in county-county distance from NBER
# https://data.nber.org/data/county-distance-database.html
county_county_dist <- read_csv("../_input/sf12010countydistancemiles.csv") %>% 
  mutate(county1=as.numeric(county1),
         county2=as.numeric(county2))

data("dict_cbsa")

ny_csa_counties <- dict_cbsa %>% 
  filter(CSA == 408) %>% 
  mutate(fips = as.numeric(paste0(STATE, COUNTY))) %>% 
  .$fips

# Max dist from NYC in "New York-Newark, NY-NJ-CT-PA" CSA
county_county_dist %>% 
  filter(
    county1 == 36061,
    county2 %in% ny_csa_counties) %>% 
  .$mi_to_county %>% 
  max

# Max dist from WESTCHESTER in "New York-Newark, NY-NJ-CT-PA" CSA
county_county_dist %>% 
  filter(
    county1 == 36119,
    county2 %in% ny_csa_counties) %>% 
  .$mi_to_county %>% 
  max


####################
### 1. Prep data ###
####################

# Here we want to construct similar data to what we made in a2_prep_data_for_binscatters.R.
# Now, however, we want to make predictions using EVERY county, isntead of just Westchester.

# Read in county pops, pulled from Census
county_pops <- read_csv("../_input/ACS_17_5YR_DP05.csv") %>% 
  select(fips = GEO.id2, label=`GEO.display-label`, pop=HC01_VC03)

# Read in COVID data
covid_dat <- 
  read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-30-2020.csv") %>% 
  filter(!is.na(FIPS)) %>% 
  mutate(FIPS = as.numeric(FIPS)) %>% 
  select(FIPS, state=Province_State, Confirmed, Deaths)

# Read in SCI data
sci_dat <- read_tsv(dir.sci_dat_county)
sci_dat <- rename(sci_dat, sci=scaled_sci) %>%
  mutate(user_loc=as.numeric(user_loc),
         fr_loc=as.numeric(fr_loc)) %>% 
  filter(user_loc <= 57000 & fr_loc <= 57000) # removes territories

# Read in rural-urban classification from NCHS
# https://www.cdc.gov/nchs/data_access/urban_rural.htm
rural_urban_classification <- read_csv("../_input/NCHSURCodes2013.csv") %>% 
  select(fips = `FIPS code`, urban_rural_code = `2013 code`)

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
  select(fips, med_hhinc2016, poor_share2010, popdensity2010)

# Merge SCI to distance
# This large double merge takes a few seconds
dat1 <- sci_dat %>% 
  left_join(county_county_dist, by=c("user_loc"="county1", "fr_loc"="county2")) %>% 
  # Hopkins puts all NYC cases in Manhattan county.
  # To match this in the SCI data we will average the NYC counties into one observation.
  # In the final binscatter we will exclude cases within 50 miles, so this will not matter.
  mutate(user_loc = if_else(user_loc %in% c(36005, 36047, 36081, 36085), 36061, user_loc)) %>% 
  mutate(fr_loc = if_else(fr_loc %in% c(36005, 36047, 36081, 36085), 36061, fr_loc)) %>% 
  group_by(user_loc, fr_loc) %>% 
  summarise(sci = mean(sci),
            mi_to_county = mean(mi_to_county)) %>% 
  ungroup



###############################
### 2. Run Placebo Analysis ###
###############################

# For our placebo comparison, we will use every county with pop over 50k
counties_to_use <- filter(county_pops, pop >= 50000, !fips %in% c(36005, 36047, 36081, 36085))$fips

# In our baseline regression we excluded counties within 50 miles. Here we try that, but also
# robustness check with another distance.
distances_to_use <- c(50, 150)

# The list 'rsq_dat' will store the final results
rsq_dat <- vector("list", length(distances_to_use))
names(rsq_dat) <- as.character(distances_to_use)

for(i in 1:length(counties_to_use)){
  
  print(paste("Running placebo analysis for county", i, "of", length(counties_to_use)))
  
  # Filter to SCI with Westchester
  dat_curr <- dat1 %>% 
    filter(fr_loc == counties_to_use[i]) %>% 
    select(-fr_loc) %>% 
    rename(dist = mi_to_county)
  
  # Add all other covariates
  dat_curr <- dat_curr %>% 
    rename(county_fips = user_loc) %>% 
    left_join(county_pops, by=c("county_fips"="fips")) %>% 
    left_join(covid_dat, by=c("county_fips" = "FIPS")) %>% 
    left_join(rural_urban_classification, by=c("county_fips"="fips")) %>% 
    left_join(oi_covariates, by=c("county_fips"="fips")) %>% 
    mutate(cases_per_10k = (Confirmed/pop)*10000)
  
  # Final cleaning
  reg_dat <- dat_curr %>% 
    filter(county_fips != counties_to_use[i]) %>% 
    filter(!is.na(dist)) %>% 
    mutate(dist_group = ntile(dist, 100)) %>% 
    mutate(
      log_dist = log(dist),
      log_sci = log(sci),
      log_sci_2 = log_sci^2)
  
  # Produce the final regression data for all distances
  for(j in names(rsq_dat)){
    
    j_numeric <- as.numeric(j)
    
    model_1 <-
      felm(cases_per_10k ~ med_hhinc2016 + popdensity2010 | urban_rural_code + dist_group | 0 | 0, data = filter(reg_dat, dist > j_numeric))
    
    model_2 <-
      felm(cases_per_10k ~ log_sci + log_sci_2 + med_hhinc2016 + popdensity2010 | urban_rural_code + dist_group | 0 | 0, data = filter(reg_dat, dist > j_numeric))
    
    curr_rsq_dat <- tibble(
      county_fips = counties_to_use[i],
      incremental_r.squared = summary(model_2)$r.squared - summary(model_1)$r.squared
    )
    
    if(is.null(rsq_dat[[j]])){
      rsq_dat[[j]] <- curr_rsq_dat
    }
    else{
      rsq_dat[[j]] <- bind_rows(rsq_dat[[j]], curr_rsq_dat)
    }
  }
}



###################################
### 3. Make Final Placebo Plots ###
###################################

# Make clean county labels for this last plot
county_labels <- read_csv("../_input/NCHSURCodes2013.csv") %>% 
  mutate(county_label = paste(str_remove(`County name`, " County"), `State Abr.`, sep =", ")) %>% 
  select(county_fips=`FIPS code`, county_label)


# A little helper function to make two versions of this last placebo plot
make_final_placebo_plot <- function(dat, label){
  
  top10 <- dat %>% 
    left_join(county_labels, by=c("county_fips")) %>% 
    arrange(desc(incremental_r.squared)) %>% 
    head(10)
  
  ggplot() +
    geom_histogram(dat = dat, aes(incremental_r.squared), bins = 100) +
    theme_bw() +
    labs(y="Count", x="Incremental R-Squared") +
    geom_text_repel(data=top10, 
                    aes(x=incremental_r.squared, y=0, label=county_label),
                    segment.size = 0.05,
                    size = 2.75,
                    nudge_y = 100,
                    force = 35)
  
  ggsave(str_interp("../_output/placebo_test_${label}.png"),
         width = 5, height = 4, units = "in", last_plot())
}

set.seed(11) # to get reproducible ggrepel label positions

make_final_placebo_plot(rsq_dat[["50"]], "baseline")
make_final_placebo_plot(rsq_dat[["150"]], "150mi_exclusion")
