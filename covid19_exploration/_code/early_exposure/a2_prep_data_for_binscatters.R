# Purpose: Prepare COVID and SCI data for binscatters and maps
# Inputs: 
#     _input/ACS_17_5YR_DP05.csv
#     US COVID data from: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-30-2020.csv
#     SCI Data: dir.sci_dat_county
#     _input/sf12010countydistancemiles.csv
#     _input/NCHSURCodes2013.csv
#     SCI Data: dir.sci_dat_gadm1_nuts3_counties
#     _output/it_distance_table.csv (NOTE: THIS CROSSWALK MAPS ALL OF SARDINIA TO THE REGION LEVEL)
#     _input/italy_xw.csv
#     Italy COVID data from: https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province-20200330.csv
# Outputs:
#     _intermediate/westchester_sci_covariates.csv
#     _intermediate/lodi_sci_covariates.csv"
# Date: 12/01/2020

# Steps:
#     1. Generate Westchester data
#     2. Generate Lodi data

library(tidyverse)

########################################
##### 1. Generate Westchester data #####
########################################

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

# Read in county-county distance from NBER
# https://data.nber.org/data/county-distance-database.html
county_county_dist <- read_csv("../_input/sf12010countydistancemiles.csv") %>% 
  mutate(county1=as.numeric(county1),
         county2=as.numeric(county2))

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

# Filter to SCI with Westchester
dat_westchester <- dat1 %>% 
      filter(fr_loc == 36119) %>% 
      select(-fr_loc) %>% 
      rename(dist = mi_to_county)

# Add all other covariates
dat_westchester_final <- dat_westchester %>% 
  rename(county_fips = user_loc) %>% 
  left_join(county_pops, by=c("county_fips"="fips")) %>% 
  left_join(covid_dat, by=c("county_fips" = "FIPS")) %>% 
  left_join(rural_urban_classification, by=c("county_fips"="fips")) %>% 
  left_join(oi_covariates, by=c("county_fips"="fips")) %>% 
  mutate(cases_per_10k = (Confirmed/pop)*10000)

# Save the data
write_csv(dat_westchester_final, "../_intermediate/westchester_sci_covariates.csv")


#################################
##### 2. Generate Lodi data #####
#################################

# Throughout this section we pull Sardinia at the region level.
# This is because the SCI and covariate data we use are at NUTS3
# which is defined differently in Sardinia than the current
# Italian provinces (the level of the COVID-19 data).
# This is because of a recent change in Italian province definitions.
# NUTS3 and provinces will match again in the NUTS3 2021 data.

# Read in the populations 
pop <- read.table("../_input/eurostat/demo_r_pjangrp3.tsv", sep = '\t', header = TRUE) %>% 
  separate(sex.unit.age.geo.time, c("sex","unit","age","geo"), ",") %>% 
  filter(sex == "T", age == "TOTAL") %>% 
  select(geo, X2018) %>% 
  # Combine Sardinia
  filter(str_length(geo) == 5 | substr(geo, 1, 4) == "ITG2") %>%
  filter(!(str_length(geo) == 5 & substr(geo, 1, 4) == "ITG2")) %>%
  filter(substr(geo, 1, 2) == "IT") %>% 
  mutate(X2018 = gsub("[^0-9\\.]", "", X2018)) %>%  # Remove comment codes
  mutate(pop_2018 = as.numeric(X2018)) %>% 
  select(-X2018) %>% 
  as_tibble()

# Read in the population densities
density <- read.table("../_input/eurostat/demo_r_d3dens.tsv", sep = '\t', header = TRUE) %>% 
  separate(unit.geo.time, c("unit","geo"), ",") %>% 
  # Combine Sardinia
  filter(str_length(geo) == 5 | substr(geo, 1, 4) == "ITG2") %>%
  filter(!(str_length(geo) == 5 & substr(geo, 1, 4) == "ITG2")) %>% 
  filter(substr(geo, 1, 2) == "IT") %>% 
  mutate(X2018 = gsub("[^0-9\\.]", "", X2018)) %>%  # Remove comment codes
  mutate(pop_per_km = as.numeric(X2018)) %>% 
  select(geo, pop_per_km) %>% 
  as_tibble()

# Read in the GDP per person
gdp <- read.table("../_input/eurostat/nama_10r_3gdp.tsv", sep = '\t', header = TRUE) %>% 
  separate(unit.geo.time, c("unit","geo"), ",") %>% 
  filter(unit == "EUR_HAB") %>% 
  # Combine Sardinia
  filter(str_length(geo) == 5 | substr(geo, 1, 4) == "ITG2") %>%
  filter(!(str_length(geo) == 5 & substr(geo, 1, 4) == "ITG2")) %>%
  filter(substr(geo, 1, 2) == "IT") %>%
  mutate(X2017 = gsub("[^0-9\\.]", "", X2017)) %>%  # Remove comment codes
  mutate(gdp_per_hab = as.numeric(X2017)) %>% 
  select(geo, gdp_per_hab) %>% 
  as_tibble()

# Merge all Eurostat together
all_eurostat <-
  pop %>% 
  inner_join(density, by="geo") %>% 
  inner_join(gdp, by="geo")

# Read in the Italy distance data
it_distance <- read_csv("../_intermediate/it_distance_table.csv") %>% 
  # Average distances for Sardinia
  mutate(from_nuts3 = if_else(substr(from_nuts3, 1, 4) == "ITG2", "ITG2", from_nuts3)) %>% 
  mutate(to_nuts3 = if_else(substr(to_nuts3, 1, 4) == "ITG2", "ITG2", to_nuts3)) %>% 
  group_by(from_nuts3, to_nuts3) %>% 
  summarise(dist = mean(dist)) %>% 
  ungroup

# Filter distance data to Lodi
dist_to_lodi <- it_distance %>% 
  filter(from_nuts3 == "ITC49") %>% 
  select(-from_nuts3)

# Read in the Italy COVID data
it_dat <- read_csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province-20200330.csv")

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

# Combine Eurostat and covid
eurostat_covid <- 
  all_eurostat %>% 
  inner_join(it_covid_nuts3, by=c("geo"="nuts3_code"))

# Read in SCI data and filter to Italy
sci_dat <- read_tsv(dir.sci_dat_gadm1_nuts3_counties)
sci_dat <- rename(sci_dat, sci=scaled_sci)
it_sci_dat <- sci_dat %>%
  filter(substr(user_loc, 1, 2) == "IT") %>%
  filter(substr(fr_loc, 1, 2) == "IT")

# Make final data
final_lodi_dat <- 
  filter(it_sci_dat, user_loc == "ITC49") %>%
  mutate(fr_loc = if_else(substr(fr_loc, 1, 4) == "ITG2", "ITG2", fr_loc)) %>% 
  group_by(fr_loc) %>% 
  # For Sardinia, we do use mean SCI of the NUTS3 regions
  summarise(sci = mean(sci)) %>% 
  inner_join(eurostat_covid, by=c("fr_loc"="geo")) %>% 
  inner_join(dist_to_lodi, by=c("fr_loc"="to_nuts3")) %>% 
  mutate(cases_per_10k = (total_cases/pop_2018)*10000) %>% 
  rename(nuts3_code=fr_loc)

write_csv(final_lodi_dat, "../_intermediate/lodi_sci_covariates.csv")