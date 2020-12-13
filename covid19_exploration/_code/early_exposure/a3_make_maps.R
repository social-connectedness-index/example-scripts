# Purpose: Make the SCI to intital hotspots and early COVID maps
# Inputs: 
#     SCI Data: dir.sci_dat_county
#     _input/ACS_17_5YR_DP05.csv
#     US COVID data from: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-30-2020.csv
#     _intermediate/lodi_sci_covariates.csv
#     SCI Data: dir.sci_dat_gadm1_nuts3_counties
# Outputs: 
#     _output/sci_from_westchester.jpg
#     _output/us_cases.jpg
#     _output/sci_from_lodi.jpg
#     _output/italy_cases.jpg
# Date: 07/30/2020
# Steps:
#     1. Generate Westchester maps
#     2. Generate Lodi maps

library(tigris)
library(sf)
library(tidyverse)

################################
# 1. Generate Westchester maps #
################################

# Get the maps from the tigris package
counties_map <- counties(cb = TRUE) %>%
  st_as_sf() %>%
  st_transform("+init=epsg:2163") %>%
  mutate(fips = as.numeric(paste0(STATEFP, COUNTYFP)))

states_map <- states(cb = TRUE) %>%
  st_as_sf() %>%
  st_transform("+init=epsg:2163")

# Read in county SCI data.
# We don't use other data set because it excludes NYC counties
# other than Manhattan, which can be seen on the map.
sci_dat <- read_tsv(dir.sci_dat_county)
county_dat <- rename(sci_dat, sci=scaled_sci) %>%
  filter(user_loc == "36119") %>%
  mutate(county_fips = as.numeric(fr_loc)) %>% 
  select(-user_loc, -fr_loc)

# Merge with shape files
dat_map <- 
  right_join(county_dat,
             counties_map,
             by=c("county_fips"="fips")) %>% 
  st_as_sf

# Create clean buckets for these levels
dat_map <- dat_map %>% 
  mutate(log_sci = log(sci)) %>% 
  mutate(log_sci_bkt = case_when(
    log_sci < 7 ~ "< 7",
    log_sci < 7.5 ~ "7 - 7.5",
    log_sci < 8 ~ "7.5 - 8",
    log_sci < 9 ~ "8 - 9",
    log_sci < 10.5 ~ "9 - 10.5",
    log_sci >= 10.5 ~ "10.5+")) %>% 
  mutate(log_sci_bkt = factor(log_sci_bkt, levels=c("< 7", "7 - 7.5", "7.5 - 8",
                                                    "8 - 9", "9 - 10.5", "10.5+")))

# Get the map of the region you are in
curr_region_outline <- dat_map %>% 
  filter(county_fips == 36119)

# Plot the US case data
ggplot(filter(dat_map, !is.na(log_sci_bkt))) +
  geom_sf(aes(fill = log_sci_bkt), colour="#ADADAD", lwd=0) +
  geom_sf(data=states_map, fill="transparent", colour="#A1A1A1", size=0.2) +
  geom_sf(data=curr_region_outline, fill="#A00000", colour="#A00000", size=0.2) +
  labs(fill = "SCI") +
  theme_void() +
  scale_fill_brewer(palette = "GnBu", drop=FALSE) +
  theme(legend.title = element_blank(), 
        legend.text  = element_text(size = 6.5),
        legend.key.size = unit(0.25, "lines"),
        legend.position = "bottom", legend.box = "horizontal") +
  guides(fill = guide_legend(nrow = 1, title.hjust = 0.5)) +
  coord_sf(xlim = c(-2200000, 2700000), ylim = c(-2200000, 850000), expand = FALSE) 

ggsave(paste0("../_output/sci_from_westchester.jpg"),
       width = 3.25, height = 1.9, units = "in", dpi = 800, last_plot())

# Read in population and cases data
county_pops <- read_csv("../_input/ACS_17_5YR_DP05.csv") %>% 
  select(fips = GEO.id2, label=`GEO.display-label`, pop=HC01_VC03)

covid_dat <- 
  read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-30-2020.csv") %>% 
  filter(!is.na(FIPS)) %>% 
  mutate(fips = as.numeric(FIPS)) %>% 
  select(fips, state=Province_State, Confirmed, Deaths)

county_dat <- covid_dat %>% 
  left_join(county_pops, by=c("fips")) %>% 
  mutate(cases_per_10k = (Confirmed/pop)*10000)

# Merge with shape files
dat_map <- 
  right_join(county_dat,
             counties_map,
             by=c("fips")) %>% 
  st_as_sf

# Create clean buckets for these levels
dat_map <- dat_map %>% 
  mutate(cases_per_10k_bkt = case_when(
    cases_per_10k < 1 ~ "< 1",
    cases_per_10k < 1.5 ~ "1 - 1.5",
    cases_per_10k < 3 ~ "1.5 - 3",
    cases_per_10k < 6 ~ "3 - 6",
    cases_per_10k < 10 ~ "6 - 10",
    cases_per_10k >= 10 ~ "10+")) %>%
  mutate(cases_per_10k_bkt = factor(cases_per_10k_bkt, levels=c("< 1", "1 - 1.5", "1.5 - 3", "3 - 6",
                                                                "6 - 10", "10+")))

# Plot the Westchester SCI data
ggplot(filter(dat_map, !is.na(cases_per_10k_bkt))) +
  geom_sf(aes(fill = cases_per_10k_bkt), colour="#ADADAD", lwd=0) +
  geom_sf(data=states_map, fill="transparent", colour="#A1A1A1", size=0.2) +
  labs(fill = "SCI") +
  theme_void() +
  scale_fill_brewer(palette = "YlOrRd", drop=FALSE) +
  theme(legend.title = element_blank(), 
        legend.text  = element_text(size = 6.5),
        legend.key.size = unit(0.25, "lines"),
        legend.position = "bottom", legend.box = "horizontal") +
  guides(fill = guide_legend(nrow = 1, title.hjust = 0.5)) +
  coord_sf(xlim = c(-2200000, 2700000), ylim = c(-2200000, 850000), expand = FALSE) 

ggsave(paste0("../_output/us_cases.jpg"),
       width = 3.25, height = 1.9, units = "in", dpi = 800, last_plot())


#########################
# 2. Generate Lodi maps #
#########################

# Read in it dat
it_dat <- read_csv("../_intermediate/lodi_sci_covariates.csv")

# Read in the detailed GADM shapes
shapes_in <- readRDS(dir.gadm1_nuts3_counties_shapes) %>% 
  filter(substr(key, 1, 2) == "IT") %>% 
  filter(substr(key, 1, 2) == "IT")

# Use the average SCI for Sardinia.
# (See the long note on why we do this in the prep data script)
shapes_clean <- shapes_in %>% 
  mutate(key = as.character(key)) %>% 
  mutate(key = if_else(substr(key,1,4) == "ITG2", "ITG2", key)) %>% 
  group_by(key) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  ungroup

# The shapes we use are rather detailed. To generate these maps
# we simplify the shape files.
shapes_simple <- st_simplify(shapes_clean, dTolerance = .015)

# Join the shapes to the data
final_it_dat <- 
  it_dat %>%
  inner_join(shapes_simple, by=c("nuts3_code"="key")) %>% 
  st_as_sf()

# The measures are the percentiles of all connections
ptile_50 <- quantile(final_it_dat$sci, .5)
ptile_60 <- quantile(final_it_dat$sci, .6)
ptile_70 <- quantile(final_it_dat$sci, .7)
ptile_80 <- quantile(final_it_dat$sci, .8)
ptile_90 <- quantile(final_it_dat$sci, .9)

# Create clean buckets for these levels
final_it_dat <- final_it_dat %>%
  mutate(cases_bkt = case_when(
    cases_per_10k < 5 ~ "< 5",
    cases_per_10k < 10 ~ "5 - 10",
    cases_per_10k < 20 ~ "10 - 20",
    cases_per_10k < 30 ~ "20 - 30",
    cases_per_10k < 40 ~ "30 - 40",
    cases_per_10k >= 40 ~ "40+")) %>% 
  mutate(cases_bkt= factor(cases_bkt, levels=c("< 5", "5 - 10", "10 - 20", "20 - 30", "30 - 40", "40+"))) %>% 
  mutate(sci_bkt = case_when(
    sci < ptile_50 ~ "< Province 50th Pctile",
    sci < ptile_60 ~ "50-60th Pctile",
    sci < ptile_70 ~ "60-70th Pctile",
    sci < ptile_80 ~ "70-80th Pctile",
    sci < ptile_90 ~ "80-90th Pctile",
    sci >= ptile_90 ~ ">= 90th Pctile")) %>% 
  mutate(sci_bkt = factor(sci_bkt, levels=c("< Province 50th Pctile", "50-60th Pctile", "60-70th Pctile",
                                            "70-80th Pctile", "80-90th Pctile", ">= 90th Pctile")))

# Get the map of Lodi
curr_region_outline <- final_it_dat %>% 
  filter(nuts3_code == "ITC49")

# Plot the Lodi SCI data
ggplot(final_it_dat) +
  geom_sf(aes(fill = sci_bkt), colour="#e0e0e0", size=0.1) +
  geom_sf(data=curr_region_outline, fill="#A00000", colour="#A00000", lwd=0) +
  labs(fill = "Cases per 10k people") +
  theme_void() +
  scale_fill_brewer(palette = "GnBu", drop=FALSE) +
  theme(legend.title = element_blank(), 
        legend.text  = element_text(size = 9),
        legend.key.size = unit(1, "lines"))

ggsave("../_output/sci_from_lodi.jpg",
       width = 5, height = 4, units = "in", dpi = 800, last_plot())

# Plot the Italy case data
ggplot(final_it_dat) +
  geom_sf(aes(fill = cases_bkt), colour="#e0e0e0", size=0.1) +
  labs(fill = "Cases per 10k people") +
  theme_void() +
  scale_fill_brewer(palette = "YlOrRd", drop=FALSE) +
  theme(legend.title = element_blank(), 
        legend.text  = element_text(size = 9),
        legend.key.size = unit(1, "lines"))

ggsave("../_output/italy_cases.jpg",
       width = 5, height = 4, units = "in", dpi = 800, last_plot())