// Purpose: Make tables with time series regressions
// Inputs:
//		_intermediate\time_series_regress_dat.csv
// Outputs:
//		_output\tables\raw\social_proximity_to_cases_pooled.xml
//		_output\tables\raw\social_proximity_to_deaths_pooled.xml
// 		_output\tables\raw\pooled_appendix_other_measures.xml
//		_output\tables\raw\social_proximity_to_cases_by_week.xml
// Date: 12/01/2020
// Steps:
//		1. Generate 'pooled' regression table
//		2. Generate regression table by time period

clear
set more off

////////////////////////////////////////////													
// 1. Generate 'pooled' regression table //
///////////////////////////////////////////

import delimited "../_intermediate/time_series_regress_dat.csv", clear

// Make percentiles of median household income and pop density
egen med_hhinc_group = cut(med_hhinc2016_10k), group(100)
egen popdensity_group = cut(popdensity2010_10k), group(100)

encode province_state, gen(state_clean)

// xml_tab has a very annoying variable character limit, we shorten here to avoid problems
rename (log_chg_deaths_10k_4wk log_l2_chg_deaths_10k_4wk log_l4_chg_deaths_10k_4wk log_l2_chg_swd_10k_4wk log_l4_chg_swd_10k_4wk log_l2_chg_dwd_10k_4wk log_l4_chg_dwd_10k_4wk) (log_chg_death_10k log_l2_chg_death_10k log_l4_chg_death_10k log_l2_chg_swd_10k log_l4_chg_swd_10k log_l2_chg_dwd_10k log_l4_chg_dwd_10k)

// Destring case measures
destring(log_chg_cases_10k log_l1_chg_cases_10k log_l2_chg_cases_10k log_l1_chg_swc_10k log_l2_chg_swc_10k log_l1_chg_dwc_10k log_l2_chg_dwc_10k log_l1_chg_lwc_10k log_l2_chg_lwc_10k), replace force

// Destring death measures
destring(log_chg_death_10k log_l2_chg_death_10k log_l4_chg_death_10k log_l2_chg_swd_10k log_l4_chg_swd_10k log_l2_chg_dwd_10k log_l4_chg_dwd_10k log_l2_chg_lwd_10k log_l4_chg_lwd_10k), replace force

// Destring G-trends measures
destring(pchg_gogl_fever l1_pchg_gogl_fever pchg_gogl_cough l1_pchg_gogl_cough pchg_gogl_fatigue l1_pchg_gogl_fatigue), replace force

// We need 4 weeks of lagged data for these regressions
keep if week_num >= 15

/// Pooling all time periods (but using FEs interacted with month), calculate cases per 10k change
eststo clear

eststo panel1: reghdfe log_chg_cases_10k log_l1_chg_swc_10k log_l2_chg_swc_10k log_l1_chg_cases_10k log_l2_chg_cases_10k, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group) vce(cluster i.week#state_clean)
estadd ysumm

eststo panel2: reghdfe log_chg_cases_10k log_l1_chg_swc_10k log_l2_chg_swc_10k log_l1_chg_cases_10k log_l2_chg_cases_10k, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group i.week#state_clean) vce(cluster i.week#state_clean)
estadd ysumm

eststo panel3: reghdfe log_chg_cases_10k share_within50 share_within150 log_l1_chg_cases_10k log_l2_chg_cases_10k, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group) vce(cluster i.week#state_clean)
estadd ysumm

eststo panel4: reghdfe log_chg_cases_10k share_within50 share_within150 log_l1_chg_cases_10k log_l2_chg_cases_10k, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group i.week#state_clean) vce(cluster i.week#state_clean)
estadd ysumm

eststo panel5: reghdfe log_chg_cases_10k log_l1_chg_dwc_10k log_l2_chg_dwc_10k log_l1_chg_cases_10k log_l2_chg_cases_10k, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group) vce(cluster i.week#state_clean)
estadd ysumm

eststo panel6: reghdfe log_chg_cases_10k log_l1_chg_dwc_10k log_l2_chg_dwc_10k log_l1_chg_cases_10k log_l2_chg_cases_10k, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group i.week#state_clean) vce(cluster i.week#state_clean)
estadd ysumm

eststo panel7: reghdfe log_chg_cases_10k log_l1_chg_swc_10k log_l2_chg_swc_10k share_within50 share_within150 log_l1_chg_dwc_10k log_l2_chg_dwc_10k log_l1_chg_cases_10k log_l2_chg_cases_10k, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group) vce(cluster i.week#state_clean)
estadd ysumm

eststo panel8: reghdfe log_chg_cases_10k log_l1_chg_swc_10k log_l2_chg_swc_10k share_within50 share_within150 log_l1_chg_dwc_10k log_l2_chg_dwc_10k log_l1_chg_cases_10k log_l2_chg_cases_10k, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group i.week#state_clean) vce(cluster i.week#state_clean)
estadd ysumm

xml_tab panel*, save("../_output/tables/raw/social_proximity_to_cases_pooled") stats(N r2 ymean) replace below

eststo clear

/// Pooling all time periods (but using FEs interacted with month), calculate DEATHS per 10k change

// Each observation here is based on 4-week changes, but we have bi-weekly observations.
// To get observations every 4 weeks we filter to mod(week_num,4) == 3 -> 19, 23, 27, 31, ...
// We start at week 21 because we now need eight weeks of lagged data for these regressions

eststo clear

eststo panel1: reghdfe log_chg_death_10k log_l2_chg_swd_10k log_l4_chg_swd_10k log_l2_chg_death_10k log_l4_chg_death_10k, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group) vce(cluster i.week#state_clean), ///
if week_num >= 19 & mod(week_num,4) == 3
estadd ysumm

eststo panel2: reghdfe log_chg_death_10k log_l2_chg_swd_10k log_l4_chg_swd_10k log_l2_chg_death_10k log_l4_chg_death_10k, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group i.week#state_clean) vce(cluster i.week#state_clean), ///
if week_num >= 19 & mod(week_num,4) == 3
estadd ysumm

eststo panel3: reghdfe log_chg_death_10k share_within50 share_within150 log_l2_chg_death_10k log_l4_chg_death_10k, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group) vce(cluster i.week#state_clean), ///
if week_num >= 19 & mod(week_num,4) == 3
estadd ysumm

eststo panel4: reghdfe log_chg_death_10k share_within50 share_within150 log_l2_chg_death_10k log_l4_chg_death_10k, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group i.week#state_clean) vce(cluster i.week#state_clean), ///
if week_num >= 19 & mod(week_num,4) == 3
estadd ysumm

eststo panel5: reghdfe log_chg_death_10k log_l2_chg_dwd_10k log_l4_chg_dwd_10k log_l2_chg_death_10k log_l4_chg_death_10k, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group) vce(cluster i.week#state_clean), ///
if week_num >= 19 & mod(week_num,4) == 3
estadd ysumm

eststo panel6: reghdfe log_chg_death_10k log_l2_chg_dwd_10k log_l4_chg_dwd_10k log_l2_chg_death_10k log_l4_chg_death_10k, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group i.week#state_clean) vce(cluster i.week#state_clean), ///
if week_num >= 19 & mod(week_num,4) == 3
estadd ysumm

eststo panel7: reghdfe log_chg_death_10k log_l2_chg_swd_10k log_l4_chg_swd_10k share_within50 share_within150 log_l2_chg_dwd_10k log_l4_chg_dwd_10k log_l2_chg_death_10k log_l4_chg_death_10k, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group) vce(cluster i.week#state_clean), ///
if week_num >= 19 & mod(week_num,4) == 3
estadd ysumm

eststo panel8: reghdfe log_chg_death_10k log_l2_chg_swd_10k log_l4_chg_swd_10k share_within50 share_within150 log_l2_chg_dwd_10k log_l4_chg_dwd_10k log_l2_chg_death_10k log_l4_chg_death_10k, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group i.week#state_clean) vce(cluster i.week#state_clean), ///
if week_num >= 19 & mod(week_num,4) == 3
estadd ysumm

xml_tab panel*, save("../_output/tables/raw/social_proximity_to_deaths_pooled") stats(N r2 ymean) replace below

eststo clear

// Here we compare to other potential predictors (LEX based measures and Google symptom searches)

eststo clear

eststo panel1: reghdfe log_chg_cases_10k log_l1_chg_swc_10k log_l2_chg_swc_10k share_within50 share_within150 log_l1_chg_dwc_10k log_l2_chg_dwc_10k  log_l1_chg_cases_10k log_l2_chg_cases_10k, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group) vce(cluster i.week#state_clean)
estadd ysumm

eststo panel2: reghdfe log_chg_cases_10k log_l1_chg_swc_10k log_l2_chg_swc_10k share_within50 share_within150 log_l1_chg_dwc_10k log_l2_chg_dwc_10k log_l1_chg_cases_10k log_l2_chg_cases_10k, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group i.week#state_clean) vce(cluster i.week#state_clean)
estadd ysumm

eststo panel3: reghdfe log_chg_cases_10k log_l1_chg_swc_10k log_l2_chg_swc_10k share_within50 share_within150 log_l1_chg_dwc_10k log_l2_chg_dwc_10k log_l1_chg_cases_10k log_l2_chg_cases_10k pchg_gogl_fever pchg_gogl_cough pchg_gogl_fatigue, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group) vce(cluster i.week#state_clean)
estadd ysumm

eststo panel4: reghdfe log_chg_cases_10k log_l1_chg_swc_10k log_l2_chg_swc_10k share_within50 share_within150 log_l1_chg_dwc_10k log_l2_chg_dwc_10k log_l1_chg_cases_10k log_l2_chg_cases_10k pchg_gogl_fever pchg_gogl_cough pchg_gogl_fatigue, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group i.week#state_clean) vce(cluster i.week#state_clean)
estadd ysumm

eststo panel5: reghdfe log_chg_cases_10k log_l1_chg_swc_10k log_l2_chg_swc_10k share_within50 share_within150 log_l1_chg_dwc_10k log_l2_chg_dwc_10k log_l1_chg_cases_10k log_l2_chg_cases_10k l1_pchg_gogl_fever l1_pchg_gogl_cough l1_pchg_gogl_fatigue, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group) vce(cluster i.week#state_clean)
estadd ysumm

eststo panel6: reghdfe log_chg_cases_10k log_l1_chg_swc_10k log_l2_chg_swc_10k share_within50 share_within150 log_l1_chg_dwc_10k log_l2_chg_dwc_10k log_l1_chg_cases_10k log_l2_chg_cases_10k l1_pchg_gogl_fever l1_pchg_gogl_cough l1_pchg_gogl_fatigue, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group i.week#state_clean) vce(cluster i.week#state_clean)
estadd ysumm

eststo panel7: reghdfe log_chg_cases_10k log_l1_chg_swc_10k log_l2_chg_swc_10k share_within50 share_within150 log_l1_chg_dwc_10k log_l2_chg_dwc_10k log_l1_chg_cases_10k log_l2_chg_cases_10k log_l1_chg_lwc log_l2_chg_lwc, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group) vce(cluster i.week#state_clean)
estadd ysumm

eststo panel8: reghdfe log_chg_cases_10k log_l1_chg_swc_10k log_l2_chg_swc_10k share_within50 share_within150 log_l1_chg_dwc_10k log_l2_chg_dwc_10k log_l1_chg_cases_10k log_l2_chg_cases_10k log_l1_chg_lwc log_l2_chg_lwc, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group i.week#state_clean) vce(cluster i.week#state_clean)
estadd ysumm

xml_tab panel*, save("../_output/tables/raw/pooled_appendix_other_measures") stats(N r2 ymean) replace below

eststo clear

/////////////////////////////////////////////////													
// 2. Generate regression table by time period //
/////////////////////////////////////////////////

eststo clear

levelsof week_num, local(weeks)
foreach week of local weeks {
	
	eststo week`week': reghdfe log_chg_cases_10k log_l1_chg_swc_10k log_l2_chg_swc_10k share_within50 share_within150 log_l1_chg_dwc_10k log_l2_chg_dwc_10k log_l1_chg_cases_10k log_l2_chg_cases_10k, absorb(med_hhinc_group popdensity_group state_clean), if week_num == `week'
	estadd ysumm

	xml_tab week*, save("../_output/tables/raw/social_proximity_to_cases_by_time_period") stats(N r2 ymean) replace below


}

eststo clear
