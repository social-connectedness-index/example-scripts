// Purpose: Make tables with time series regressions
// Inputs:
//		_intermediate\time_series_regress_dat.csv
// Outputs:
//		_output\tables\raw\social_proximity_to_cases_pooled.xml
//		_output\tables\raw\social_proximity_to_cases_by_week.xml
// Date: 07/30/2020
// Steps:
//		1. Generate 'pooled' regression table
//		2. Generate regression table by time period

clear
set more off

////////////////////////////////////////////													
// 1. Generate 'pooled' regression table //
///////////////////////////////////////////

import delimited "../_intermediate/time_series_regress_dat.csv", clear

destring(log_chg_cases_10k log_l1_chg_cases_10k log_l2_chg_cases_10k log_l1_chg_swc_10k log_l2_chg_swc_10k log_l1_chg_dwc_10k log_l2_chg_dwc_10k), replace force

// Make percentiles of median household income and pop density
egen med_hhinc_group = cut(med_hhinc2016_10k), group(100)
egen popdensity_group = cut(popdensity2010_10k), group(100)

encode province_state, gen(state_clean)

// We need two weeks of lagged data for these regressions
keep if week_num >= 15

/// Pooling all time periods (but using FEs interacted with month), calculate cases per 10k change
eststo clear

eststo panel1: reghdfe log_chg_cases_10k log_l1_chg_swc_10k log_l2_chg_swc_10k log_l1_chg_cases_10k log_l2_chg_cases_10k, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group) vce(cluster i.week_num)
estadd ysumm

eststo panel2: reghdfe log_chg_cases_10k log_l1_chg_dwc_10k log_l2_chg_dwc_10k log_l1_chg_cases_10k log_l2_chg_cases_10k, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group) vce(cluster i.week_num)
estadd ysumm

eststo panel3: reghdfe log_chg_cases_10k log_l1_chg_swc_10k log_l2_chg_swc_10k log_l1_chg_dwc_10k log_l2_chg_dwc_10k log_l1_chg_cases_10k log_l2_chg_cases_10k, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group) vce(cluster i.week_num)
estadd ysumm

eststo panel4: reghdfe log_chg_cases_10k log_l1_chg_swc_10k log_l2_chg_swc_10k log_l1_chg_cases_10k log_l2_chg_cases_10k, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group i.week#state_clean) vce(cluster i.week_num)
estadd ysumm

eststo panel5: reghdfe log_chg_cases_10k log_l1_chg_dwc_10k log_l2_chg_dwc_10k log_l1_chg_cases_10k log_l2_chg_cases_10k, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group i.week#state_clean) vce(cluster i.week_num)
estadd ysumm

eststo panel6: reghdfe log_chg_cases_10k log_l1_chg_swc_10k log_l2_chg_swc_10k log_l1_chg_dwc_10k log_l2_chg_dwc_10k log_l1_chg_cases_10k log_l2_chg_cases_10k, absorb(i.week_num#med_hhinc_group i.week_num#popdensity_group i.week#state_clean) vce(cluster i.week_num)
estadd ysumm

xml_tab panel*, save("../_output/tables/raw/social_proximity_to_cases_pooled") stats(N r2 ymean) replace below

eststo clear



/////////////////////////////////////////////////													
// 2. Generate regression table by time period //
/////////////////////////////////////////////////

eststo clear

levelsof week_num, local(weeks)
foreach week of local weeks {
	
	eststo week`week': reghdfe log_chg_cases_10k log_l1_chg_swc_10k log_l2_chg_swc_10k log_l1_chg_dwc_10k log_l2_chg_dwc_10k log_l1_chg_cases_10k log_l2_chg_cases_10k, absorb(med_hhinc_group popdensity_group state_clean), if week_num == `week'
	estadd ysumm

	xml_tab week*, save("../_output/tables/raw/social_proximity_to_cases_by_time_period") stats(N r2 ymean) replace below


}

eststo clear
