// Purpose: Make the early exposure SCI vs COVID binscatters and in-text stats
// Inputs:
//		_intermediate\westchester_sci_covariates.csv	
//		_intermediate\lodi_sci_covariates.csv
// Outputs:
//		_output\westchester_binscatter_no_control.pdf
//		_output\westchester_binscatter_final.pdf
//		_output\lodi_binscatter_no_control.pdf
//		_output\lodi_binscatter_final.pdf
// Date: 07/30/2020
// Steps:
//		1. Generate Westchester binscatters
//		2. Generate Lodi binscatters

clear
set more off

//////////////////////////////////////////													
// 1. Generate Westchester binscatters //
/////////////////////////////////////////

import delimited "../_intermediate/westchester_sci_covariates.csv", clear

drop if county_fips == 36119

destring(dist pop confirmed deaths urban_rural_code cases_per_10k med_hhinc2016 popdensity2010), replace force

gen log_dist = log(dist)
egen dist_group = cut(dist), group(100)
gen log_sci = log(sci)

// Drop within 50 miles
drop if dist < 50

// Generate the sample with all covarites
quiet reg cases_per_10k log_sci i.urban_rural_code med_hhinc2016 popdensity2010 i.dist_group
gen  sample = e(sample)

// Get the R^2 of quadratic fits
gen log_sci_2 = log_sci^2
reg cases_per_10k log_sci log_sci_2 if sample == 1
reg cases_per_10k i.urban_rural_code med_hhinc2016 popdensity2010 i.dist_group if sample == 1 
reg cases_per_10k log_sci log_sci_2 i.urban_rural_code med_hhinc2016 popdensity2010 i.dist_group if sample == 1 

// Get the slopes of linear regression
reg cases_per_10k log_sci if sample == 1
reg cases_per_10k log_sci i.urban_rural_code med_hhinc2016 popdensity2010 i.dist_group if sample == 1 

// Generate the binscatters
binscatter  cases_per_10k log_sci if sample == 1, controls() ytitle(Cases per 10k people) xtitle(log(Social Connectedness))  reportreg ///
														n(100) line(qfit) ytitle(, size(large)) ylabel(, labsize(large)) xtitle(, size(large)) xlabel(, labsize(large))															
graph export "../_output/westchester_binscatter_no_control.pdf", replace

binscatter  cases_per_10k log_sci if sample == 1, controls(i.urban_rural_code med_hhinc2016 popdensity2010 i.dist_group) ytitle(Cases per 10k people) xtitle(log(Social Connectedness)) reportreg ///
														n(100) line(qfit) ytitle(, size(large)) ylabel(, labsize(large)) xtitle(, size(large)) xlabel(, labsize(large))															
graph export "../_output/westchester_binscatter_final.pdf", replace
													

//////////////////////////////////													
// 2. Generate Lodi binscatters //
//////////////////////////////////

import delimited "../_intermediate/lodi_sci_covariates.csv", clear

drop if nuts3_code == "ITC49"

gen log_sci = log(sci)
gen log_dist = log(dist)
egen dist_group = cut(dist), group(20)

drop if dist < 50

// Generate the sample with all covarites
quiet reg cases_per_10k log_sci i.dist_group gdp_per_hab pop_per_km
gen  sample = e(sample)

// Get the R^2 of quadratic fits
gen log_sci_2 = log_sci^2
reg cases_per_10k log_sci log_sci_2 if sample == 1
reg cases_per_10k i.dist_group gdp_per_hab pop_per_km if sample == 1 
reg cases_per_10k log_sci log_sci_2 i.dist_group gdp_per_hab pop_per_km if sample == 1 

// Get the slopes of linear regression
reg cases_per_10k log_sci if sample == 1
reg cases_per_10k log_sci i.dist_group gdp_per_hab pop_per_km if sample == 1 

// Generate the binscatters
binscatter  cases_per_10k log_sci, controls() ytitle(Cases per 10k people) xtitle(log(Social Connectedness)) reportreg ///
														n(30) line(qfit) ytitle(, size(large)) ylabel(, labsize(large)) xtitle(, size(large)) xlabel(, labsize(large))															
graph export "../_output/lodi_binscatter_no_control.pdf", replace

binscatter  cases_per_10k log_sci if sample == 1, controls(i.dist_group gdp_per_hab pop_per_km) ytitle(Cases per 10k people) xtitle(log(Social Connectedness)) reportreg ///
														n(30) line(qfit) ytitle(, size(large)) ylabel(, labsize(large)) xtitle(, size(large)) xlabel(, labsize(large))															
graph export "../_output/lodi_binscatter_final.pdf", replace

														
														
