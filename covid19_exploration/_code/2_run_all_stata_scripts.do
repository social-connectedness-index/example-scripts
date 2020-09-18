clear
set more off

// Make the early exposure SCI vs COVID binscatters and in-text stats
do "early_exposure/a4_make_binscatters.do"

clear

// Make the regression tables for the time series in US
do "time_series/b4_time_series_regress.do"
