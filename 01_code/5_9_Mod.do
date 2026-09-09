* Models for processed notifications. 

*  Folders

*   Either go down a couple folders or go up one. 

* cd "GitHub/fncap_sketch"
* cd ..

*  Data

clear

import delimited "03_intermediate/dat_firms_implicit_3_1.csv"

gen mbf_both = mbf_douglasfir + mbf_westernhemlock

*  Linear

reg mbf_both siteclassmode elevation distance_mill stumpage_mean_20 rate_mean_20 vpd_mean_20 fire_30_doughnut_lag_1

*  Tobit

clear

import delimited "03_intermediate/dat_firms_explicit_3_1.csv"

gen mbf_both = mbf_douglasfir + mbf_westernhemlock

tobit mbf_both siteclassmode elevation distance_mill stumpage_mean_20 rate_mean_20 vpd_mean_20 fire_30_doughnut_lag_1, ll(0)

*  Heckit

gen mbf_0 = 0
replace mbf_0 = 1 if mbf_both > 0

gen mbf_log = .
replace mbf_log = log(mbf_both) if mbf_0 == 1

heckman mbf_log siteclassmode elevation distance_mill stumpage_mean_20 rate_mean_20 vpd_mean_20 fire_30_doughnut_lag_1, select(mbf_0 = stumpage_mean_20 rate_mean_20 vpd_mean_20 fire_30_doughnut_lag_1) twostep first

*  Hurdles

* Cragg's double hurdle might not be appropriate in the absence of "zero-type" (all zeros) observations. Or there are other problems with the structure of the data that prevent convergence. 
* See also Engel et al. (2014) in the Stata Journal on alternative approaches to double hurdle implementation. The code is outdated but the statistical reasoning might not be. 

* churdle linear mbf_both siteclassmode elevation distance_mill stumpage_mean_20 rate_mean_20 vpd_mean_20 fire_30_doughnut_lag_1, select(stumpage_mean_20 rate_mean_20 vpd_mean_20 fire_30_doughnut_lag_1) ll(0)
