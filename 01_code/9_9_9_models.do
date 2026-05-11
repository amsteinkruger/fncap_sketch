* Models for processed notifications. 

*  Data

* cd "GitHub/fncap_sketch"

clear

import delimited "03_intermediate/dat_firms_implicit_3_1.csv"

gen stumpage_lag_1_log = log(stumpage_lag_1)
gen lumber_lag_1_log = log(lumber_lag_1)
gen vpd_lag_1_log = log(vpd_lag_1)

*  Linear

reg mbf stumpage_lag_1 lumber_lag_1 rate_lag_1 fire_30 fire_proportion vpd_lag_1

*  Tobit

clear

import delimited "03_intermediate/dat_firms_explicit_3_1.csv"

gen stumpage_lag_1_log = log(stumpage_lag_1)
gen lumber_lag_1_log = log(lumber_lag_1)
gen vpd_lag_1_log = log(vpd_lag_1)

tobit mbf stumpage_lag_1 lumber_lag_1 rate fire_30 fire_proportion vpd_lag_1, ll(0)

*  Heckit

gen mbf_0 = 0
replace mbf_0 = 1 if mbf > 0

gen mbf_log = .
replace mbf_log = log(mbf) if mbf_0 == 1

heckman mbf_log stumpage_lag_1_log lumber_lag_1_log rate fire_30 fire_proportion vpd_lag_1, select(mbf_0 = stumpage_lag_1 lumber_lag_1 rate) twostep first // fire_30 fire_proportion vpd_lag_1

*  Hurdles

* Cragg's double hurdle might not be appropriate in the absence of "zero-type" (all zeros) observations. Or there are other problems with the structure of the data that prevent convergence. 
* See also Engel et al. (2014) in the Stata Journal on alternative approaches to double hurdle implementation. The code is outdated but the statistical reasoning might not be. 

* churdle linear mbf stumpage_lag_1_log lumber_lag_1_log rate fire_30 fire_proportion vpd_lag_1_log, select(stumpage_lag_1_log lumber_lag_1_log rate) ll(0) // fire_30 fire_proportion vpd_lag_1_log
* churdle exponential mbf stumpage_lag_1_log lumber_lag_1_log rate fire_30 fire_proportion vpd_lag_1_log, select(stumpage_lag_1_log lumber_lag_1_log rate fire_30 fire_proportion vpd_lag_1_log) ll(0)
