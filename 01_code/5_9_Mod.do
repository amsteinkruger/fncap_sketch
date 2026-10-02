* Models for processed notifications. 

*  Workspace

* cd ..

*  Data

clear

import delimited "03_intermediate/dat_panel_portfolios_implicit.csv"

destring price* rate* site_class proportion* vpd_mean cwd_mean fire*, replace ignore("NA")

*  Linear

reg mbf_both acres_owned price_stumpage_douglasfir_mean rate_mean elevation slope site_class proportion_douglasfir distance_city pyrome_klamath_mountains_area_pr pyrome_middle_cascades_area_pr cwd_mean fire_15_doughnut_mean_4 fire_30_doughnut_mean_4

*  Tobit

clear

import delimited "03_intermediate/dat_panel_portfolios_explicit.csv"

destring price* rate* site_class proportion* vpd_mean cwd_mean fire*, replace ignore("NA")

tobit mbf_both acres_owned price_stumpage_douglasfir_mean rate_mean elevation slope site_class proportion_douglasfir distance_city pyrome_klamath_mountains_area_pr pyrome_middle_cascades_area_pr cwd_mean fire_15_doughnut_mean_4 fire_30_doughnut_mean_4, ll(0)

*  Heckit

gen mbf_binary = 0
replace mbf_binary = 1 if mbf_bin == "TRUE"

gen mbf_log = .
replace mbf_log = log(mbf_both) if mbf_binary == 1

heckman mbf_log acres_owned price_stumpage_douglasfir_mean rate_mean elevation slope site_class proportion_douglasfir distance_city pyrome_klamath_mountains_area_pr pyrome_middle_cascades_area_pr cwd_mean fire_15_doughnut_mean_4 fire_30_doughnut_mean_4, select(mbf_binary = acres_owned price_stumpage_douglasfir_mean rate_mean elevation slope site_class proportion_douglasfir distance_city pyrome_klamath_mountains_area_pr pyrome_middle_cascades_area_pr cwd_mean fire_15_doughnut_mean_4 fire_30_doughnut_mean_4) twostep first

*  Craggit

*   Quick fix for churdle's nonconvergence with level production. Does this log form bias estimates?

gen mbf_log_binary = mbf_binary
replace mbf_log_binary = mbf_log if mbf_binary == 1

churdle linear mbf_log_binary acres_owned price_stumpage_douglasfir_mean rate_mean elevation slope site_class proportion_douglasfir distance_city pyrome_klamath_mountains_area_pr pyrome_middle_cascades_area_pr cwd_mean fire_15_doughnut_mean_4 fire_30_doughnut_mean_4, select(acres_owned price_stumpage_douglasfir_mean rate_mean elevation slope site_class proportion_douglasfir distance_city pyrome_klamath_mountains_area_pr pyrome_middle_cascades_area_pr cwd_mean fire_15_doughnut_mean_4 fire_30_doughnut_mean_4) ll(0)

margins, dydx(acres_owned price_stumpage_douglasfir_mean rate_mean elevation slope site_class proportion_douglasfir distance_city pyrome_klamath_mountains_area_pr pyrome_middle_cascades_area_pr cwd_mean fire_15_doughnut_mean_4 fire_30_doughnut_mean_4)
