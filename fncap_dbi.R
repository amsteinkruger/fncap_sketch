# Check out an FIA DB product from PNWRS by way of DBI.

# Packages

library(tidyverse)
library(dbplyr)
library(DBI)

# Data

con <- dbConnect(RSQLite::SQLite(), "data/FIADB_CAORWA2021_SQLite.db")

dbListTables(con)

# Count plots.

obs_pnwrs = 
  tbl(con, "PLOT") %>% 
  select(STATECD, PLOT, INVYR) %>% 
  filter(STATECD == 41) %>% 
  distinct %>% 
  as_tibble %>% 
  nrow

obs_core = 
  "data/OR_PLOT.csv" %>% 
  read_csv %>% 
  select(STATECD, PLOT, INVYR) %>% 
  distinct %>% 
  nrow

# Note that discrepancies in counts follow from different time series lengths: 1999 appears in "core" but not "pnwrs."

# Check missingness in disturbance codes.

dstrb_pnwrs =
  tbl(con, "COND") %>% 
  filter(STATECD == 41) %>% 
  select(DSTRBCD1) %>% 
  as_tibble %>% 
  drop_na %>% 
  nrow

dstrb_core = 
  "data/OR_COND.csv" %>% 
  read_csv %>% 
  select(DSTRBCD1) %>% 
  drop_na %>% 
  nrow

# These match.

# Compare disturbance (and treatment) codes and years from core against PNWRS.

dstrb_match =
  tbl(con, "COND") %>% 
  filter(STATECD == 41) %>% 
  select(starts_with("DSTRBCD"), starts_with("TRT")) %>% 
  as_tibble

# These match at a glance. PNWRS appears to add detail to some observations, but then some observations are missing DSTRB/TRT codes in PNWRS and non-missing in core.

# So, I'll put a pin in this until I can follow up with someone who knows more about PNWRS FIA processes.
