# Packages

#  General

library(tidyverse)
library(janitor)
library(readxl)
library(writexl)
library(magrittr)
library(scriptName) # This will someday be useful for generating logs. 
library(furrr) # Beware futures. 

#  Spatial

library(sf)
library(terra)
library(tidyterra)
library(geodata)

#  Visualization

library(viridis)
library(RColorBrewer)
library(patchwork)
library(ggridges)
library(ggpubr)

#  Statistics and Econometrics

library(fixest)
library(broom)

#  Tables

library(modelsummary)
library(flextable)
library(gt)

#  Negation

`%!in%` <- Negate(`%in%`)
