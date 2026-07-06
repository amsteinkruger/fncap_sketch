# Packages

#  General

library(tidyverse)
library(readxl)
library(writexl)
library(magrittr)
library(scriptName) # This will someday be useful for generating logs. 

#  Spatial

library(terra)
library(tidyterra)

#  Visualization

library(viridis)
library(patchwork)
library(ggridges)
library(ggpubr)

#  Statistics and Econometrics

library(fixest)
library(broom)

#  Tables

library(modelsummary)
library(flextable)

#  Negation

`%!in%` <- Negate(`%in%`)
