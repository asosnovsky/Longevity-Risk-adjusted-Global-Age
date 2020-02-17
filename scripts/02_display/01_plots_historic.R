# Ensure we are starting with a clean-slate
rm(list=ls())

# Load used libraries
library(readr)
source("./scripts/00_method.R")

# Read in the modelled data
Stage1_model <- read_rds("./data/01_models/historic-stage1.rds")

# Util functions
pretty_names_s1 <- function(s1) s1 %>% mutate( stat = case_when(
  stat == "b" ~"Dispersion Coefficient (b)",
  stat == "m" ~ "Modal Value (m)"
))
pretty_names_s2 <- function(s2) s2 %>% mutate( stat = case_when(
  stat == "x*" ~ "Plateau Age (x*)",
  stat == "L" ~ "Intercept (L)",
  stat == "G" ~ "Mortality Growth Rate (G)"
))

# 1945
Stage1_model %>% 
  filter( Gender != "Total" ) %>%
  apply_filters(from_year = 1945, to_year = 2011, delta_year = 1) -> 
  prep_data1945

# =====================
# Figure 5 - no titles
# =====================

prep_data1945 %>%
  prep_display_data_stage1 %>%
  filter( stat %in% c('b', 'm') ) %>% pretty_names_s1 %>%
  history_plot("Figure 5a: Historic Gompertz Coefficients", x_breaks = 5, no_title = T) %>%
  save_plots("images/figure-5/f5a-historic_gompertz-1945")
