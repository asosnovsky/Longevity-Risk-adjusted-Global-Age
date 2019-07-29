# Ensure we are starting with a clean-slate
rm(list=ls())

# Load used libraries
library(readr)
source("./scripts/00_method.R")

# Read in the modelled data
Stage1_model <- read_rds("./data/03_historic/stage1-lg.rds")

# Prep Data
Stage1_model %>% 
  filter( Gender != "Total" ) %>%
  apply_filters(from_year = 1945, to_year = 2011, delta_year = 1) ->
  prep_data

prep_data %>%
  prep_display_data_stage1 %>%
  write_rds("./data/04_historic_display/stage1.rds")

prep_data %>% 
  prep_display_data_stage2 %>%
  write_rds("./data/04_historic_display/stage2.rds")
