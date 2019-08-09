##
## This file will prep the data used for the historic plots and latex tables
##

# Ensure we are starting with a clean-slate
rm(list=ls())

# Load used libraries
library(readr)
source("./scripts/00_method.R")

# Read in the modelled data
Stage1_model <- read_rds("./data/02_models/historic-stage1.rds")

# Prep Data
Stage1_model %>% 
  filter( Gender != "Total" ) %>%
  apply_filters(from_year = 1945, to_year = 2011, delta_year = 1) ->
  prep_data

prep_data %>%
  prep_display_data_stage1 %>%
  write_rds("./data/02_display/historic_stage1.rds")

prep_data %>% 
  prep_display_data_stage2 %>%
  write_rds("./data/02_display/historic_stage2.rds")
