# Ensure we are starting with a clean-slate
rm(list=ls())

# Set the seed for any future randomization needs
set.seed(0)

# Load used libraries
library(readr)
library(broom)
source("./scripts/00_method.R")

# Read in the merged dataset for 2011
dataset = read_csv("./data/01_processed/2011_qx_data.csv") %>% 
  # Remove Iceland as its too much of an outlier
  filter( !(Country %in% c("ISL")) )

#################################################
# Stage One - Estimating Sub-Group Parameters
# --- PLEASE BE AWARE THAT THIS IS SLOW ---
#   --- IT TAKE ~2-5 minutes to run ---
#################################################
dataset %>%
  # Limit to Just Ages between 35 to 95
  filter(between(Age, 35, 95)) %>% 
  # Group by Country and Gender, so that we may run the operations on each of the sub-groups
  group_by(`Country Name`, Year, Gender) %>%
  # Compute the results of stage1
  compute_stage1 -> 
  Stage1_model

Stage1_model %>% write_rds("./data/02_models/stage1.rds")

#################################################
# Stage Two - CLaM
#################################################
Stage1_model %>% group_by(Year, Gender) %>% compute_stage2 -> Stage2_model

Stage2_model %>% write_rds("./data/02_models/stage2.rds")

#################################################
# Stage Three+Four - Compute B-Age
#################################################
Stage2_model %>% compute_stage3 -> Stage3_model

Stage3_model %>% write_rds("./data/02_models/stage3.rds")
