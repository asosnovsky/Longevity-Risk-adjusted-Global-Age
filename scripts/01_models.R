# Ensure we are starting with a clean-slate
rm(list=ls())

# Set the seed for any future randomization needs
set.seed(0)

# Load used libraries
library(readr)
library(tidyverse)
library(broom)
library(scales)
source("./scripts/00_method.R")

# Read in the merged dataset for 2011
dataset = read_csv("./data/01_processed/2011_qx_data.csv") %>% 
  # Remove Iceland as its too much of an outlier
  filter(
    !(Country %in% c("ISL"))
  )

#################################################
# Stage One - Estimating Sub-Group Parameters
#################################################
dataset %>%
  # Limit to Just Ages between 35 to 95
  filter(between(Age, 35, 95)) %>% 
  # Generate initial lambda values
  mutate( l = log(1/(1-qx)) ) %>%
  # Group by Country and Gender, so that we may run the operations on each of the sub-groups
  group_by(`Country Name`, Gender) %>% nest %>% 
  mutate(model = map(data, ~lapply( 
    # Create a sequence of potential accidental death lambdas
    seq(1E-5, min(.$l), by=1E-5),
    # Fit a linear-regression model for each acc-death lambda per sub-group
    function(l_m) compute_model1(., l_m)
  ) %>% reduce(bind_rows))) %>%
  mutate( optimal = map(model, ~filter(., sigma==min(sigma))) ) %>% 
  unnest(optimal) -> 
  Stage1_model

Stage1_model %>% write_rds("./data/02_models/stage1.rds")

#################################################
# Stage Two - CLaM
#################################################
Stage1_model %>% compute_stage2 -> Stage2_model

Stage2_model %>% write_rds("./data/02_models/stage2.rds")

#################################################
# Stage Three+Four - Compute B-Age
#################################################
Stage2_model %>% 
  mutate(
    x_stdev = map_dbl(model, ~.$coefficients[2,2]),
    `x* - higher` = `x*` + 2*x_stdev,
    `x* - lower` = `x*` - 2*x_stdev,
  ) %>% 
  unnest(data) %>% 
  mutate(
    ki = (g/G)-1
  ) %>% unnest(data) %>% 
  mutate(
    B_Age = Age - ki*(`x*`-Age),
    B_Age_Upper = Age - ki*(`x* - higher`-Age),
    B_Age_Lower = Age - ki*(`x* - lower`-Age)
  ) -> Stage3_model

Stage3_model %>% write_rds("./data/02_models/stage3.rds")
