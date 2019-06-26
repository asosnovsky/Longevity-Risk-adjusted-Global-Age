rm(list=ls())

library(readr)
library(readxl)
library(tidyverse)
source("./scripts/00_method.R")

# Load my data-set
narrow_dt = read_csv("./data/01_processed/2011_qx_data.csv") %>% 
  # Remove Iceland
  filter(Country != "ISL") %>% 
  # filter to between 35 and 95 (replicating table 1)
  filter(between(Age, 35, 95))

##########################
#   Table 1 Generation
##########################
narrow_dt %>% 
  mutate(lambda_makeham = 0) %>% 
  compute_stage1 -> stage1_model

# Format table numbers (make the numbers more presentatble)
stage1_model %>% compute_table1 -> Table1


for (gender in stage1_model$Gender %>% unique) {
  Table1 %>% filter( Gender == gender ) %>%
    write_csv(paste0("./data/01_lm_zero/table_1_", gender, '.csv'))
}

# Save averages
stage1_model %>% group_by(Gender) %>% 
  summarise_if(is.numeric, mean) %>% 
  mutate( g = g %>% percent ) %>% 
  mutate_if(is.numeric, ~round(., 3)) %>% 
  write_csv("./data/01_lm_zero/01_table1-averages.csv")

##########################
#   Table 2 Generation
##########################
stage1_model %>% compute_stage2 -> stage2_model

# Format numbers
stage2_model %>% compute_table2 -> table2

# Save to csv
table2 %>% write_csv("./data/01_lm_zero/01_table2.csv")

