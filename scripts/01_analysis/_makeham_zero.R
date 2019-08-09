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
  # Limit to Just Ages between 35 to 95
  filter(between(Age, 35, 95)) %>% 
  # Group by Country and Gender, so that we may run the operations on each of the sub-groups
  group_by(`Country Name`, Year, Gender) %>%
  mutate(l = log(1/(1-qx)) ) %>% 
  compute_model1(lambda_makeham = 0) -> 
  stage1_model

# Format table numbers (make the numbers more presentatble)
stage1_model %>% compute_table1 -> Table1


for (gender in stage1_model$Gender %>% unique) {
  Table1 %>% filter( Gender == gender ) %>%
    View(paste0("Table 1 - ", gender))
}

# Save averages
stage1_model %>% group_by(Gender) %>% 
  summarise_if(is.numeric, mean) %>% 
  mutate( g = g %>% percent ) %>% 
  mutate_if(is.numeric, ~round(., 3)) %>% 
  View("Table 1 Averages")
##########################
#   Table 2 Generation
##########################
stage1_model %>% 
  group_by(Gender) %>% 
  compute_stage2 -> stage2_model

# Format numbers
stage2_model %>% compute_table2 -> table2

# Save to csv
table2 %>% View("Table 2")

##########################
#   Table 3 Generation
##########################
stage2_model %>% compute_stage3 -> stage3_model
stage3_model %>% compute_table3 -> table3

# Save to csv
table3 %>% View("Table 3")

