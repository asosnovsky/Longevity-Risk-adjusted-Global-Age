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
  compute_stage1 -> stage1_model_zm

stage1_model_zm %>% 
  mutate(
    H = (exp(g)-1)*exp(lnh)/g
  ) %>% 
  select(H, g) %>% 
  gather(param, value) %>% 
  group_by(param) %>% 
  summarise( min(value), max(value) )
