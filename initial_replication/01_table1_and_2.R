rm(list=ls())

library(readr)
library(readxl)
library(tidyverse)
source("./scripts/00_method.R")

# Read Data
cc = read_csv("./initial_replication/manual_coef.csv") %>%  
  select(Country = m_cc, `Country Name`, M=lmakeham_M, F=lmakeham_F) %>% 
  gather(Gender, lambda_makeham, -c(Country, `Country Name`))
World_Mortality_2011 <- read_excel("./initial_replication/World_Mortality_2011.xlsx", skip = 1)

# Mutate dataset to fit modelling (going from flat to narrow table)
World_Mortality_2011 %>% 
  gather(Country_Sex, qx, -c(Year, Age)) %>% 
  separate(Country_Sex, c("Country", "Gender"), '_') -> 
  narrow_dt

##########################
#   Table 1 Generation
##########################
narrow_dt %>% 
  # Remove Iceland
  filter(Country != "ISL") %>% 
  # filter to between 35 and 95 (replicating table 1)
  filter(between(Age, 35, 95)) %>% 
  # Join the data-set with manually entered coefficients
  inner_join(cc, by=c("Country", "Gender")) %>% 
  compute_stage1 -> stage1_model

# Format table numbers (make the numbers more presentatble)
stage1_model %>% compute_table1 -> table1

# Save Table1a
table1 %>% filter(Gender == "M") %>% select(-Gender) %>% 
  write_csv("./initial_replication/01_table1a.csv")

# Save Table1b
table1 %>% filter(Gender == "F") %>% select(-Gender) %>% 
  write_csv("./initial_replication/01_table1b.csv")

# Save averages
stage1_model %>% group_by(Gender) %>% 
  summarise_if(is.numeric, mean) %>% 
  mutate( g = g %>% percent ) %>% 
  mutate_if(is.numeric, ~round(., 3)) %>% 
  write_csv("./initial_replication/01_table1-averages.csv")

##########################
#   Table 2 Generation
##########################
stage1_model %>% compute_stage2 -> stage2_model

# Format numbers
stage2_model %>% compute_table2 -> table2

# Save to csv
table2 %>% write_csv("./initial_replication/01_table2.csv")

