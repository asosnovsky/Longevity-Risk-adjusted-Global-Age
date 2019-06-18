# Ensure we are starting with a clean-slate
rm(list=ls())

# Set the seed for any future randomization needs
set.seed(0)

# Load used libraries
library(readr)
library(tidyverse)
library(scales)

# Read in the modelled data
Stage1_model <- read_rds("./data/02_models/stage1.rds")
Stage2_model <- read_rds("./data/02_models/stage2.rds")
Stage3_model <- read_rds("./data/02_models/stage3.rds")

# Read-in Country-codes
country_codes <- read_csv("./data/01_processed/country_codes.csv")

# Set Constants
lambda_multiplier = 10^5
epsilon = 1E-10

# Helper Format Function
lmb_format = function(l) paste0(round(l*lambda_multiplier), "x10^-", log10(lambda_multiplier))

# Table 1
Stage1_model %>% 
  mutate(
    l_0 = map_dbl(data, ~.$l[1]) %>% lmb_format,
    l_55 = map_dbl(data, ~.$l[56]),
    m = (log(g) - lnh)/g,
    b = 1/g,
    l_m = lmb_format(l_m)
  ) %>% 
  left_join(country_codes, by="Country") %>% 
  select(
    Gender,
    `Country Name`,
    lnh, l_0, l_m, g, l_55, m, b
  ) %>% mutate_if(is.numeric, ~round(., 2)) -> 
  Table1

for (gender in Stage1_model$Gender %>% unique) {
  Table1 %>% filter( Gender == gender ) %>%
    write_csv(paste0("./data/02_paper_tables/table_1_", gender, '.csv'))
}

# Table 2
Stage2_model %>% 
  mutate(params = map(model, ~data_frame(
    `6_Adj. R^2` = .$adj.r.squared %>% percent,
    `2_L Std.Err` = .$coefficients[1,2],
    `3_-x* Std.Err` = .$coefficients[2,2],
    `4_L t-value` = .$coefficients[1,3],
    `5_-x* t-value` = .$coefficients[2,3],
  ))) %>% unnest(params) %>% 
  mutate(params = map(data, ~data_frame(
    `7_Range: g` = paste0("(", percent(min(.$g)), ', ', percent(max(.$g)), ')' ),
    `9_Countries` = nrow(.)
  ))) %>% unnest(params) %>% 
  mutate_if(is.numeric, ~round(., 2)) %>% 
  rename(
    `0_L` = L,
    `1_x*` = `x*`,
    `8_G` = G
  ) %>% 
  select(-data, -model) %>% 
  gather(stat, value, -Gender) %>% 
  spread(Gender, value) %>% 
  separate(stat, c("idx", "Statistic"), sep='_') %>% 
  select(-idx) -> Table2

write_csv(Table2, "./data/02_paper_tables/table2.csv")

# Table 3
Stage3_model %>% 
  filter( Age %in% c(55, 70, 85) ) %>% 
  mutate( Age = paste0(Gender, "\n", "x = ", Age) )  %>% 
  left_join(country_codes, by="Country") %>% 
  select(`Country Name`, Age, B_Age) %>% 
  spread(Age, B_Age) %>% 
  mutate_if(is.numeric, ~round(., 2)) ->
  Table3

write_csv(Table3, "./data/02_paper_tables/table3.csv")
