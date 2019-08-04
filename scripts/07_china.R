rm(list = ls())
library(tidyverse)
library(readr)

source("./scripts/00_method.R")

df <- bind_rows(
  read_table2("data/00_raw/china/group1.tsv") %>% mutate(Sector="1"),
  read_table2("data/00_raw/china/group2.tsv") %>% mutate(Sector="2"),
  read_table2("data/00_raw/china/group3.tsv") %>% mutate(Sector="3"),
  read_table2("data/00_raw/china/group4.tsv") %>% mutate(Sector="4"),
  read_table2("data/00_raw/china/group5.tsv") %>% mutate(Sector="5")
) %>% select(Sector, Age, qx)

df %>% 
  filter(between(Age, 35, 95)) %>%
  group_by(Sector) %>%
  compute_stage1 ->
  Stage1_model

Stage1_model %>% compute_stage2 -> Stage2_model

Stage2_model %>% compute_stage3 -> Stage3_model

# Table 1
Stage1_model %>% 
  mutate( g = g %>% percent(accuracy = 0.001) ) %>% 
  mutate_if(is.numeric, ~round(., 3)) %>% 
  select(Sector, lnh, l_m, g, m, b)

# Table 2
Stage2_model %>% mutate(params = map(model, ~tibble(
    `6_Adj. R^2` = .$adj.r.squared %>% percent,
    `2_L Std.Err` = .$coefficients[1,2],
    `3_-x* Std.Err` = .$coefficients[2,2],
    `4_L t-value` = .$coefficients[1,3],
    `5_-x* t-value` = .$coefficients[2,3],
  ))) %>% unnest(params) %>% 
  mutate(params = map(data, ~tibble(
    `7_Range: g` = paste0("(", percent(min(.$g)), ', ', percent(max(.$g)), ')' ),
    `9_Countries` = nrow(.)
  ))) %>% unnest(params) %>% 
  mutate_if(is.numeric, ~round(., 3)) %>% 
  rename(
    `0_L` = L,
    `1_x*` = `x*`,
    `8_G` = G
  ) %>% 
  select(-data, -model) %>%
  gather(stat, value) 

# Table 3
Stage3_model %>% filter( Age %in% c(35, 55, 70, 85, 95) ) %>% 
  mutate( Age = paste0("x = ", Age) )  %>% 
  select(`Sector`, Age, B_Age) %>% 
  spread(Age, B_Age) %>% 
  mutate_if(is.numeric, ~number(., acc=0.01))  
