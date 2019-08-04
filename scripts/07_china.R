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
  select(Sector, lnh, l_m, g, m, b) %>%
  write_csv("data/05_china/table1.csv")

# Table 2
Stage2_model %>% 
  rename(`L Value` = L, `x* Value` = `x*`, `G Value` = G) %>%
  mutate(params = map(model, ~tibble(
    `L Std.Err` = .$coefficients[1,2],
    `x* Std.Err` = .$coefficients[2,2],
    `L t-value` = .$coefficients[1,3],
    `x* t-value` = .$coefficients[2,3],
  ))) %>% unnest(params) %>%
  select(-c(data, model)) %>%
  gather(stat, value) %>%
  separate(stat, c("coef", "stat"), sep=" ") %>%
  spread(stat, value) %>%
  write_csv("data/05_china/table2-coef.csv")

Stage2_model %>% select(-c(L, G, `x*`)) %>%
  mutate(params = map(data, ~tibble(
    `Range: g` = paste0("(", percent(min(.$g)), ', ', percent(max(.$g)), ')' ),
    `Sectors` = nrow(.),
  ))) %>% unnest(params) %>% 
  mutate_if(is.numeric, ~round(., 3)) %>% 
  mutate( `Adj. R^2` = map_chr(model, ~.$adj.r.squared %>% percent)) %>%
  select(-c(data,model)) %>%
  gather(stat, value) %>%
  write_csv("data/05_china/table2-stats.csv")


# Table 3
Stage3_model %>% filter( Age %in% c(35, 55, 70, 85, 95) ) %>% 
  mutate( Age = paste0("x = ", Age) )  %>% 
  select(`Sector`, Age, B_Age) %>% 
  spread(Age, B_Age) %>% 
  mutate_if(is.numeric, ~number(., acc=0.01)) %>%
  write_csv("data/05_china/table3.csv")

