rm(list=ls())

library(readr)
library(tidyverse)
library(readxl)
library(scales)

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
# filter to Males between 35 and 95 (replicating table 1a)
narrow_dt %>% 
  filter(between(Age, 35, 95)) %>% 
  # Join the data-set with manually entered coefficients
  inner_join(cc, by=c("Country", "Gender")) %>% 
  # create lambda values and y's
  mutate( 
    l = log(1/(1-qx)),
    y = log(l-(lambda_makeham)*10^-5)  
  ) %>% 
  group_by(`Country Name`, Gender) %>% nest %>% 
  mutate(
    # Fit model
    model = map(data, ~lm(y~Age ,data=.)),
    # Extracts params
    l_m = map_dbl(data, ~unique(.$lambda_makeham)),
    params = map(model, ~data_frame(
      K0 = .$coefficients[1],
      g = .$coefficients[2],
      lnh = as.numeric(K0-log((exp(g)-1)/g)),
      m = (log(g) - lnh)/g,
      b = 1/g
    ))
  ) %>% select(-c(data, model)) %>% unnest(params) -> stage1_model

# Format table numbers (make the numbers more presentatble)
stage1_model %>% 
  mutate( g = g %>% percent ) %>% 
  mutate_if(is.numeric, ~round(., 3)) %>% arrange(Gender) %>% 
  select(Gender, `Country Name`, lnh, l_m, g, m, b) -> table1

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
stage1_model %>% group_by(Gender) %>% nest %>% 
  mutate(
    # Run the lin-reg
    model = map(data, ~summary(lm( lnh~g, data=. )) ),
    # Extract computed params
    L = map_dbl(model, ~.$coefficients[1,1]),
    `x*` = -map_dbl(model, ~.$coefficients[2,1]),
    G = map_dbl(data, ~mean(.$g))
  ) -> stage2_model

# Format numbers
stage2_model %>% mutate(params = map(model, ~data_frame(
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
  select(-idx) -> table2

# Save to csv
table2 %>% write_csv("./initial_replication/01_table2.csv")

