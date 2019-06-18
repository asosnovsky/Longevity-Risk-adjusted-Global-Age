rm(list=ls())

library(readr)
library(tidyverse)
library(readxl)
library(scales)

# Read Data
cc = read_csv("./initial_replication/manual_coef.csv")
World_Mortality_2011 <- read_excel("initial_replication/World_Mortality_2011.xlsx", skip = 1)

# Mutate dataset to fit modelling (going from flat to narrow table)
World_Mortality_2011 %>% 
  gather(Country_Sex, qx, -c(Year, Age)) %>% 
  separate(Country_Sex, c("Country", "Gender"), '_') -> 
  narrow_dt

# filter to Males between 35 and 95 (replicating table 1a)
narrow_dt %>% filter(Gender == "M") %>%
  filter(between(Age, 35, 95)) %>% 
  # Join the data-set with manually entered coefficients
  inner_join(cc, by=c("Country" = "m_cc")) %>% 
  # create lambda values and y's
  mutate( 
    l = log(1/(1-qx)),
    y = log(l-(lambda_makem)*10^-5)  
  ) %>% 
  group_by(`Country Name`, Gender) %>% nest %>% 
  mutate(
    # Fit model
    model = map(data, ~lm(y~Age ,data=.)),
    # Extracts params
    l_m = map_dbl(data, ~unique(.$lambda_makem)),
    params = map(model, ~data_frame(
      K0 = .$coefficients[1],
      g = .$coefficients[2],
      lnh = as.numeric(K0-log((exp(g)-1)/g)),
      m = (log(g) - lnh)/g,
      b = 1/g
    ))
  ) %>% select(-c(data, model)) %>% unnest(params) %>% 
  # Apply some formatting
  mutate( g = g %>% percent ) %>% 
  mutate_if(is.numeric, ~round(., 3)) %>% 
  select(`Country Name`, lnh, l_m, g, m, b) %>% 
  write_csv("./initial_replication/01_table1a.csv")
