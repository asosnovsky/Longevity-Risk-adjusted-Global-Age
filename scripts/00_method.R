library(scales)
library(tidyverse)

compute_model1 <- function(dt, lambda_makeham) dt %>% 
  mutate( y = log(l-lambda_makeham) ) %>% nest %>% 
  mutate(
    # Fit model
    model = map(data, ~summary(lm(y~Age ,data=.))),
    # Extracts params
    l_m = lambda_makeham,
    params = map(model, ~tibble(
      K0 = .$coefficients[1],
      g = .$coefficients[2],
      lnh = as.numeric(K0-log((exp(g)-1)/g)),
      m = (log(g) - lnh)/g,
      b = 1/g,
      sigma = .$sigma
    ))
  ) %>% 
  select(-c(data, model)) %>% 
  unnest(params) 

compute_stage1 <- function(narrow_table, qeps=0) narrow_table %>% mutate( 
  l = log(1/(1-qx-qeps)),
  y = log(l-(lambda_makeham)*10^-5)  
) %>% 
  # Segment the dataset by country and gender
  group_by(`Country Name`, Gender) %>% nest %>% 
  mutate(
    # Fit model
    model = map(data, ~lm(y~Age ,data=.)),
    # Extracts params
    l_m = map_dbl(data, ~unique(.$lambda_makeham)),
    params = map(model, ~tibble(
      K0 = .$coefficients[1],
      g = .$coefficients[2],
      lnh = as.numeric(K0-log((exp(g)-1)/g)),
      m = (log(g) - lnh)/g,
      b = 1/g
    ))
  ) %>% 
  select(-c(data, model)) %>% 
  unnest(params) 

compute_stage2 <- function(stage1_model) stage1_model %>% group_by(Gender) %>% nest %>% 
  mutate(
    # Run the lin-reg
    model = map(data, ~summary(lm( lnh~g, data=. )) ),
    # Extract computed params
    L = map_dbl(model, ~.$coefficients[1,1]),
    `x*` = -map_dbl(model, ~.$coefficients[2,1]),
    G = map_dbl(data, ~mean(.$g))
  )


compute_table1 <- function(stage1_model) stage1_model %>% 
  mutate( g = g %>% percent ) %>% 
  mutate_if(is.numeric, ~round(., 3)) %>% arrange(Gender) %>% 
  select(Gender, `Country Name`, lnh, l_m, g, m, b)

compute_table2 <- function(stage2_model) stage2_model %>% mutate(params = map(model, ~tibble(
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
  gather(stat, value, -Gender) %>% 
  spread(Gender, value) %>% 
  separate(stat, c("idx", "Statistic"), sep='_') %>% 
  select(-idx)
