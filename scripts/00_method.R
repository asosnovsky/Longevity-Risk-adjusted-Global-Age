library(scales)
library(tidyverse)

apply_filters <- function(dataset, from_year, to_year, delta_year, from_age, to_age) {
  dataset %>% 
    filter(between(Age, from_age, to_age)) %>% 
    inner_join(
      tibble(Year= seq(from_year,to_year,delta_year)),
      by="Year"
    ) %>% 
    group_by(`Country Name`) %>% arrange(Year) %>% nest %>% 
    mutate(
      ycounts = map_chr(data, ~paste(unique(.$Year), collapse=','))
    ) %>% 
    filter(
      ycounts == paste(seq(from_year,to_year,delta_year), collapse = ',')
    ) %>% 
    select(-ycounts) %>% 
    unnest(data)
}

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

compute_stage2 <- function(stage1_model) stage1_model %>% 
  group_by(Year, Gender) %>% nest %>% 
  mutate(
    # Run the lin-reg
    model = map(data, ~summary(lm( lnh~g, data=. )) ),
    # Extract computed params
    L = map_dbl(model, ~.$coefficients[1,1]),
    `x*` = -map_dbl(model, ~.$coefficients[2,1]),
    G = map_dbl(data, ~mean(.$g))
  )

compute_stage3 <- function(stage2_model) stage2_model %>% 
  mutate(
    x_stdev = map_dbl(model, ~.$coefficients[2,2]),
    `x* - higher` = `x*` + 2*x_stdev,
    `x* - lower` = `x*` - 2*x_stdev,
  ) %>% 
  unnest(data) %>% 
  mutate(
    ki = (g/G)-1
  ) %>% unnest(data) %>% 
  mutate(
    B_Age = Age - ki*(`x*`-Age),
    B_Age_Upper = Age - ki*(`x* - higher`-Age),
    B_Age_Lower = Age - ki*(`x* - lower`-Age)
  ) -> Stage3_model


compute_table1 <- function(stage1_model) stage1_model %>% 
  mutate( g = g %>% percent(accuracy = 0.001) ) %>% 
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

compute_table3 <- function(stage3_model) stage3_model %>% 
  filter( Age %in% c(55, 70, 85) ) %>% 
  mutate( Age = paste0(Gender, "\n", "x = ", Age) )  %>% 
  select(`Country Name`, Age, B_Age) %>% 
  spread(Age, B_Age) %>% 
  mutate_if(is.numeric, ~round(., 3))

compute_s2_param_list <- function(stage2_model) stage2_model %>%
  mutate(
    `l*` = L
  ) %>%
  gather(stat, value, -c(Year, Gender, model, data)) %>%
  mutate(
    std = case_when(
      stat == "L" ~ map_dbl(model, ~.$coefficients[1,2]),
      stat == "l*" ~ map_dbl(model, ~.$coefficients[1,2]),
      stat == "x*" ~ map_dbl(model, ~.$coefficients[2,2]),
      stat == "G" ~ 0
    ),
    upper = value + 2*std,
    lower = value - 2*std
  ) %>% 
  mutate(
    l_m = map_dbl(data, ~mean(.$l_m)),
    value = case_when(
      stat == "l*" ~ exp(value) - l_m,
      TRUE ~ value
    ),
    upper = case_when(
      stat == "l*" ~ exp(upper) - l_m,
      TRUE ~ upper
    ),
    lower = case_when(
      stat == "l*" ~ exp(lower) - l_m,
      TRUE ~ lower
    )
  ) 

