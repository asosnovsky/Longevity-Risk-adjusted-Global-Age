# Ensure we are starting with a clean-slate
rm(list=ls())

# Load used libraries
library(readr)
source("./scripts/00_method.R")

# Read in the modelled data
Stage1_model <- read_rds("./data/03_historic/stage1-lg.rds")

# Prep Data
constricted_years = seq(1921, 2011, 1)
Stage1_model %>% 
  filter( Gender != "Total" ) %>%
  inner_join(
    tibble(Year= constricted_years),
    by="Year"
  ) %>% 
  group_by(`Country Name`) %>% arrange(Year) %>% nest %>% 
  mutate(
    ycounts = map_chr(data, ~paste(unique(.$Year), collapse=','))
  ) %>% 
  filter(
    ycounts == paste( constricted_years, collapse = ',')
  ) %>% 
  select(-ycounts) %>% 
  unnest(data) -> 
  prep_data

prep_data %>%
  group_by(Year, Gender) %>% nest %>%
  mutate(
    b = map_dbl(data, ~mean(.$b)),
    m = map_dbl(data, ~mean(.$m)),
    lnh = map_dbl(data, ~mean(.$lnh))
    #,    N = map_dbl(data, ~length(unique(.$`Country Name`)))
  ) %>%
  gather(stat, value, -c(Year, Gender, data)) %>%
  mutate(
    std = case_when(
      stat == "b" ~ map_dbl(data, ~sd(.$b)),
      stat == "m" ~ map_dbl(data, ~sd(.$m)),
      stat == "lnh" ~ map_dbl(data, ~sd(.$lnh))
    ),
    upper = value + 2*std,
    lower = value - 2*std
  ) %>%
  write_rds("./data/04_historic_display/stage1.rds")

prep_data %>% 
  compute_stage2 %>%
  gather(stat, value, -c(Year, Gender, model, data)) %>%
  mutate(
    std = case_when(
      stat == "L" ~ map_dbl(model, ~.$coefficients[1,2]),
      stat == "x*" ~ map_dbl(model, ~.$coefficients[2,2]),
      stat == "G" ~ 0
    ),
    upper = value + 2*std,
    lower = value - 2*std
  ) %>%
  write_rds("./data/04_historic_display/stage2.rds")
