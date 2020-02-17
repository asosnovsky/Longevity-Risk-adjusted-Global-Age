# Ensure we are starting with a clean-slate
rm(list=ls())

# Read in the modelled data
Stage1_model <- read_rds("./data/01_models/stage1.rds") %>% filter( Gender != "Total" )

Stage1_model %>%
  mutate(
    r.sqaured = map_dbl(raw_model, ~summary(.)$r.squared)
  ) %>%
  select(
    Year, Gender, `Country Name`, r.sqaured
  ) %>% View("R2 Values for first Regression")
