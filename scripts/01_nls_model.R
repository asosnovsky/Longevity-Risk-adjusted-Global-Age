rm(list=ls())

# Load used libraries
library(readr)
library(tidyverse)
source("./scripts/00_method.R")

# Read in the merged dataset for 2011
dataset = read_csv("./data/01_processed/2011_qx_data.csv") %>% 
  # Remove Iceland as its too much of an outlier
  filter( !(Country %in% c("ISL") ) )

dataset %>% 
  filter(between(Age, 35, 95)) %>% 
  mutate( qtx = -log( 1-qx ) ) %>% 
  group_by( `Country Name`, Gender ) %>% nest %>% 
  mutate(
    # Use R's least-square routine to obtain a solution
    model = map(data, ~summary(nls(
      formula = qtx~exp(lnh+g*Age)*(exp(g)-1)/g+l_m, 
      start=list(lnh=-0.1,g=0.1,l_m=0),
      data = .
    ))),
    params = map(model, ~data.frame(t(.$parameters[,1])))
  ) %>% unnest(params) %>% 
  mutate(
    m = (log(g) - lnh)/g,
    b = 1/g,
    l_m = l_m*1E5
  ) -> Stage1_model

Stage1_model %>% compute_table1 -> Table1
for (gender in Stage1_model$Gender %>% unique) {
  Table1 %>% filter( Gender == gender ) %>%
    write_csv(paste0("./data/01_nls_model/table_1_", gender, '.csv'))
}

# Table 2
Stage1_model %>% compute_stage2 -> Stage2_model
Stage2_model %>% compute_table2 %>% write_csv("./data/01_nls_model/table2.csv")

# Table 3
Stage2_model %>% compute_stage3 %>% compute_table3 %>% 
  write_csv("./data/01_nls_model/table3.csv")




