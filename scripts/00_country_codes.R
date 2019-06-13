rm(list=ls())
library(readr)
library(tidyverse)

data_path = "./data/00_raw/death_rates/Mx_1x1"


lapply(dir(data_path), function(fname) {
  data_frame(
     Country = str_extract(fname, '^([A-Z_]+)'),
    `Country Name` = read_lines(paste0(data_path, '/', fname), n_max = 1) %>% 
      str_extract('^([\\w ]+)')
  )
}) %>% 
  reduce(bind_rows) %>% 
  write_csv("./data/01_processed/country_codes.csv")

