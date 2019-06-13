rm(list=ls())
library(readr)
library(tidyverse)

data_path = "./data/00_raw/death_rates/Mx_1x1"

dataset = lapply(dir(data_path), function(file_name) {
  read_table2(paste0(data_path, "/", file_name),
    col_types = cols(
      Year = col_integer(),
      Age = col_character(),
      .default = col_double()
    ), na='.', skip = 1
  ) %>% 
    mutate( Country = str_extract(file_name, '^([A-Z_]+)') )
}) %>% reduce(bind_rows) %>% 
  filter( Year == 2011 )


dataset %>% 
  gather(StatName, Value, -c(Country, Year, Age)) %>% 
  write_csv("./data/01_processed/2011_qx_data.csv")
