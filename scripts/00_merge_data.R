rm(list=ls())
library(readr)
library(tidyverse)

data_path = "./data/00_raw/death_rates/Mx_1x1"
country_codes = read_csv("./data/01_processed/country_codes.csv")

dataset = lapply(country_codes$CountryCode, function(ccode) {
  read_table2(paste0(data_path, "/", ccode, ".Mx_1x1.txt"),
    col_types = cols(
      Year = col_integer(),
      Age = col_character(),
      .default = col_double()
    ), na='.', skip = 1
  ) %>% mutate( Country = ccode )
}) %>% reduce(bind_rows) %>% 
  filter( Year == 2011 ) %>% 
  gather(Gender, qx, -c(Country, Year, Age)) %>% 
  mutate(Age = replace(Age, Age == "110+", 110)) %>% 
  mutate( Age = as.numeric(Age) )

dataset %>% 
  write_csv("./data/01_processed/2011_qx_data.csv")
