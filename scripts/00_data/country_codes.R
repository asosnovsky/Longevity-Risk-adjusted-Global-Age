#
# This file will generate the raw country-codes from a manually-extracted
# csv file
#
#

rm(list=ls())
library(readr)
library(tidyverse)

country_codes <- read_csv("./data/00_raw/country_codes.csv")
country_codes %>% 
  separate(`Country (Code)`, c("CountryName", "CountryCode"), " \\(") %>% 
  mutate( CountryCode = str_remove(CountryCode, "\\)") ) %>% 
  write_csv("./data/01_processed/country_codes.csv")
