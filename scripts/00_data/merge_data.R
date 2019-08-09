##############
#
# This file will recreate the joined datasets used in this analysis
# Specifically the following tables:
#  - ./data/01_processed/2011_qx_data.csv
#  - ./data/01_processed/full_qx_data.csv
#
# Please make sure to grab the "death_rates.zip" file from 
#  https://www.mortality.org/cgi-bin/hmd/hmd_download.php
# Under the "PERIOD DATA" / Death Rates row
# Then extract the contents of the zip file into "./data/00_raw/death_rates"

rm(list=ls())
library(readr)
library(tidyverse)

country_codes = read_csv("./data/01_processed/country_codes.csv")

# extract_method
into_dataset <- function(ccodes, data_path, ext = ".Mx_1x1.txt") lapply(
  ccodes, function(ccode) {
    read_table2(paste0(data_path, "/", ccode, ext),
                col_types = cols(
                  Year = col_integer(),
                  Age = col_character(),
                  .default = col_double()
                ), na='.', skip = 1
    ) %>% mutate( Country = ccode )
  }) %>% 
  reduce(bind_rows) %>% 
  gather( Gender, qx, -c(Country, Year, Age) ) %>% 
  mutate( Age = replace(Age, Age == "110+", 110) ) %>% 
  mutate( Age = as.numeric(Age) )

country_codes$CountryCode %>%
  into_dataset("./data/00_raw/death_rates/Mx_1x1", ".Mx_1x1.txt")  %>% 
  inner_join(country_codes, by=c("Country"="CountryCode")) %>% 
  rename( `Country Name` = CountryName ) -> 
  dataset

dataset %>% filter( Year == 2011 ) %>% 
  write_csv("./data/01_processed/2011_qx_data.csv")

dataset %>% 
  write_csv("./data/01_processed/full_qx_data.csv")

list.files("./data/00_raw/cdeath_rates") %>% 
  map_chr(~str_remove(., ".cMx_1x1.txt")) %>%
  into_dataset("./data/00_raw/cdeath_rates", ".cMx_1x1.txt") -> 
  cdataset

cdataset %>%
  write_csv("./data/00_raw/full_qx_cdata.csv")
