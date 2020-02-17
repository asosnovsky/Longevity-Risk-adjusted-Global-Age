
##############
#
# This file will recreate the joined datasets used in this analysis
# Specifically the following tables:
#  - ./data/01_processed/2011_qx_data.csv
#  - ./data/01_processed/full_qx_data.csv
#
# It will download the data files straight from the website
# Please note that it makes use of the "Authorization" header that the website generates
# You may need to fetch this header from the webpages headers in the developer console (the one here could expire)
#
# If you want to recreate this by first going to the website and then downloading the files manually use the `parse_downloaded_zip` 
# function instead of the `get_data_from_url`, but make sure to first point to where you downloaded the two files
#

rm(list=ls())
library(readr)
library(tidyverse)

###############################
# Helper Functions
###############################

parse_downloaded_zip <- function(fol, subfolder) {
  file_paths = paste0(fol, "/", subfolder)
  file_paths = paste0(file_paths, '/', dir(file_paths))
  lapply(
    file_paths,
    function(fpath) read_table2(fpath,
        col_types = cols(
          Year = col_integer(),
          Age = col_character(),
          qx = col_double()
        ), 
        na='.', 
        skip = 1
    ) %>% 
      select(Year, Age, qx) %>%
      mutate( Age = replace(Age, Age == "110+", 110) ) %>% 
      mutate( Age = as.numeric(Age) ) %>%
      mutate( Country = strsplit(basename(fpath), "\\.")[[1]][1] )
  ) %>% reduce(bind_rows)
}
get_data_from_url <- function(link, subfolder, authorization = "Basic YXJpZWxAc29zbm92c2t5LmNhOmFsXFgoMkVfajszVzM=") {
  
  if (tolower(substr(readline(paste0("download the file `", basename(link) ,"`? (y/N): ")), 0, 1)) == "y") {
    browseURL(link) 
  }
  
  print(paste0("Please select where you downloaded `", basename(link), "` to? "))
  fname <- file.choose()
  if (tolower(tools::file_ext(fname)) != "zip") {
    stop("Invalif File Type")
  }
  temp_fol <- paste0(tempdir(), "/", basename(link))
  unzip(fname, exdir=temp_fol)
  data <- parse_downloaded_zip(temp_fol, subfolder)
  
  return(data)
}

###############################
# Parse Data
###############################
country_codes = read_csv("./data/00_raw/country_codes.csv")

bind_rows(
  get_data_from_url(
    link="https://www.mortality.org/hmd/zip/by_statistic/lt_male.zip", 
    subfolder="mltper_1x1"
  ) %>% mutate(Gender = "Male"),
  get_data_from_url(
    "https://www.mortality.org/hmd/zip/by_statistic/lt_female.zip", 
    "fltper_1x1"
  ) %>% mutate(Gender = "Female")
) %>% arrange(Country, Gender, Year, Age) %>% 
  inner_join(country_codes, by=c("Country"="CountryCode")) %>%
  rename( `Country Name` = CountryName ) -> 
  dataset

dataset %>% filter( Year == 2011 ) %>% 
  write_csv("./data/00_raw/2011_qx_data.csv")

dataset %>% 
  write_csv("./data/00_raw/full_qx_data.csv")


