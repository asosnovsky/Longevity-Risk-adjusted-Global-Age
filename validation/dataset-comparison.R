# Ensure we are starting with a clean-slate
rm(list=ls())

# Load used libraries
library(readr)
library(tidyverse)
library(readxl)

# Read in the merged dataset for 2011
dataset = read_csv("./data/01_processed/2011_qx_data.csv")
mcoef = read_csv("./initial_replication/manual_coef.csv") %>% select(m_cc, CountryCode)
milvesky <- read_excel("initial_replication/World_Mortality_2011.xlsx", skip = 1) %>% 
  gather(Country_Gender, qx, -c(Year, Age)) %>% 
  separate(Country_Gender, c("Country", "Gender"), '_') 

dataset %>% 
  mutate( Age = str_remove(Age, '\\+') %>% as.numeric ) %>% 
  mutate( Gender = str_sub(Gender, 1, 1) ) %>% 
  mutate( qx = replace_na(qx, 0) ) %>% 
  select(Country, Year, Gender, Age, qx) %>% 
  inner_join(mcoef, by=c("Country"="CountryCode")) %>% 
  select(Country=m_cc, Year, Gender, Age, qx) -> mine

inner_join(
  mine, milvesky, 
  by=c("Country", "Year", "Gender", "Age") , 
  suffix=c('_mine', '_milv')
) %>% filter(Age < 100) %>%  mutate(
  d = abs(qx_mine-qx_milv)
) -> joined

joined %>% ggplot(aes(x=qx_mine, y=qx_milv, size=d)) + 
  geom_point() + ggsave("./validation/dataset-comparison.png")

joined %>% 
  summarise( mean(d, na.rm=T) )


