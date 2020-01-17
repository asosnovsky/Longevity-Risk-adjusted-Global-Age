###################
# This file will output the images saved under the images folder
###################

# Ensure we are starting with a clean-slate
rm(list=ls())

# Load used libraries
library(ggstance)
library(readr)
library(tidyverse)
library(scales)
source("./scripts/00_method.R")

# Read in the modelled data
Stage1_model <- read_rds("./data/02_models/stage1.rds")%>% filter( Gender != "Total" )
Stage2_model <- read_rds("./data/02_models/stage2.rds")%>% filter( Gender != "Total" )
Stage3_model <- read_rds("./data/02_models/stage3.rds")%>% filter( Gender != "Total" )

dir_save = "images/"

country_codes = read_csv("./data/01_processed/country_codes.csv") %>% 
  rename(`Country Name` = CountryName)

Stage1_model %>% inner_join(country_codes, by="Country Name") %>% 
  filter(Gender == "Female") %>% 
  rename(`lnh[i]`=lnh, `g[i]`=g) %>% 
  ggplot(aes(`g[i]`, `lnh[i]`)) + geom_point(color='red') +
  geom_smooth(size=1/4, method='lm', se=F, col='black') +
  geom_text(aes(label=`CountryCode`), size=2, nudge_x=-.0007) +
  labs(x="Mortality Growth Rate", y='Log of Initial Mortality Rate') + 
  ggtitle("Female") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(paste0(dir_save, "fig1_female.ps"))

Stage1_model %>% inner_join(country_codes, by="Country Name") %>% 
  filter(Gender == "Male") %>% 
  rename(`lnh[i]`=lnh, `g[i]`=g) %>% 
  ggplot(aes(`g[i]`, `lnh[i]`)) + geom_point(color='blue') +
  geom_smooth(size=1/4, method='lm', se=F, col='black') +
  geom_text(aes(label=`CountryCode`), size=2, nudge_x=-.0007) +
  labs(x="Mortality Growth Rate", y='Log of Initial Mortality Rate') + 
  ggtitle("Male") +
  theme(plot.title = element_text(hjust = 0.5))


ggsave(paste0(dir_save, "fig1_male.ps"))

Stage3_model %>% filter(Age == 55) %>% 
  filter(Gender == "Female") %>%  
  arrange(-B_Age_Lower) %>% 
  mutate(
    Country = as_factor(Country)
  ) %>% 
  ggplot() + geom_crossbarh(aes(
    y = Country,
    xmin = B_Age_Lower,
    xmax = B_Age_Upper,
    x = B_Age
  ), size=0.15, fill='#0a0a0a') +
  geom_vline(xintercept = 55, linetype="dotted") +
  ggtitle("FEMALE: Chronological Age 55") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(paste0(dir_save, "fig2_female.ps"))

Stage3_model %>% filter(Age == 55) %>% 
  filter(Gender == "Male") %>%  
  arrange(-B_Age_Lower) %>% 
  mutate(
    Country = as_factor(Country)
  ) %>% 
  ggplot() + geom_crossbarh(aes(
    y = Country,
    xmin = B_Age_Lower,
    xmax = B_Age_Upper,
    x = B_Age
  ), size=0.15 , fill='#0a0a0a') +
  geom_vline(xintercept = 55, linetype="dotted")+
  ggtitle("MALE: Chronological Age 55") +
  theme(plot.title = element_text(hjust = 0.5))


ggsave(paste0(dir_save, "fig2_male.ps"))