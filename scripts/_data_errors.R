# Clear memory
rm(list=ls())

# Dependencies
source("./scripts/00_method.R")

# Load Data
dataset <- read_csv("./data/01_processed/full_qx_data.csv")

dataset %>% filter(between(Age, 35, 95)) %>%
  filter(qx == 0) %>%
  group_by(`Country Name`, Year, Gender) %>%
  summarise(n(), min(Age), max(Age))

dataset %>% 
  filter(between(Age, 35, 95)) %>%
  filter(Year >= 1911) %>%
  group_by(Country, Gender, Year) %>%
  filter(is.na( mean(qx) )) %>%
  summarise(n())

dataset %>% filter(between(Age, 35, 95)) %>%
  filter(Year >= 1911) %>%
  filter(qx > 1) %>%
  group_by(`Country Name`, Country, Year, Gender) %>%
  summarise(n(), min(Age), max(Age))

dataset %>% filter(Country == "FIN") %>% filter(Year == 1911) %>%
  filter(Gender == "Male") %>% filter(between(Age, 35, 95)) %>%
  ggplot() + geom_line(aes(y=qx,x=Age))



