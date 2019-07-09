library(ggstance)

dir_save = "reports/Revised\ Tables/"

country_codes = read_csv("./data/01_processed/country_codes.csv") %>% 
  rename(`Country Name` = CountryName)

Stage1_model %>% inner_join(country_codes, by="Country Name") %>% 
  filter(Gender == "Female") %>% 
  rename(`lnh[i]`=lnh, `g[i]`=g) %>% 
  ggplot(aes(`g[i]`, `lnh[i]`)) + geom_point(color='red') +
  geom_smooth(size=1/4, method='lm', se=F, col='black') +
  geom_text(aes(label=`CountryCode`), size=2, nudge_x=-.0007)

ggsave(paste0(dir_save, "fig1_female.png"))

Stage1_model %>% inner_join(country_codes, by="Country Name") %>% 
  filter(Gender == "Male") %>% 
  rename(`lnh[i]`=lnh, `g[i]`=g) %>% 
  ggplot(aes(`g[i]`, `lnh[i]`)) + geom_point(color='blue') +
  geom_smooth(size=1/4, method='lm', se=F, col='black') +
  geom_text(aes(label=`CountryCode`), size=2, nudge_x=-.0007)

ggsave(paste0(dir_save, "fig1_male.png"))

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
  ), fill='#0a0a0a') +
  geom_vline(xintercept = 55, linetype="dotted")

ggsave(paste0(dir_save, "fig2_female.png"))

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
  ), fill='#0a0a0a') +
  geom_vline(xintercept = 55, linetype="dotted") 
  ggsave("test.png")

  ggsave(paste0(dir_save, "fig2_male.png"))