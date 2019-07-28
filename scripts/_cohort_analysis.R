cdataset %>% 
  filter(between(Age, 35, 95)) %>%
  filter(between(Year, 1911, 2011)) %>%
  filter(!is.na(qx))  %>%
  group_by(Year, Country) %>% summarise(max(Age)) %>%
  ggplot(aes(Year, `max(Age)`, group=Country)) + geom_line()
  

group_by(Year, Country, Gender) %>%
  filter( n() == (95-35+1) )
