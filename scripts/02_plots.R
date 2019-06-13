library(ggplot2)

Stage01_results %>% filter(Gender %in% c("Male", "Female")) %>% 
  ggplot(aes(
    x=g, y=lnh
  )) +
  facet_wrap(~Gender, nrow=2, scales="free") + 
  geom_point() +
  geom_text(
    aes(label=Country),
    hjust=0, vjust=0,
    size = 3
  ) +
  geom_smooth(
    method = 'lm', se = FALSE,
    formula = y~x
  )

dataset %>% filter(StatName %in% c("Male", "Female")) %>% 
  filter(between(Age, 35, 95)) %>% 
  ggplot(aes(
    x=Age, y=Value
  )) + 
  facet_wrap(
    ~StatName, nrow = 2
  ) +
  geom_point()

dataset %>% filter(StatName == "Total") %>% 
  filter(between(Age, 35, 95)) %>% 
  ggplot(aes(
    x=Age, y=Value
  )) + 
  geom_point()
