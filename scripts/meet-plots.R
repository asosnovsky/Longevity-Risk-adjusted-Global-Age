
dataset %>%
  apply_filters(
    from_year = 1925, to_year = 2011,
    delta_year = 1, 
    from_age = 35, to_age = 95
  ) %>%
  distinct( `Country Name`, Year ) -> 
  countries

print(countries %>% distinct(`Country Name`))
print(paste0("There are ", nrow(countries%>% distinct(`Country Name`)), " countries"))


# add average line for each coef V
# similar plot for E[m] and E[b] and E[makeham] V
# save in eps and jpg
# - apply model on cohorts tables (eg the 1925-cohort for as most countries as posibble)
# nest week 11AM

Stage1_model %>% 
  filter( Gender != "Total" ) %>%
  inner_join(countries,
    by=c("Country Name", "Year")
  ) %>% 
  compute_stage2 %>% 
  compute_s2_param_list  %>% 
  group_by(Gender, stat) %>% mutate( mean_value = mean(value) ) %>%
  filter(stat != "l*") %>%
  ggplot(aes(x=Year,y=value)) +
  facet_grid(stat~Gender, scale='free') +
  geom_line() +
  geom_ribbon(aes(ymin=upper, ymax=lower), alpha=0.2) +
  geom_hline(aes(yintercept = mean_value), linetype="dashed") +
  theme(axis.title.y=element_blank()) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) +  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8))
