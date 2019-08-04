# Ensure we are starting with a clean-slate
rm(list=ls())

# Load used libraries
library(readr)
source("./scripts/00_method.R")

# Read in the modelled data
Stage1_model <- read_rds("./data/03_historic/cstage1-lg.rds")

# Keep only cases where all data is there (95-35+1 == 61)
Stage1_model %>%
  mutate( N = map_dbl(data, ~sum(!is.na(.$qx)))) %>%
  filter(N == 61) -> 
  Stage1_model

# Prep Data
constricted_years = seq(1921, 1978, 1)
Stage1_model %>% 
  filter( Gender != "Total" ) %>%
  inner_join(
    tibble(Year= constricted_years),
    by="Year"
  ) %>% 
  group_by(`Country`) %>% arrange(Year) %>% nest %>% 
  mutate(
    ycounts = map_chr(data, ~paste(unique(.$Year), collapse=','))
  ) %>% 
  filter(
    ycounts == paste( constricted_years, collapse = ',')
  ) %>% 
  select(-ycounts) %>% 
  unnest(data) -> 
  pdata

pdata %>%
  group_by(Year, Gender) %>% nest %>%
  mutate(
    b = map_dbl(data, ~mean(.$b)),
    m = map_dbl(data, ~mean(.$m))
    #,    N = map_dbl(data, ~length(unique(.$`Country Name`)))
  ) %>%
  gather(stat, value, -c(Year, Gender, data)) %>%
  mutate(
    std = case_when(
      stat == "b" ~ map_dbl(data, ~sd(.$b)),
      stat == "m" ~ map_dbl(data, ~sd(.$m))
    ),
    upper = value + 2*std,
    lower = value - 2*std,
    stat = case_when(
      stat == "b" ~"Dispersion Coefficient (b)",
      stat == "m" ~ "Modal Value (m)"
    )
  ) -> s1_prep

pdata %>% 
  group_by(Year, Gender) %>%
  compute_stage2 %>%
  gather(stat, value, -c(Year, Gender, model, data)) %>%
  mutate(
    std = case_when(
      stat == "L" ~ map_dbl(model, ~.$coefficients[1,2]),
      stat == "x*" ~ map_dbl(model, ~.$coefficients[2,2]),
      stat == "G" ~ 0
    ),
    upper = value + 2*std,
    lower = value - 2*std,
    stat = case_when(
      stat == "x*" ~ "Plateau Age (x*)",
      stat == "L" ~ "Intercept (L)",
      stat == "G" ~ "Mortality Growth Rate (G)"
    )
  ) -> s2_prep

# Plot Function
history_plot <- function(d, title) d %>% 
  group_by(Gender, stat) %>% 
  mutate( mean_value = mean(value), N = map_dbl(data, ~length(unique(.$`Country`))) ) %>%
  ggplot(aes(x=Year,y=value)) +
  facet_grid(stat~Gender, scale='free') +
  geom_line() +
  geom_ribbon(aes(ymin=upper, ymax=lower), alpha=0.2) +
  geom_hline(aes(yintercept = mean_value), linetype="dashed") +
  geom_label(aes( 
    y=Inf, 
    label=paste0(
      "mean = ", round(mean_value, 3), 
      ", N = ", N
    ),
    x=-Inf,
    vjust = 1.15,
    hjust = -0.025
  )) +
  theme(axis.title.y=element_blank()) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) +  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8))  + 
  labs(title = paste0(
    title,
    " (", min(d$Year), " to ", max(s1_prep$Year), ")"
  )) +
  theme(plot.title = element_text(hjust = 0.5, family="serif"))


# Plot
postscript("images/fig6a.eps", family = "serif")
s1_prep %>% history_plot("Figure 6a: Historic Gompez Coefficients - Cohorts")
dev.off()
ggsave("images/fig6a.jpg")
postscript("images/fig5b.eps", family = "serif")
s2_prep %>% history_plot("Figure 6b: Historic CLaM Parameters - Cohorts")
dev.off()
ggsave("images/fig6b.jpg")
