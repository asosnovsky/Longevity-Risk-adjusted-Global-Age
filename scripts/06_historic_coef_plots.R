# Ensure we are starting with a clean-slate
rm(list=ls())

# Load used libraries
library(readr)
source("./scripts/00_method.R")

# Read in the modelled data
Stage1_model <- read_rds("./data/03_historic/stage1-lg.rds")

# Util functions
pretty_names_s1 <- function(s1) s1 %>% mutate( stat = case_when(
  stat == "b" ~"Dispersion Coefficient (b)",
  stat == "m" ~ "Modal Value (m)"
))
pretty_names_s2 <- function(s2) s2 %>% mutate( stat = case_when(
  stat == "x*" ~ "Plateau Age (x*)",
  stat == "L" ~ "Intercept (L)",
  stat == "G" ~ "Mortality Growth Rate (G)"
))
save_plots <- function(p, name="images/fig") {
  postscript(paste0(name, ".eps"), family = "serif")
  plot(p)
  dev.off()
  ggsave(paste0(name, ".jpg"), plot = p)
}

# 1947
Stage1_model %>% 
  filter( Gender != "Total" ) %>%
  apply_filters(from_year = 1945, to_year = 2011, delta_year = 1) -> 
  prep_data1945

# 1900
Stage1_model %>% 
  filter( Gender != "Total" ) %>%
  apply_filters(from_year = 1900, to_year = 2011, delta_year = 1) -> 
  prep_data1900

full_join(
  prep_data1900 %>% distinct(`Country Name`) %>% mutate(in1900=T),
  prep_data1945 %>% distinct(`Country Name`) %>% mutate(in1945=T)
) %>% mutate(in1900 = replace(in1900, is.na(in1900), F)) %>%
  write_csv("data/03_historic/countries_avail.csv")
#%>% select(`Country Name`)

# =====================
# Figure 5
# =====================
prep_data1945 %>%
  prep_display_data_stage1 %>%
  filter( stat != "lnh" ) %>% pretty_names_s1 %>%
  history_plot("Figure 5a: Historic Gompertz Coefficients", x_breaks = 5) %>%
  save_plots("images/figure-5/with-title/fig5a")

prep_data1945 %>% 
  prep_display_data_stage2 %>% pretty_names_s2 %>%
  history_plot("Figure 5c: Historic CLaM Parameters", x_breaks=5) %>%
  save_plots("images/figure-5/with-title/fig5c")

prep_data1900 %>%
  prep_display_data_stage1 %>%
  filter( stat != "lnh" ) %>% pretty_names_s1 %>%
  history_plot("Figure 5b: Historic Gompertz Coefficients", x_breaks = 5) %>%
  save_plots("images/figure-5/with-title/fig5b")

prep_data1900 %>% 
  prep_display_data_stage2 %>% pretty_names_s2 %>%
  history_plot("Figure 5d: Historic CLaM Parameters", x_breaks=5) %>%
  save_plots("images/figure-5/with-title/fig5d")

# =====================
# Figure 5 - no titles
# =====================

prep_data1947 %>%
  prep_display_data_stage1 %>%
  filter( stat != "lnh" ) %>% pretty_names_s1 %>%
  history_plot("Figure 5a: Historic Gompertz Coefficients", x_breaks = 5, no_title = T) %>%
  save_plots("images/figure-5/no-title/fig5a")

prep_data1947 %>% 
  prep_display_data_stage2 %>% pretty_names_s2 %>%
  history_plot("Figure 5c: Historic CLaM Parameters", x_breaks=5, no_title = T) %>%
  save_plots("images/figure-5/no-title/fig5c")

prep_data1900 %>%
  prep_display_data_stage1 %>%
  filter( stat != "lnh" ) %>% pretty_names_s1 %>%
  history_plot("Figure 5b: Historic Gompertz Coefficients", x_breaks = 5, no_title = T) %>%
  save_plots("images/figure-5/no-title/fig5b")

prep_data1900 %>% 
  prep_display_data_stage2 %>% pretty_names_s2 %>%
  history_plot("Figure 5d: Historic CLaM Parameters", x_breaks=5, no_title = T) %>%
  save_plots("images/figure-5/no-title/fig5d")

