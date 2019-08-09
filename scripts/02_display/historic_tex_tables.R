# Ensure we are starting with a clean-slate
rm(list=ls())

# Load used libraries
library(readr)

# Read in the modelled data
historic_stage1 <- read_rds("./data/02_models/historic-stage1.rds")
Stage2_model <- read_rds("./data/04_historic_display/stage2.rds")

# Constants
out_folder = "./reports/Historical Modelling/"
table4a_loc <- paste0(out_folder, "table4a.txt")
table4b_loc <- paste0(out_folder, "table4b.txt")

# =====================
# Table 4
# =====================
# Utils method
convert_stat_latex = ~paste0(
  "$\\bf ", number(.$value, acc=0.01), "$ ",
  "$\\pm ", number(.$std, acc=0.01), "$"
)
# prep data
Stage1_model %>% select(-c(data)) %>%
  group_by(Year, Gender, stat) %>% nest %>%
  spread(stat, data) %>%
  mutate(
    latex_b = map_chr(`b`, convert_stat_latex),
    latex_m = map_chr(`m`, convert_stat_latex),
    latex_lnh = map_chr(`lnh`, convert_stat_latex)
  ) %>%
  group_by(Year, Gender) %>% nest %>%
  spread(Gender, data) ->
  s1_prep

Stage2_model %>% select(-c(data, model)) %>%
  group_by(Year, Gender, stat) %>% nest %>%
  spread(stat, data) %>%
  mutate(
    latex_x = map_chr(`x*`, convert_stat_latex),
    latex_L = map_chr(`L`, convert_stat_latex),
    latex_G = map_chr(G, ~paste0(" $", number(.$value, acc=0.0001), "$"))
  ) %>%
  group_by(Year, Gender) %>% nest %>%
  spread(Gender, data) ->
  s2_prep

# Table 5a
file.remove(table4a_loc)
s1_prep %>%
  mutate(
    latex = paste0(
      map_chr(Male, ~.$latex_m), "& ",
      map_chr(Female, ~.$latex_m), "& ",
      map_chr(Male, ~.$latex_b), "& ",
      map_chr(Female, ~.$latex_b)
    )
  ) %>%
  select(Year, latex) %>%
  filter( Year %in% seq(1945, 2011, by=3) ) %>%
  apply(1, function(r) {
    cat(r[["Year"]], "& ", r[["latex"]], "\\\\ \\hline\\hline\n", 
        file=table4a_loc, append = T)
  })

# Table 5a
file.remove(table4b_loc)
s2_prep %>%
  mutate(
    latex = paste0(
      map_chr(Male, ~.$latex_x), "& ",
      map_chr(Female, ~.$latex_x), "& ",
      map_chr(Male, ~.$latex_L), "& ",
      map_chr(Female, ~.$latex_L), "& ",
      map_chr(Male, ~.$latex_G), "& ",
      map_chr(Female, ~.$latex_G)
    )
  ) %>%
  select(Year, latex) %>%
  filter( Year %in% seq(1945, 2011, by=3) ) %>%
  apply(1, function(r) {
    cat(r[["Year"]], "& ", r[["latex"]], "\\\\ \\hline\\hline\n", 
        file=table4b_loc, append = T)
  })
