# Ensure we are starting with a clean-slate
rm(list=ls())

# Load used libraries
library(readr)
source("./scripts/00_method.R")

# Read in the modelled data
Stage1_model <- read_rds("./data/03_historic/stage1-lg.rds")
Stage2_model <- read_rds("./data/03_historic/stage2-lg.rds")
Stage3_model <- read_rds("./data/03_historic/stage3-lg.rds")

Stage2_model %>%
  mutate(N = map_dbl(data, ~length(unique(.$`Country Name`)))) %>%
  ggplot(aes(Year, N, group=Gender)) +
  geom_line()

Stage2_model %>% filter( Gender != "Total" ) %>%
  compute_s2_param_list -> tmp

tmp %>%  
  ggplot(aes(x=Year)) +
  facet_grid(stat~Gender, scale='free') +
  geom_line(aes(y=value)) +
  geom_ribbon(aes(ymin=upper, ymax=lower), alpha=0.2) +
  theme(axis.title.y=element_blank())

ggsave("./reports/Historical Modelling/figure5.png", dpi='retina')

convert_stat_latex = ~paste0(
  "$\\bf ", number(.$value, acc=0.001), "$ ",
  "$\\pm ", number(.$std, acc=0.001), "$"
)
tmp %>% select(-c(data, model, l_m)) %>%
  group_by(Year, Gender, stat) %>% nest %>%
  spread(stat, data) %>%
  mutate(
    latex_x = map_chr(`x*`, convert_stat_latex),
    latex_L = map_chr(`L`, convert_stat_latex),
    latex_p = map_chr(`l*`, ~paste0("$(", number(.$lower, acc=0.01), ",", number(.$upper, acc=0.01), ")$" )),
    latex_G = map_chr(G, ~paste0("$", number(.$value, acc=0.001), "$ "))
  ) %>%
  group_by(Year, Gender) %>% nest %>%
  spread(Gender, data) ->
  tmp2
tmp2 %>%
  mutate(
    latex = paste0(
      map_chr(Male, ~.$latex_x), "& ",
      map_chr(Female, ~.$latex_x), "& ",
      map_chr(Male, ~.$latex_L), "& ",
      map_chr(Female, ~.$latex_L)
    )
  ) %>%
  select(Year, latex) %>%
  filter( Year %in% seq(1960, 2011, by=3) ) %>%
  apply(1, function(r) {
    cat(r[["Year"]], "& ", r[["latex"]], "\\\\ \\hline\\hline\n", 
        file="./reports/Historical Modelling/table5a.txt", append = T)
  })
tmp2 %>%
  mutate(
    latex = paste0(
      map_chr(Male, ~.$latex_p), "& ",
      map_chr(Female, ~.$latex_p), "& ",
      map_chr(Male, ~.$latex_G), "& ",
      map_chr(Female, ~.$latex_G)
    )
  ) %>%
  select(Year, latex) %>%
  filter( Year %in% seq(1960, 2011, by=3) ) %>%
  apply(1, function(r) {
    cat(r[["Year"]], "& ", r[["latex"]], "\\\\ \\hline\\hline\n", 
        file="./reports/Historical Modelling/table5b.txt", append = T)
  })

