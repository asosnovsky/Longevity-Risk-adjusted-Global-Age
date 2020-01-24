# Ensure we are starting with a clean-slate
rm(list=ls())

# Load used libraries
library(readr)
source("./scripts/00_method.R")

# Read in the modelled data
Stage1_model <- read_rds("./data/02_display/historic_stage1.rds")
Stage2_model <- read_rds("./data/02_display/historic_stage2.rds")

# Constants
out_file = "./reports/Historical_Tables/Historic-Tables.tex"
clear_folder(dirname(out_file))


# =====================
# File Header
# =====================
cat(
  "\\documentclass[10pt, titlepage]{article}%
\\author{Ariel Sosnovsky}
\\usepackage{longtable}
\\usepackage{graphicx}

\\begin{document}
\\title{Historic Modelling}
\\maketitle

\\clearpage

\\begin{table}
  \\begin{center}
  \\begin{tabular}{||l|c|c|c|c||}
  \\hline\\hline
  \\multicolumn{5}{||c||}{Table \\# 4a } \\\\ \\hline\\hline
  \\multicolumn{5}{||c||}{{\\bf Gompertz Regression: Historical Coefficients}} \\\\ \\hline\\hline
  
  { } & 
  \\multicolumn{2}{|c|}{ \\bf Dispersion Coefficient $m$ } & 
  \\multicolumn{2}{|c|}{ \\bf Modal Value $b$ }\\\\ \\hline\\hline
  
  {\\bf Year } &
  {MALE} & {FEMALE} &
  {MALE} & {FEMALE} \\\\ \\hline\\hline
",
  file=out_file 
)


# =====================
# Table 4
# =====================
# Utils method
convert_stat_latex = ~paste0(
  "$\\bf ", number(.$value, acc=0.01), "$ ",
  "$\\pm ", number(2*.$std, acc=0.01), "$"
)
# prep data
Stage1_model %>% select(-c(data)) %>%
  group_by(Year, Gender, stat) %>% nest %>% ungroup() %>%
  pivot_wider(names_from=stat, values_from=data) %>%
  mutate(
    latex_b = map_chr(`b`, convert_stat_latex),
    latex_m = map_chr(`m`, convert_stat_latex),
    latex_lnh = map_chr(`lnh`, convert_stat_latex)
  ) %>%
  group_by(Year, Gender) %>% nest %>%
  pivot_wider(names_from=Gender, values_from=data) ->
  s1_prep

Stage2_model %>% select(-c(data, model)) %>%
  group_by(Year, Gender, stat) %>% nest %>%
  pivot_wider(names_from=stat, values_from=data) %>%
  mutate(
    latex_x = map_chr(`x*`, convert_stat_latex),
    latex_L = map_chr(`L`, convert_stat_latex),
    latex_G = map_chr(G, ~paste0(" $", number(.$value, acc=0.0001), "$"))
  ) %>%
  group_by(Year, Gender) %>% nest %>%
  pivot_wider(names_from=Gender, values_from=data) ->
  s2_prep

# Table 5a
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
    cat("  ", r[["Year"]], "& ", r[["latex"]], "\\\\ \\hline\\hline\n", 
        file=out_file, append = T)
  })

# In between area
cat("              
\\multicolumn{5}{||r||}{{\\em Source: Human Mortality Database, Period 1945-2011, 15 Countries}} \\\\ \\hline\\hline
\\end{tabular}
\\label{table5a}

Note: The table displays two standard errors for each statistic.
\\end{center}
\\end{table}

\\begin{table}
  \\begin{center}
  \\begin{tabular}{||l|c|c|c|c|c|c||}
  \\hline\\hline
  \\multicolumn{7}{||c||}{Table \\# 4b } \\\\ \\hline\\hline
  \\multicolumn{7}{||c||}{{\\bf CLaM Regression: Historical Coefficients}} \\\\ \\hline\\hline
  
  { } & 
  \\multicolumn{2}{|c|}{ \\bf Slope: $(x^{*})$ } & 
  \\multicolumn{2}{|c|}{ \\bf Intercept $(L)$ }  &
  \\multicolumn{2}{|c||}{ \\bf Mortality Rate $(G)$ } \\\\ \\hline\\hline
  
  {\\bf Year } &
  {MALE} & {FEMALE} &
  {MALE} & {FEMALE} &
  {MALE} & {FEMALE} \\\\ \\hline\\hline

", file=out_file, append=T)

# Table 5b
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
    cat("  ", r[["Year"]], "& ", r[["latex"]], "\\\\ \\hline\\hline\n", 
        file=out_file, append = T)
  })

# Footer
cat("        
\\multicolumn{7}{||r||}{{\\em Source: Human Mortality Database, Period 1945-2011, 15 Countries}} \\\\ \\hline\\hline
\\end{tabular}
\\label{table5a}

Note: The table displays two standard errors for each statistic.
\\end{center}
\\end{table}

\\end{document}",
file = out_file,
append = T
)