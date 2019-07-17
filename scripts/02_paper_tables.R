# Ensure we are starting with a clean-slate
rm(list=ls())

# Set the seed for any future randomization needs
set.seed(0)

# Load used libraries
library(readr)
source("./scripts/00_method.R")

# Read in the modelled data
Stage1_model <- read_rds("./data/02_models/stage1.rds")
Stage2_model <- read_rds("./data/02_models/stage2.rds")
Stage3_model <- read_rds("./data/02_models/stage3.rds")


# Helper Format Function
lmb_format = function(l) paste0(round(l*lambda_multiplier), "x10^-", log10(lambda_multiplier))

# Table 1
Stage1_model %>% mutate( l_m = paste0(l_m*1E5) ) %>% 
  compute_table1 -> Table1

for (gender in Stage1_model$Gender %>% unique) {
  Table1 %>% filter( Gender == gender ) %>%
    write_csv(paste0("./data/02_paper_tables/table_1_", gender, '.csv'))
}

# Table 2
Stage2_model %>% compute_table2 -> Table2
write_csv(Table2, "./data/02_paper_tables/table2.csv")

# Table 3
Stage3_model %>% compute_table3 ->
  Table3

write_csv(Table3, "./data/02_paper_tables/table3.csv")
