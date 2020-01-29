# Ensure we are starting with a clean-slate
rm(list=ls())

# Load used libraries
library(readr)
source("./scripts/02_display/00_methods.R")

# Output
create_tables45_in_pdf(
  Stage1_model = read_rds("./data/02_display/historic_stage1.rds"),
  Stage2_model = read_rds("./data/02_display/historic_stage2.rds"),
  out_file = "./reports/Historical_Tables/Historic-Tables.tex"
)



