# Ensure we are starting with a clean-slate
rm(list=ls())

# Load used libraries
source("./scripts/02_display/00_methods.R")

# Prep Data
readr::read_rds("data/01_models/historic-stage1.rds") %>% 
  filter( Gender != "Total" ) %>%
  apply_filters(from_year = 1945, to_year = 2011, delta_year = 1) ->
  prep_data

# Output
create_tables45_in_pdf(
  Stage1_model = prep_data %>% prep_display_data_stage1,
  Stage2_model = prep_data %>% prep_display_data_stage2,
  out_file = "./reports/Historical_Tables/Historic-Tables.tex"
)



