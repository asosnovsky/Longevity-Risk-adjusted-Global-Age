################################
### This file takes the 2011 output model data
### and updates the latex table
### output file can be found here 
### "./reports/Revised Tables/MABA_Tables_Revised_Ariel.tex"
################################

# Ensure we are starting with a clean-slate
rm(list=ls())

# Load used libraries
source("./scripts/02_display/00_methods.R")

# Output file
create_tables123_in_pdf(
  Stage1_model = readr::read_rds("./data/01_models/stage1.rds") %>% filter( Gender != "Total" ),
  Stage2_model = readr::read_rds("./data/01_models/stage2.rds") %>% filter( Gender != "Total" ),
  Stage3_model = readr::read_rds("./data/01_models/stage3.rds") %>% filter( Gender != "Total" ),
  out_file = "reports/2011_Tables/2011_Tables.tex"
)

