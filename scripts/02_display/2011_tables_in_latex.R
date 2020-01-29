################################
### This file takes the 2011 output model data
### and updates the latex table
### output file can be found here 
### "./reports/Revised Tables/MABA_Tables_Revised_Ariel.tex"
################################

# Ensure we are starting with a clean-slate
rm(list=ls())

# Load used libraries
library(readr)
source("./scripts/02_display/00_methods.R")

# Output file
create_tables123_in_pdf(
  Stage1_model = read_rds("./data/02_models/stage1.rds") %>% filter( Gender != "Total" ),
  Stage2_model = read_rds("./data/02_models/stage2.rds") %>% filter( Gender != "Total" ),
  Stage3_model = read_rds("./data/02_models/stage3.rds") %>% filter( Gender != "Total" ),
  out_file = "reports/2011_Tables/2011_Tables.tex"
)

