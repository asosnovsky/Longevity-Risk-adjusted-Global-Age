#############################
# This files runs the analysis for
# data between the years of 1900 to 2011
# I included the ability to this on multiple cpu-cores
# However the default will only use 1-cpu core to aviod complexity
# (to use multiple cpu-cores change the function in line 55 %do% to %dopar% and set the value
#  on line 40 in the function registerDoMC to the number of cpus in your machine
#  please do not run anything on your computer while its running like this!!)
#
# without optimization on multiple-cores expect this script to run for 8 hours
# if the script fails at any moment, do not worry, it keeps backups on your hard-disk
# so you can always pause and resume!
#############################

# Clear memory
rm(list=ls())

# Dependencies
source("./scripts/00_method.R")
library(foreach)
library(doMC)

# Load Data
dataset <- read_csv("./data/01_processed/full_qx_data.csv")

# Clean up dataset
dataset %>%
  # Filter dataset to expected range
  filter(between(Age, 35, 95)) %>%
  filter(between(Year, 1900, 2011)) %>%
  # Remove cases where qx == 0
  filter(qx > 0) %>%
  # Remove null cases
  group_by(Year, Gender, Country) %>% filter( sum(!is.na(qx)) == (95-35+1) ) %>% ungroup %>%
  # cases where qx -> 1 change to qx = 0.98 to avoid log(-1) cases
  mutate( qx = case_when(qx >= 0.98 ~ 0.98, T ~ qx) ) -> 
  fdataset

# init parallel compute
registerDoMC(2)

# ==============
# Stage 1 - Period
# ==============
# define chunks
grps = fdataset %>% distinct(Country, Year)

# Fit model
save_data_path = "./data/02_models/historic_stage1/"
clear_folder(save_data_path)
out = foreach(
  i = 1:nrow(grps), 
  .errorhandling='pass', 
  .verbose=T, 
  .combine = c
) %do% {
  save_path = paste0(
    save_data_path, 
    grps[i,]$Country, "-",
    grps[i,]$Year, ".rds")
  if ( !file.exists(save_path) ) {
    fdataset %>%
      filter( Country == grps[i,]$Country ) %>%
      filter( Year == grps[i,]$Year ) %>%
      group_by(Year, Gender, Country, `Country Name`) %>% 
      compute_stage1 %>%
      write_rds(save_path) 
  }
  T
}

out %>% reduce(c) %>% as.logical -> out

# Check
if (any(!out)) {
  print("The following failed")
  print(grps[!out,])
  stop("The above failed!")
}

# Merge outputs
grps <- grps %>% unite(grp, Country, Year, sep='-') %>% {.$grp}
Stage1_model <- lapply(grps, function(grp) {
  read_rds(paste0(save_data_path, grp, ".rds"))
}) %>% reduce(bind_rows)

# Save stage-1-
Stage1_model %>% write_rds("./data/02_models/historic-stage1.rds")
rm(grps)
