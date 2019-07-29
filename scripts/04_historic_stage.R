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

# filter(!(Country %in% c("LUX", "ISL", "BGR", "CHL", "BEL")))

# Define Process
compute_stage1 <- function(d) d %>% 
  mutate( l = log(1/(1-qx)) ) %>% 
  group_by(Year, Gender, Country, `Country Name`) %>% nest %>% 
  mutate(model = map(data, ~lapply( 
    seq(1E-5, min(.$l), by=1E-5),
    function(l_m) compute_model1(., l_m)
  ) %>% reduce(bind_rows)
  ) ) %>%
  mutate( optimal = map(model, ~filter(., sigma==min(sigma))) ) %>% 
  unnest(optimal) 

# init parallel compute
registerDoMC(2)

# ==============
# Stage 1 - Period
# ==============
# define chunks
grps = fdataset %>% distinct(Country, Year)

# Fit model
save_data_path = "./data/03_historic/stage1/"
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
Stage1_model %>% write_rds("./data/03_historic/stage1-lg.rds")
rm(grps)
