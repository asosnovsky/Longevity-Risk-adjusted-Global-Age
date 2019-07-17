# Clear memory
rm(list=ls())

# Dependencies
source("./scripts/00_method.R")
library(foreach)
library(doMC)

# Load Data
dataset <- read_csv("./data/01_processed/full_qx_data.csv")

# Filter dataset to expected range
#  also remove Iceland and Luxemburg due to cases where qx == 0
#
dataset %>% 
  filter(!(Country %in% c("ISL", "LUX"))) %>% 
  apply_filters(
    from_year = 1960,
    to_year = 2011,
    delta_year = 1,
    from_age = 35,
    to_age = 95
  ) -> 
  fdataset

# Define Process
compute_stage1 <- function(d) d %>% 
  mutate( l = log(1/(1-qx)) ) %>% 
  group_by(Year, Gender, `Country Name`) %>% nest %>% 
  mutate(model = map(data, ~lapply( 
    seq(1E-5, min(.$l), by=1E-5),
    function(l_m) compute_model1(., l_m)
  ) %>% reduce(bind_rows)
  ) ) %>%
  mutate( optimal = map(model, ~filter(., sigma==min(sigma))) ) %>% 
  unnest(optimal) 

# init parallel compute
registerDoMC(parallel::detectCores())

# define chunks
grps = fdataset %>% distinct(Country, Year)

# Fit model
save_data_path = "./data/03_historic/stage1/"
out = foreach(i = 1:nrow(grps), .errorhandling='pass', .verbose=T, .combine = c) %do% {
  fdataset %>%
    filter( Country == grps[i,]$Country ) %>%
    filter( Year == grps[i,]$Year ) %>%
    compute_stage1 %>%
    write_rds(paste0(
      save_data_path, 
      grps[i,]$Country, "-",
      grps[i,]$Year, ".rds"))
  T
}

# Check
if (any(!out)) {
  print("The following failed")
  print(grps[!out,])
  stop("The above failed!")
}

# Merge outputs
grps <- grps %>% unite(grp, Country, Year, sep='-') %>% {.$grp}
Stage1_model <- lapply(grps, function(grp) 
  read_rds(paste0(save_data_path, grp, ".rds"))
) %>% reduce(bind_rows)
# save stage 1
Stage1_model %>% write_rds("./data/03_historic/stage1.rds")

#################################################
# Stage Two - CLaM
#################################################
Stage1_model %>% compute_stage2 -> Stage2_model
Stage2_model %>% write_rds("./data/03_historic/stage2.rds")


#################################################
# Stage Three+Four - Compute B-Age
#################################################
Stage2_model %>% compute_stage3 -> Stage3_model
Stage3_model %>% write_rds("./data/03_historic/stage3.rds")


