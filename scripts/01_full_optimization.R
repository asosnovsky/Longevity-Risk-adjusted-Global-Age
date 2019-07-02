# Ensure we are starting with a clean-slate
rm(list=ls())

# Set the seed for any future randomization needs
set.seed(0)

# Load used libraries
library(readr)
library(tidyverse)
source("./scripts/00_method.R")

# Read in the merged dataset for 2011
dataset = read_csv("./data/01_processed/2011_qx_data.csv") %>% 
  # Remove Iceland as its too much of an outlier
  filter(
    !(Country %in% c("ISL"))
  )

# Target Functions
U = function(qtx, Age) function(par) {
  l = par[1]; H = par[2]; g = par[3]
  sum( ( qtx - l - H*exp(g*Age) )^2 ) / (max(Age) - min(Age))
}
dU = function(qtx, Age) function(par) {
  l = par[1]; H = par[2]; g = par[3]
  n = max(Age) - min(Age)
  inner = qtx - l - H*exp(g*Age)
  dUdl = sum( inner*(-2)/n )
  dUdH = sum( inner*(-2)*exp(g*Age) / n )
  dUdg = sum( inner*(-2)*H*Age*g*exp(g*Age) / n )
  
  c(dUdl, dUdH, dUdg)
}
# Function to minimize the output
fit_model = function(qtx, Age) optim( 
  par=c( min(qtx)/2, 1E-5, 1E-2 ), # <- these starting points were chosen to avoid local-minimas
  fn=U(qtx, Age), 
  gr=dU(qtx, Age),
  lower = c(0, 0, 0),
  upper = c(min(qtx), 1E-4, 1), # Chosen emperically based on "zero-makeham" results
  method = 'L-BFGS-B',
  control = list(fnscale = c(1E3, 1, 2))
)

# Run optimization
dataset %>%
  filter(between(Age, 35, 95)) %>% 
  mutate(
    qtx = -log( 1-qx )
  ) %>% 
  group_by(Gender, Country) %>% nest %>% mutate(
    model = map(data, ~fit_model(.$qtx, .$Age)$par ),
    l = map_dbl(model, ~.[1]),
    H = map_dbl(model, ~.[2]),
    g = map_dbl(model, ~.[3]),
    h = g*H/(exp(g)-1),
    lnh = log(h),
    m = (log(g) - lnh)/g,
    b = 1/g,
    U = map2_dbl(data, model, ~U(.x$qtx, .x$Age)(.y))
  ) -> Stage1_model

Stage1_model %>% select(Gender, Country, g, H, l, lnh) %>% 
  mutate( l = l*1E5, H = H*1E5)

Stage1_model %>% ggplot() + geom_point(aes(lnh,g))

stop()

Stage1_model  %>% write_rds("./data/02_models/stage1_optim.rds")

#################################################
# Stage Two - CLaM
#################################################
Stage1_model %>% compute_stage2 -> Stage2_model

Stage2_model %>% write_rds("./data/02_models/stage2_optim.rds")

#################################################
# Stage Three+Four - Compute B-Age
#################################################
Stage2_model %>% 
  mutate(
    x_stdev = map_dbl(model, ~.$coefficients[2,2]),
    `x* - higher` = `x*` + 2*x_stdev,
    `x* - lower` = `x*` - 2*x_stdev,
  ) %>% 
  unnest(data) %>% 
  mutate(
    ki = (g/G)-1
  ) %>% unnest(data) %>% 
  mutate(
    B_Age = Age - ki*(`x*`-Age),
    B_Age_Upper = Age - ki*(`x* - higher`-Age),
    B_Age_Lower = Age - ki*(`x* - lower`-Age)
  ) -> Stage3_model

Stage3_model %>% write_rds("./data/02_models/stage3.rds")

