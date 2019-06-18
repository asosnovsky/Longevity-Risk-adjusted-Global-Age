# Ensure we are starting with a clean-slate
rm(list=ls())

# Set the seed for any future randomization needs
set.seed(0)

# Load used libraries
library(readr)
library(tidyverse)
library(broom)
library(scales)

# Read in the merged dataset for 2011
dataset = read_csv("./data/01_processed/2011_qx_data.csv", 
   col_types = cols(
       Year = col_integer(),
       Age = col_integer(), # note that this includes a value of "110+", this value is not used at the moment
       Country = col_character(),
       StatName = col_character(),
       .default = col_double()
     )
)


# Set Constants
lambda_multiplier = 10^5
epsilon = 1E-10

#################################################
# Stage One - Estimating Sub-Group Parameters
#################################################
dataset %>% rename(Gender=StatName, qx=Value) %>%
  # Limit to Just Ages between 35 to 95
  filter(between(Age, 35, 95)) %>% 
  # Generate initial lambda values
  mutate( l = log(1/(1-qx)) ) %>%
  # Group by Country and Gender, so that we may run the operations on each of the sub-groups
  group_by(Country, Gender) %>% nest() %>% 
  mutate(
    model = map(data, function(data) lapply(
      # Create a sequence of potential accidental death lambdas
      seq(from=0, to=min(data$l), by=1/lambda_multiplier), 
      function(l_m) list(
        # Fit a linear-regression model for each acc-death lambda per sub-group
        model = lm(y~x, data=data_frame(
          y = log(data$l-l_m+epsilon),
          x = data$Age
        )), 
        l_m = l_m
        # Only keep the best model per sub-group
      )) %>% reduce(function(l, r) {
        if( sigma(l$model) < sigma(r$model) ) l
        else r
      })
    )
  ) %>% 
  # Extract Model Parameters
  mutate(
    K0 = map_dbl(model, ~.$model$coefficients[1]),
    K1 = map_dbl(model, ~.$model$coefficients[2]),
    l_m = map_dbl(model, ~.$l_m),
    g = K1,
    lnh = K0 - log( exp(K1-1)/K1 )
  ) -> Stage1_model

Stage1_model %>% write_rds("./data/02_models/stage1.rds")

#################################################
# Stage Two - CLaM
#################################################
Stage1_model %>% 
  # Run the second regression on each gender group (note: one of the groups is "Total")
  group_by(Gender) %>% nest %>% 
  mutate(
    # Run the lin-reg
    model = map(data, ~summary(lm( lnh~g, data=. )) ),
    # Extract computed params
    L = map_dbl(model, ~.$coefficients[1,1]),
    `x*` = -map_dbl(model, ~.$coefficients[2,1]),
    G = map_dbl(data, ~mean(.$g))
  ) -> Stage2_model

Stage2_model %>% write_rds("./data/02_models/stage2.rds")

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
