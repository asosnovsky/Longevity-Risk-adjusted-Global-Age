# Ensure we are starting with a clean-slate
rm(list=ls())

# Set the seed for any future randomization needs
set.seed(0)

# Load used libraries
library(readr)
library(tidyverse)
library(broom)


# Read in the merged dataset for 2011
dataset = read_csv("./data/01_processed/2011_qx_data.csv", 
   col_types = cols(
      Year = col_integer(),
      Age = col_integer(), # note that this includes a value of "110+", this value is not used at the moment
      Country = col_character(),
      StatName = col_factor(),
      .default = col_double()
  ))
  

# Set Constants
lambda_multiplier = 10^5
epsilon = 1E-10

#################################################
# Stage One - Estimating Sub-Group Parameters
#################################################
dataset %>% rename(qx = Value, x = Age, Gender = StatName) %>% 
  # limit the data to be between the ages of 35 to 95
  filter( between(x, 35, 95) ) %>% 
  # Run the following on each country/gender combination
  group_by(Country, Gender) %>% nest %>% mutate(
    # Create a sequence of lambdas between 0 and the minimum lambda 
    lambdas = map(data, ~ seq(0, min(.$qx), 1/lambda_multiplier) ),
    # fit a linear model against each lambda
    # then keep the model-lambda combination with the best fit 
    model = map2(data, lambdas, function(data, lambdas) {
      lapply(lambdas, function(lambda) {list(
          model  = lm(y~x, data=data_frame(
            y = log(log(1/(1-data$qx))-lambda+epsilon),
            x = data$x
          )), 
          lambda = lambda
      )}) %>% reduce(function(l, r) {
        if( sigma(l$model) < sigma(r$model) ) l
        else r
      })
    })
  ) %>%
  # Compute Parameters
  mutate(
    lambda_makeham = map_dbl(model, ~.$lambda)*lambda_multiplier,
    K0 = map_dbl(model, ~.$model$coefficients[1]),
    g = map_dbl(model, ~.$model$coefficients[2]),
    lnh = K0 - log(exp(g-1)/g)
  ) -> 
  Stage01_results

#################################################
# Stage One - View and Save Results
#################################################
# Take a look at  Table - 1
Stage01_results %>% ungroup() %>% 
  left_join(read_csv("./data/01_processed/country_codes.csv")) %>% 
  arrange(Country, Gender) %>% 
  select(`Country Name`, Gender, lambda_makeham, K0, g, lnh) ->
  Table1

Table1 %>% View("Table - 1 -")
Table1 %>% write_csv("data/02_paper_tables/Table-01.csv")

#################################################
# Stage Two - CLaM
#################################################
Stage01_results %>% ungroup() %>%
  # Drop the columns with model and lambdas
  select(-c(model, data, lambdas)) %>% 
  # Group by gender
  group_by(Gender) %>% nest %>% 
  mutate(
    # Run the second regression
    model = map(data, ~lm(lnh~g, data=.)),
    # Extract parameters and statistics
    data = map2(data, model, ~data_frame(
      L = .y$coefficients[1],
      `x*` = -.y$coefficients[2],
      g_mean = mean(.x$g),
      g_min = min(.x$g),
      g_max = max(.x$g),
      r.squared = summary(.y)$r.squared,
      N = nrow(.x)
    ))
  ) %>% unnest(data) ->
  Stage2_data

#################################################
# Stage Two - View and Save Results
#################################################
Stage2_data %>% ungroup() %>% 
  select(-model) ->
  Table2

Table2 %>% View("Table - 2 -")
Table2 %>% write_csv("data/02_paper_tables/Table-02.csv")
  
