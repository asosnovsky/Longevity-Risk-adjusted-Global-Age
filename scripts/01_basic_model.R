rm(list=ls())
library(readr)
library(tidyverse)
library(broom)

dataset = read_csv("./data/2011_qx_data.csv", col_types = cols(
  Year = col_integer(),
  Age = col_character(),
  Country = col_factor(),
  StatName = col_factor(),
  .default = col_double()
))

# Constants
lambda_multiplier = 10^5
epsilon = 1E-10
x = 35:95

dataset %>% 
  # limit the data to be between the ages of 35 to 95
  filter( Age %in% paste0(x) ) %>% 
  #filter( StatName == "Female" ) %>%
  group_by(Country, StatName) %>% nest %>%  
  # Run the following on each country/gender combination
  mutate(
    # Create a sequence of lambdas between 0 and the minimum lambda 
    #  note that Value == qx
    lambdas = map(data, ~ seq(0, min(.$Value), 1/lambda_multiplier) ),
    # fit a linear model against each lambda
    # then keep the model-lambda combination with the best fit 
    model = map2(data, lambdas, function(data, lambdas) {
      qx = data$Value
      lapply(lambdas, function(lambda) {
        y = log(log(1/(1-qx))-lambda+epsilon)
        list(model=lm(y~x), lambda=lambda)
      }) %>% reduce(function(l, r) {
        if( sigma(l$model) < sigma(r$model) ) l
        else r
      })
    })
  ) %>%
  # compute parameters
  mutate(
    lambda_makeham = map_dbl(model, ~.$lambda)*lambda_multiplier,
    K0 = map_dbl(model, ~.$model$coefficients[1]),
    g = map_dbl(model, ~.$model$coefficients[2]),
    lnh = K0 - log(exp(g-1)/g)
  ) -> 
  Table1

# Take a look at  Table - 1
Table1 %>% ungroup() %>% 
  left_join(read_csv("./data/01_processed/country_codes.csv")) %>% 
  arrange(Country, StatName) %>% 
  select(`Country Name`, StatName, lambda_makeham, K0, g, lnh) %>% 
  View("Table - 1 -")

# Construct Table 2
Table1 %>% ungroup() %>%
  # Drop the columns with model and lambdas
  select(-c(model, data, lambdas)) %>% 
  # Group by gender
  group_by(StatName) %>% nest %>% 
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
  Table2

# Take a look at Table 2
Table2 %>% ungroup() %>% 
  select(-model) %>% 
  View("Table - 2 -")
