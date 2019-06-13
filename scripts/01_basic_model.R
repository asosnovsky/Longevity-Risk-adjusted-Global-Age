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
  group_by(Country, Gender) %>% 
  # Compute Lambdas
  mutate(
    lambda = log(1/(1-qx))
  ) %>% nest %>% mutate(
    # Create a sequence of lambdas between 0 and the minimum lambda 
    lambdas_makeham = map(data, ~ seq(0, min(.$qx), 1/lambda_multiplier) ),
    # fit a linear model against each lambda
    # then keep the model-lambda combination with the best fit 
    model = map2(data, lambdas_makeham, function(data, lambdas_makeham) {
      lapply(lambdas_makeham, function(lmbda_make) {list(
          model  = lm(y~x, data=data_frame(
            y = log(data$lambda-lmbda_make+epsilon),
            x = data$x
          )), 
          lambda_makeham = lmbda_make
      )}) %>% reduce(function(l, r) {
        if( sigma(l$model) < sigma(r$model) ) l
        else r
      })
    })
  ) %>%
  # Compute Parameters
  mutate(
    lambda_0 = map_dbl(data, ~.$lambda[1]),
    lambda_55 = map_dbl(data, ~.$lambda[56]),
    lambda_makeham = map_dbl(model, ~.$lambda_makeham),
    K0 = map_dbl(model, ~.$model$coefficients[1]),
    g = map_dbl(model, ~.$model$coefficients[2]),
    lnh = K0 - log(exp(g-1)/g),
    b = 1/g,
    m = (log(g)-lnh)/g
  ) -> 
  Stage01_results

#################################################
# Stage One - View and Save Results
#################################################
# Take a look at  Table - 1
Stage01_results %>% ungroup() %>% 
  left_join(read_csv("./data/01_processed/country_codes.csv")) %>% 
  arrange(Gender, Country) %>%
  mutate(
    `λ_0` = paste0(round(lambda_0*lambda_multiplier), "x10^-", log10(lambda_multiplier)),
    `Makeham: λ` = paste0(round(lambda_makeham*lambda_multiplier), "x10^-", log10(lambda_multiplier)),
    `λ_55` = percent(lambda_55, acc=0.01),
    g = percent(g, accuracy = 0.01),
    m = round(m, 2),
    b = round(b, 2),
    lnh = round(lnh, 3)
  ) %>% 
  select(
    Gender, `Country Name`,
    lnh, `λ_0`, `Makeham: λ`, g, `λ_55`, 
    m, b
  ) -> 
  Table1

Table1 %>% View("Table - 1 -")
Table1 %>% write_csv("data/02_paper_tables/Table-01.csv")

#################################################
# Stage Two - CLaM
#################################################
Stage01_results %>% ungroup() %>%
  # Drop the columns with model and lambdas
  select(-c(model, data, lambdas_makeham)) %>% 
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
      `Adj. R2` = summary(.y)$adj.r.squared,
      N = nrow(.x)
    ))
  ) %>% unnest(data) ->
  Stage02_results

#################################################
# Stage Two - View and Save Results
#################################################
Stage02_results %>% ungroup() %>% 
  select(-model) ->
  Table2

Table2 %>% View("Table - 2 -")
Table2 %>% write_csv("data/02_paper_tables/Table-02.csv")
  
