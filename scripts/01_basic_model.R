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

# Helper Functions
lmb_format = function(l) paste0(round(l*lambda_multiplier), "x10^-", log10(lambda_multiplier))

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
    lambda_min = map_dbl(data, ~min(.$lambda)),
    lambda_mean = map_dbl(data, ~mean(.$lambda)),
    lambda_max = map_dbl(data, ~max(.$lambda)),
    lambda_makeham = map_dbl(model, ~.$lambda_makeham),
    K0 = map_dbl(model, ~.$model$coefficients[1]),
    g = map_dbl(model, ~.$model$coefficients[2]),
    lnh = K0 - log(exp(g-1)/g),
    b = 1/g,
    m = (log(g)-lnh)/g
  ) -> 
  Stage01_results

#################################################
# Stage Two - CLaM
#################################################
# Setup a function for the continous mortality (equation 15)
lxg = function(x, g, xx, lmd, lmd_m) ifelse(x < xx , lmd + (lmd_m-lmd)*exp(g*(x-xx)), lmd_m )
# Compute Global parameters
Stage01_results %>% ungroup() %>%
  # Group by gender
  group_by(Gender) %>% nest %>% 
  mutate(
    # Run the second regression
    model = map(data, ~summary(lm(lnh~g, data=.))),
    # Extract parameters and statistics
    parameters = map(model, ~data_frame(
      L = .$coefficients[1],
      `x*` = -.$coefficients[2],
      adj.r.squared = .$adj.r.squared
    )),
    data = map2(data, parameters, ~.x %>% group_by(Country) %>% mutate(
      lxg = map(data, function(d) lxg(
        x = 35:95,
        xx = .y$`x*`,
        lmd_m = lambda_makeham,
        lmd = d$lambda,
        g = g
      ))
    ))
  ) %>% 
  unnest(parameters) ->
  Stage02_results



#################################################
# Save Results
#################################################
# Take a look at  Table - 1
Stage02_results %>% 
  select(Gender, data) %>% unnest(data) %>% 
  left_join(read_csv("./data/01_processed/country_codes.csv")) %>% 
  group_by(Gender, `Country Name`) %>% 
  summarise(
    `λ_0` = unique(lambda_0) %>% lmb_format(),
    `Makeham: λ` = lambda_makeham %>% lmb_format,
    `λ_55` = unique(lambda_55),
    g = percent(g, accuracy = 0.01),
    m = round(m, 2),
    b = round(b, 2),
    lnh = round(lnh, 3)
  ) -> 
  Table1

#Table1 %>% View("Table - 1 -")
Table1 %>% write_csv("data/02_paper_tables/Table-01.csv")

# Take a look at  Table - 2
Stage02_results %>% ungroup() %>% 
  mutate(
    `Adj. R2` = adj.r.squared %>% percent(acc =0.01),
    `Range: g` = map_chr(data, ~paste0("(", percent(min(.$g)), ', ', percent(max(.$g)), ')' )),
    `Plateau: λ*` = map_chr(data, ~paste0("(", min(.$lambda_min), ', ', round(max(.$lambda_max), 2), ')' )),
    `Average: g` = map_dbl(data, ~mean(.$lambda_mean)),
    Countries = map_dbl(data, nrow)
  ) %>% 
  mutate(
    params = map(model, ~data_frame(
      `L Std.Err` = .$coefficients[1,2],
      `-x* Std.Err` = .$coefficients[2,2],
      `L t-value` = .$coefficients[1,3],
      `-x* t-value` = .$coefficients[2,3],
    ))
  ) %>% unnest(params) %>% 
  mutate_if(is.numeric, ~round(., 2)) %>% 
  select(-data, -model) -> 
  Table2

#Table2 %>% View("Table - 2 -")
Table2 %>% write_csv("data/02_paper_tables/Table-02.csv")
  
