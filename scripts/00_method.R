# Required Libraries
library(scales)
library(tidyverse)

# --------------------------
# This file contains various functions used throughout this research
# It ensures that all processes used are the same and are standardized,
# regardless of when we apply them
# --------------------------

## Section 1 - Utility functions

### This function will ensure that only countries that have data from 
### the provided start-date and end-date (consecutively) will be kept
apply_filters <- function(dt, from_year, to_year, delta_year) {
  constricted_years = seq(from_year, to_year, delta_year)
  dt %>% 
    inner_join( tibble(Year= constricted_years), by="Year" ) %>% 
    group_by(`Country Name`) %>% 
    arrange(Year) %>% nest %>% 
    mutate( ycounts = map_chr(data, ~paste(unique(.$Year), collapse=',')) ) %>% 
    filter( ycounts == paste( constricted_years, collapse = ',') ) %>% 
    select(-ycounts) %>% 
    unnest(data)
}

## Section 2 - Model Functions

### This function computes the result of the first regression, 
### however it requires that you first specify a lambda_m value
### for the optimize function that will try all values of lambda and return 
### a more optimized value see `compute_stage1`
compute_model1 <- function(dt, lambda_makeham) dt %>% 
  mutate( y = log(l-lambda_makeham) ) %>% nest %>% 
  mutate(
    # Fit model
    model = map(data, ~summary(lm(y~Age ,data=.))),
    # Extracts params
    l_m = lambda_makeham,
    params = map(model, ~tibble(
      K0 = .$coefficients[1],
      g = .$coefficients[2],
      lnh = as.numeric(K0-log((exp(g)-1)/g)),
      m = (log(g) - lnh)/g,
      b = 1/g,
      sigma = .$sigma
    ))
  ) %>% 
  unnest(params) 

### This function will take a simple narrow data-set, that is assumed to be
### already pre-grouped and filtered, and apply `stage 1` as described in the paper
###   - First it will compute the log-inverse of qx
###   - Then it will generate sample l_m values starting with 1E-5
###   - Then it will fit the first regression against all lambdas
###   - Lastly it will filter out the best model best on the standard-error measure
compute_stage1 <- function(d) d %>% 
  mutate( l = log(1/(1-qx)) ) %>% nest %>% 
  mutate(model = map(data, ~lapply( 
    seq(1E-5, min(.$l), by=1E-5),
    function(l_m) compute_model1(., l_m) %>% select(-c(data, model))
  ) %>% reduce(bind_rows)
  ) ) %>%
  mutate( optimal = map(model, ~filter(., sigma==min(sigma))) ) %>% 
  unnest(optimal) 

### This function will take the results of the `compute_stage1` function
### and will compute the second stage as described in the paper
###   - first it will fit a regression of lnh vs g
###   - then it will extract the L and x* parameters
compute_stage2 <- function(stage1_model) stage1_model %>% nest %>% 
  mutate(
    # Run the lin-reg
    model = map(data, ~summary(lm( lnh~g, data=. )) ),
    # Extract computed params
    L = map_dbl(model, ~.$coefficients[1,1]),
    `x*` = -map_dbl(model, ~.$coefficients[2,1]),
    G = map_dbl(data, ~mean(.$g))
  )

### This function will take the results of the `compute_stage2` function
### and will compute the third and fourth stages as described in the paper
###   - first it will exctract the standard errors for x*
###   - then it will compute a parameter k_i
###   - lastly it will be the extracted information to compute Biological Age
compute_stage3 <- function(stage2_model) stage2_model %>% 
  mutate(
    x_stdev = map_dbl(model, ~.$coefficients[2,2]),
    `x* - higher` = `x*` + 2*x_stdev,
    `x* - lower` = `x*` - 2*x_stdev,
  ) %>% 
  select(-model) %>%
  unnest(data) %>% 
  mutate(  ki = (g/G)-1 ) %>% unnest(data) %>% 
  mutate(
    B_Age = Age - ki*(`x*`-Age),
    B_Age_Upper = Age - ki*(`x* - higher`-Age),
    B_Age_Lower = Age - ki*(`x* - lower`-Age)
  ) -> Stage3_model

## Section 3 - "Prettifying Functions"
## The functions in this section simply modify the extraction of various stages
## round and format the numeric values and provide clean neater looking tables

compute_table1 <- function(stage1_model) stage1_model %>% 
  mutate( g = g %>% percent(accuracy = 0.001) ) %>% 
  mutate_if(is.numeric, ~round(., 3)) %>% arrange(Gender) %>% 
  select(Gender, `Country Name`, lnh, l_m, g, m, b)

compute_table2 <- function(stage2_model) stage2_model %>% mutate(params = map(model, ~tibble(
  `6_Adj. R^2` = .$adj.r.squared %>% percent,
  `2_L Std.Err` = .$coefficients[1,2],
  `3_-x* Std.Err` = .$coefficients[2,2],
  `4_L t-value` = .$coefficients[1,3],
  `5_-x* t-value` = .$coefficients[2,3],
))) %>% unnest(params) %>% 
  mutate(params = map(data, ~tibble(
    `7_Range: g` = paste0("(", percent(min(.$g)), ', ', percent(max(.$g)), ')' ),
    `9_Countries` = nrow(.)
  ))) %>% unnest(params) %>% 
  mutate_if(is.numeric, ~round(., 3)) %>% 
  rename(
    `0_L` = L,
    `1_x*` = `x*`,
    `8_G` = G
  ) %>% 
  select(-data, -model) %>% 
  gather(stat, value, -Gender) %>% 
  spread(Gender, value) %>% 
  separate(stat, c("idx", "Statistic"), sep='_') %>% 
  select(-idx)

compute_table3 <- function(stage3_model) stage3_model %>% 
  filter( Age %in% c(55, 70, 85) ) %>% 
  mutate( Age = paste0(Gender, "\n", "x = ", Age) )  %>% 
  select(`Country Name`, Age, B_Age) %>% 
  spread(Age, B_Age) %>% 
  mutate_if(is.numeric, ~round(., 3))

compute_s2_param_list <- function(stage2_model) stage2_model %>%
  mutate(
    `l*` = L
  ) %>%
  gather(stat, value, -c(Year, Gender, model, data)) %>%
  mutate(
    std = case_when(
      stat == "L" ~ map_dbl(model, ~.$coefficients[1,2]),
      stat == "l*" ~ map_dbl(model, ~.$coefficients[1,2]),
      stat == "x*" ~ map_dbl(model, ~.$coefficients[2,2]),
      stat == "G" ~ 0
    ),
    upper = value + 2*std,
    lower = value - 2*std
  ) %>% 
  mutate(
    l_m = map_dbl(data, ~mean(.$l_m)),
    value = case_when(
      stat == "l*" ~ exp(value) - l_m,
      TRUE ~ value
    ),
    upper = case_when(
      stat == "l*" ~ exp(upper) - l_m,
      TRUE ~ upper
    ),
    lower = case_when(
      stat == "l*" ~ exp(lower) - l_m,
      TRUE ~ lower
    )
  ) 


## Section 4 -  Plotting

### This function takes the output of `compute_stage1` and provide a table with 
### the variables `b` `m` and `lnh` with their confidence intervals
prep_display_data_stage1 <- function(s1) s1 %>%
  group_by(Year, Gender) %>% nest %>%
  mutate(
    b = map_dbl(data, ~mean(.$b)),
    m = map_dbl(data, ~mean(.$m)),
    lnh = map_dbl(data, ~mean(.$lnh))
    #,    N = map_dbl(data, ~length(unique(.$`Country Name`)))
  ) %>%
  gather(stat, value, -c(Year, Gender, data)) %>%
  mutate(
    std = case_when(
      stat == "b" ~ map_dbl(data, ~sd(.$b)),
      stat == "m" ~ map_dbl(data, ~sd(.$m)),
      stat == "lnh" ~ map_dbl(data, ~sd(.$lnh))
    ),
    upper = value + 2*std,
    lower = value - 2*std
  )


### This function takes the output of `compute_stage1` and provide a table with 
### the variables `L` `x*` and `G` with their confidence intervals
prep_display_data_stage2 <- function(s1) s1 %>%
  group_by(Year, Gender) %>%
  compute_stage2 %>%
  gather(stat, value, -c(Year, Gender, model, data)) %>%
  mutate(
    std = case_when(
      stat == "L" ~ map_dbl(model, ~.$coefficients[1,2]),
      stat == "x*" ~ map_dbl(model, ~.$coefficients[2,2]),
      stat == "G" ~ 0
    ),
    upper = value + 2*std,
    lower = value - 2*std
  )

### This function takes one of the outputs of the "prep_display_data_stage*" functions
### and provide a historical plot (this assumes the dataframe has historic data)
history_plot <- function(d, title, x_breaks=12, y_breaks=8, no_title=F) d %>% 
  group_by(Gender, stat) %>% 
  mutate( mean_value = mean(value), N = map_dbl(data, ~length(unique(.$`Country Name`))) ) %>%
  ggplot(aes(x=Year,y=value)) +
  facet_grid(stat~Gender, scale='free') +
  geom_line() +
  geom_ribbon(aes(ymin=upper, ymax=lower), alpha=0.2) +
  geom_hline(aes(yintercept = mean_value), linetype="dashed") +
  geom_label(aes( 
    y=Inf, 
    label=paste0(
      "mean = ", round(mean_value, 3), 
      ", N = ", N
    ),
    x=-Inf,
    vjust = 1.15,
    hjust = -0.025
  )) +
  theme(axis.title.y=element_blank()) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = x_breaks)) +  
  scale_y_continuous(breaks = scales::pretty_breaks(n = y_breaks))  + 
  labs(title = ifelse(no_title, "", paste0(
    title,
    " (", min(d$Year), " to ", max(d$Year), ")"
  ))) +
  theme(plot.title = element_text(hjust = 0.5, family="serif"))
