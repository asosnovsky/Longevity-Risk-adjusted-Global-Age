rm(list=ls())

# Load used libraries
library(readr)
library(tidyverse)
source("./scripts/00_method.R")

# Read in the merged dataset for 2011
dataset = read_csv("./data/01_processed/2011_qx_data.csv") %>% 
  # Remove Iceland as its too much of an outlier
  filter( !(Country %in% c("ISL") ) )

emp_dataset = dataset %>%
  filter(between(Age, 35, 95)) %>% 
  mutate( qtx = -log( 1-qx ) ) %>% 
  group_by(Gender, Country) %>% nest %>% 
  unite(id, Country, Gender) %>% 
  spread(id, data)

U <- function(qx, x,n=length(x)) function(par) {
  l = par[1]*1E-5; h = exp(par[2]); g = par[3];
  sum(
    qx - h*(exp(g)-1)*exp(g*x)/g - l
  )^2
}
gs_otm <- function(qtx, Age) {
  lapply(
    seq(0, 0.5, by=0.1), function(g) lapply(
      seq(-50, 0, by=1), function(lnh) lapply(
        seq(0, min(qtx)*1E5, by=5), function(l) {
          tibble(
            g=g,l=l,lnh=lnh, 
            U=U(qtx,Age)(c(l,lnh,g))
          )
        }
      ) %>% reduce(bind_rows)
    ) %>% reduce(bind_rows)
  ) %>% reduce(bind_rows) %>% 
    filter(U == min(U, na.rm=T))
}

dataset %>% 
  filter(between(Age, 35, 95)) %>% 
  mutate( qtx = -log( 1-qx ) ) %>% 
  group_by( `Country Name`, Gender ) %>% nest %>% {
    pb = pb = progress_estimated(nrow(.)*2)
    mutate(., optm = map(data, function(d) {
      pb$tick()$print()
      out = gs_otm(d$qtx, d$Age)
      pb$tick()$print()
      out
    }))
  } ->
  stage1_model

stage1_model %>% unnest(optm) %>% rename(l_m=l) %>% 
  mutate(
    m = (log(g) - lnh)/g,
    b = 1/g
  ) %>% compute_table1 

stage1_model %>% unnest(optm) %>% 
  group_by(Gender) %>% nest %>% 
  mutate( 
    model = map(data, ~summary(lm( lnh~g, data=. )) ),
    L = map_dbl(model, ~.$coefficients[1,1]),
    `x*` = -map_dbl(model, ~.$coefficients[2,1]),
    G = map_dbl(data, ~mean(.$g))
  ) -> stage2_model

stage2_model %>% compute_table2


