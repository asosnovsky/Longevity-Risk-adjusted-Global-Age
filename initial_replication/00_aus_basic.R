# Ensure Memory is clear (no prior variables)
rm(list = ls())

# Libraries used
library(readxl)

# constants
x<-35:95

# Read Dataset
World_Mortality_2011 <- read_excel("initial_replication/World_Mortality_2011.xlsx", skip = 1)

# Extract qx
qx = World_Mortality_2011$AUS_M[x+1]
# Fixed makeham  
lambda_makeham <- 20*10^(-5) 
# This is the error minimizing value.
y<-log(log(1/(1-qx))-lambda_makeham) 
# Final pick for the regression.
fit<-lm(y~x)
# Compute Params
K0 = fit$coefficients[1]
g = fit$coefficients[2]
lnh = as.numeric(K0-log((exp(g)-1)/g))
m = (log(g) - lnh)/g
b = 1/g

# Save output
data.frame(
  g=g %>% percent(acc=0.001), 
  lnh=lnh %>% round(3),
  m = m %>% round(2),
  b = b %>% round(2)
) %>% t %>% write.csv("./initial_replication/00_aus_m_basic.csv")
