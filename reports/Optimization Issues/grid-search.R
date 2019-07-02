grid_search <- function(qtx, Age) {
  u = U(qtx, Age);  du = dU(qtx, Age);
  # generate the grid
  lapply(
    seq(0,0.5,l=50), function(g) lapply(
      seq(0,1E-2,l=100), function(H) lapply(
        seq(0,min(qtx),l=50), function(l) tibble(
          # Compute values
          g=g,l=l,H=H, 
          U=u(c(g,H,l)),
          dU=du(c(g,H,l))
        )
      ) %>% reduce(bind_rows)
    ) %>% reduce(bind_rows)
  ) %>% reduce(bind_rows) %>% 
    # Compute the rest of the values and format l and H
    mutate(
      l = l*1E5, H = H*1E5,
      h = g*H/(exp(g)-1),
      lnh = log(h),
      m = (log(g) - lnh)/g,
      b = 1/g
    )
}
emp_dataset = dataset %>%
  filter(between(Age, 35, 95)) %>% 
  mutate( qtx = -log( 1-qx ) ) %>% 
  group_by(Gender, Country) %>% nest %>% 
  unite(id, Country, Gender) %>% 
  spread(id, data)

with(emp_dataset$RUS_Male[[1]], { grid_search(qtx, Age) }) %>% 
  write_csv("./reports/Optimization Issues/emp_rus.csv")
with(emp_dataset$UKR_Male[[1]], { grid_search(qtx, Age) }) %>% 
  write_csv("./reports/Optimization Issues/emp_ukr.csv")
with(emp_dataset$CAN_Male[[1]], { grid_search(qtx, Age) }) %>% 
  write_csv("./reports/Optimization Issues/emp_can.csv")

emp_rus %>% filter( U == min(U) ) %>% distinct(g,l,H) %>% 
  mutate( `min(qx)` = min(data_russ_male$qtx)*10^5 )
emp_ukr %>% filter( U == min(U) ) %>% distinct(g,l,H) %>%
  mutate( `min(qx)` = min(data_ukr_male$qtx)*10^5 )
emp_can %>% filter( U == min(U) ) %>% distinct(g,l,H) %>%
  mutate( `min(qx)` = min(data_can_male$qtx)*10^5 )


