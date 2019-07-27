# Ensure we are starting with a clean-slate
rm(list=ls())

# Load used libraries
library(readr)

# Read in the modelled data
Stage1_model <- read_rds("./data/04_historic_display/stage1.rds") %>%
  mutate(
    stat = case_when(
      stat == "b" ~"Dispersion Coefficient (b)",
      stat == "m" ~ "Modal Value (m)",
      stat == "lnh" ~ "Log Hazard Rate (ln(h))"
    )
  )
Stage2_model <- read_rds("./data/04_historic_display/stage2.rds") %>%
  mutate(
    stat = case_when(
      stat == "x*" ~ "Plateau Age (x*)",
      stat == "L" ~ "Intercept (L)",
      stat == "G" ~ "Mortality Growth Rate (G)"
    )
  )

# =====================
# Figure 5
# =====================

# Plot Function
history_plot <- function(d, title) d %>% 
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
  scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) +  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8))  + 
  labs(title = paste0(
    title,
    " (", min(d$Year), " to ", max(d$Year), ")"
  )) +
  theme(plot.title = element_text(hjust = 0.5, family="serif"))


# Plot
postscript("images/fig5a.eps", family = "serif")
Stage1_model %>% history_plot("Figure 5a: Historic Gompez Coefficients")
dev.off()
ggsave("images/fig5a.jpg")
postscript("images/fig5b.eps", family = "serif")
Stage2_model %>% history_plot("Figure 5b: Historic CLaM Parameters")
dev.off()
ggsave("images/fig5b.jpg")
