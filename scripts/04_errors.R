# Ensure we are starting with a clean-slate
rm(list=ls())

# Load used libraries
source("./scripts/00_method.R")

# Load Data

Stage1_model = read_rds("./data/01_models/stage1.rds")
Stage2_model = read_rds("./data/01_models/stage2.rds")

# Make Plot Data
inner_join(
  Stage2_model %>% ungroup %>%
    mutate(s2_out = map2(model, data, function(model, y) tibble(
      `Country Name`=y$`Country Name`,
      `Country Residual`=residuals(model),
      `C0` = model$coefficients[1,1],
      `C1` = model$coefficients[2,1],
      `C0 Std.Error` = model$coefficients[1,2],
      `C1 Std.Error` = model$coefficients[2,2]
    ))) %>%
    select(Gender, s2_out) %>%
    unnest(s2_out),
  Stage1_model %>% ungroup %>%
    mutate(s1_model = map(raw_model, summary)) %>%
    mutate(s1_out = map2(s1_model, data, function(model, y) tibble(
      Age=y$Age,
      `Age Residual` = residuals(model),
      `K0` = model$coefficients[1,1],
      `K1` = model$coefficients[2,1],
      `K0 Std.Error` = model$coefficients[1,2],
      `K1 Std.Error` = model$coefficients[2,2]
    ))) %>%
    select(Gender, `Country Name`, s1_out) %>%
    unnest(c(s1_out))
) %>%
  mutate(`Country Name` = as_factor(`Country Name`)) -> 
  joined_data



joined_data %>%
  mutate(`Country Name` = as_factor(`Country Name`)) %>%
  group_by(Gender, `Country Name`) %>%
  mutate(`Age Residual (Mean)` = sd(`Age Residual`)) %>%
  #filter(Gender=="Female") %>%
  group_by(Gender) %>%
  save_plots_by_group(
    #just_plot = T,
    name="images/errors/m1_m2_res/m1_m2_res",
    exts=c('.png'),
    plot_fcn = function(d, key) ggplot(d, aes(
      x = reorder(`Country Name`, `Country Residual`)
    )) +
      geom_boxplot(aes(y=`Age Residual`), size=0.1, outlier.shape = NA) +
      geom_point(aes(y = `Country Residual`), size=0.5, color='blue') +
      coord_flip() +
      labs(
        y="Residuals", x = "",
        title = paste0("GM vs CLaM Regression Residuals - ", key),
        subtitle = "Distribution of GM Regression Residuals (eij) vs CLaM Residuals (ej)"
      ) + 
      theme(
        axis.text.x = element_text(size = 2),
        plot.title = element_text(hjust = 0.25),
        plot.subtitle = element_text(size = 8, hjust = 0.25)
      )
  )

joined_data %>%
  select(Gender, `Country Name`, `K1 Std.Error`, `Country Residual`) %>%
  distinct() %>%
  #filter(Gender=="Female") %>%
  group_by(Gender) %>%
  save_plots_by_group(
    #just_plot = T,
    name="images/errors/ageerr_m2_res/ageerr_m2_res",
    exts=c('.png'),
    plot_fcn = function(d, key) {
      ggplot(d, aes(x = `Country Residual`)) +
        geom_point(aes(
          y=`K1 Std.Error`
        )) +
        labs(
          x="CLaM Residuals (ej)",
          title=paste0("K1 Std.Error vs CLaM Residuals - ", key)
        ) + 
        theme(
          plot.title = element_text(hjust = 0.5)
        )
    }
  )

joined_data %>%
  select(Gender, `Country Name`, `K0 Std.Error`, `Country Residual`) %>%
  distinct() %>%
  group_by(Gender) %>%
  save_plots_by_group(
    name="images/errors/ageeintrr_m2_res/ageinterr_m2_res",
    exts=c('.png'),
    plot_fcn = function(d, key) {
      ggplot(d, aes(x = `Country Residual`)) +
        geom_point(aes(
          y=`K0 Std.Error`
        )) +
        labs(
          x="CLaM Residuals (ej)",
          title=paste0("K0 Std.Error vs CLaM Residuals - ", key)
        ) + 
        theme(
          plot.title = element_text(hjust = 0.5)
        )
    }
  )

joined_data %>% group_by(Gender) %>%
  save_plots_by_group(
    name="images/errors/residuals/residuals",
    exts=c('.png'),
    plot_fcn = function(d, k) ggplot(d, aes(
        y=`Age Residual`, 
        x=`Country Residual`
      )) +
      geom_point(size=0.1) + 
      labs(
        x="Age Residual (eij)", y="Country Residual (ej)",
        title = paste0("Residual Plot - ",  k)
      ) + 
      theme(
        plot.title = element_text(hjust = 0.5)
      ) +
      coord_flip() 
  )


joined_data %>%
  group_by(`Country Name`, Gender) %>%
  summarise(
    `K0 Std.Error` = unique(`K0 Std.Error`),
    `K1 Std.Error` = unique(`K1 Std.Error`),
    `Country Residual` = unique(`Country Residual`),
    `Age Residual (Mean)` = mean(`Age Residual`),
    `Age Residual (Median)` = median(`Age Residual`),
  ) %>%
  group_by(Gender) %>% summarise(
    `Cor[err(K0),ej]` = cor(`K0 Std.Error`, `Country Residual`),
    `Cor[err(K1),ej]` = cor(`K0 Std.Error`, `Country Residual`),
    `Cor[mean(eij),ej]` = cor(`Age Residual (Mean)`, `Country Residual`),
    `Cor[median(eij),ej]` = cor(`Age Residual (Median)`, `Country Residual`)
  ) %>%
  left_join(
    joined_data %>%
      group_by(Gender) %>% summarise(
        `Cor[eij,ej]` = scales::scientific(cor(`Age Residual`, `Country Residual`)),
        `Cor[err(K0),err(C0)]` = cor(`C0 Std.Error`, `K0 Std.Error`),
        `Cor[err(K1),err(C0)]` = cor(`C0 Std.Error`, `K1 Std.Error`),
        `Cor[err(K0),err(C1)]` = cor(`C1 Std.Error`, `K0 Std.Error`),
        `Cor[err(K1),err(C1)]` = cor(`C1 Std.Error`, `K1 Std.Error`)
      ),
    by="Gender"
  ) %>% 
  mutate_if(is.numeric, ~round(., 4)) %>%
  group_by(Gender) %>%
  gather(Stat, Value, -c(Gender)) %>%
  spread(Gender, Value) %>% 
  write_csv("images/errors/correlations.csv")
