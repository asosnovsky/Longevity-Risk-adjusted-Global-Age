# Ensure we are starting with a clean-slate
rm(list=ls())

# Set the seed for any future randomization needs
set.seed(0)

# Load used libraries
library(readr)
source("./scripts/02_display/00_methods.R")

# util functions
load_data <- function(path) read_csv(
  path, 
  col_names = c("Country_ID", 40:76), 
  skip = 1, 
  col_types = cols(
    Country_ID = col_integer(),
    .default=col_double()
  )
) %>% gather(Age, qx, -Country_ID)

# read data
dt <- bind_rows(
  load_data("./data/00_raw/p100/p100_mortality_male.csv") %>% mutate(Gender = "Male"),
  load_data("./data/00_raw/p100/p100_mortality_female.csv") %>% mutate(Gender = "Female")
) %>% mutate(Age=as.numeric(Age))

dt %>% write_rds("data/00_raw/p100/00_raw.rds")

#################################################
# Stage One - Estimating Sub-Group Parameters
#################################################
if(file.exists("data/01_models/p100/Stage1_model.rds")) {
  Stage1_model = read_rds("data/01_models/p100/Stage1_model.rds")  
} else {
  Stage1_model <- dt %>% group_by(Country_ID, Gender) %>% compute_stage1()
  Stage1_model %>% write_rds("data/01_models/p100/Stage1_model.rds")
}

#################################################
# Stage Two - CLaM
#################################################
if(file.exists("data/01_models/p100/Stage2_model.rds")) {
  Stage2_model = read_rds("data/01_models/p100/Stage2_model.rds")
}else{
  Stage1_model %>% group_by(Gender) %>% compute_stage2 -> Stage2_model
  Stage2_model %>% write_rds("data/01_models/p100/Stage2_model.rds")
}

#################################################
# Stage Three+Four - Compute B-Age
#################################################
if(file.exists("data/01_models/p100/Stage3_model.rds")) {
  Stage3_model = read_rds("data/01_models/p100/Stage3_model.rds")
}else{
  Stage2_model %>% compute_stage3 -> Stage3_model
  Stage3_model %>% write_rds("data/01_models/p100/Stage3_model.rds")
}


#################################################
# Displays
#################################################

# Tables
Stage1_model %>% arrange(Gender, Country_ID) %>%
  group_by(Gender) %>% 
  mutate(
    idx = 1:n(),
    l_0 = l_m+exp(g*1+lnh),
    l_55 = l_m+exp(g*56+lnh)
  ) %>% 
  select(
    Gender, 
    Group=Country_ID,
    lnh, l_0, l_m, g, l_55, m, b
  ) -> table1
table1 %>% write_csv("./data/02_table/p100/table_1.csv")
table1 %>% select(-Group) %>% nest %>% 
  mutate( averages = map(
    data,  
    ~summarise_if(., is.numeric, mean)
  ) ) %>% 
  unnest(averages) %>% 
  select(-data) %>%
  write_csv("./data/02_table/p100/table_1-averages.csv")

Stage3_model %>% ungroup() %>%
  select(Gender, Group=Country_ID, Age, B_Age) %>%
  write_csv("./data/02_table/p100/table_3.csv")

Stage3_model %>% ungroup() %>%
  select(Gender, Group=Country_ID, Age, B_Age) %>%
  group_by(Gender, Age) %>%
  summarise(B_Age.mean=mean(B_Age), B_Age.max=max(B_Age),B_Age.min=min(B_Age)) %>%
  write_csv("./data/02_table/p100/table_3-agg.csv")


# Latex
table3_latex <- function(Stage3_model) {
  Stage3_model %>% ungroup() %>% filter(Gender!="Total") %>%  
    filter( Age %in% c(55, 70, 75) ) %>%  
    select(Gender, `Country Name`, Age, B_Age) %>% {
      bind_rows(
        spread(., Age, B_Age) %>% 
          mutate_if(is.numeric, ~paste0("$", number(., acc=0.01), "$")) %>% 
          unite(value, `55`, `70`, `75`, sep=" & ") %>% 
          spread(Gender, value) %>% 
          unite(value, Male, Female, sep=" & ") %>% 
          mutate(value = paste0(value, "\\\\ \\hline \\hline\n")) %>% 
          mutate(`Country Name` = paste0(1:n(), ". ", `Country Name`)),
        group_by(., Gender, Age) %>% 
          summarise(B_Age = mean(B_Age) ) %>% 
          spread(Age, B_Age) %>% ungroup %>%  
          mutate_if(is.numeric, ~paste0("{\\bf", number(., acc=0.01), "}")) %>% 
          unite(value, `55`, `70`, `75`, sep=" & ") %>% 
          spread(Gender, value) %>% 
          unite(value, Male, Female, sep=" & ") %>% 
          mutate(value = paste0(value, "\\\\ \\hline \\hline\n")) %>% 
          mutate(`Country Name` = "{\\bf Average:}")
      )
    } %>% ungroup %>% 
    unite(value, `Country Name`, value, sep=" & ") %>% { paste0(.$value, collapse = '', sep="") } -> 
    inner_table
  
  paste0(
    "\\clearpage",
    "\\begin{longtable}{||\n",
    "    p{95pt}|\n",
    "    p{35pt}|\n",
    "    p{35pt}|\n",
    "    p{35pt}|\n",
    "    p{35pt}|\n",
    "    p{35pt}|\n",
    "    p{35pt}\n",
    "||}\n",
    "\\hline\\hline\n",
    "\\multicolumn{7}{||c||}{Table \\# 3 } \\\\ \\hline\\hline\n",
    "\\multicolumn{7}{||c||}{{\\bf ",
    "Mortality-Adjusted Biological Ages $(\\xi)$ Around the World}}", 
    "\\\\ \\hline\\hline\n",
    "& \\multicolumn{3}{|c|}{{\\bf MALE}} &  ",
    "\\multicolumn{3}{|c||}{{\\bf FEMALE}} \\\\ \\hline \\hline",
    "{\\bf Country} & ",
    "$x=55$ &  $x=70$ & $x=85$ & $x=55$ & $x=70$ & $x=85$ \\\\ \\hline \\hline\n",
    inner_table,
    "\\hline\n",
    "\\multicolumn{7}{||r||}{{\\em ",
    "Source: Human Mortality Database, Period 2011}} \\\\ \\hline\\hline\n",
    "\\end{longtable}"
  ) 
}
create_tables123_in_pdf(
  Stage1_model = Stage1_model %>% mutate(Year = NA, `Country Name` = Country_ID),
  Stage2_model = Stage2_model,
  Stage3_model = Stage3_model %>% mutate(Year = NA, `Country Name` = Country_ID),
  out_file = "reports/p100/p100_base_tables.tex"
)


# Plots


save_plots(
  Stage1_model %>% 
    filter(Gender == "Female") %>% 
    rename(`lnh[i]`=lnh, `g[i]`=g) %>% 
    ggplot(aes(`g[i]`, `lnh[i]`)) + geom_point(color='red') +
    geom_smooth(size=1/4, method='lm', se=F, col='black') +
    geom_text(aes(label=`Country_ID`), size=2, nudge_x=-.0007) +
    labs(x="Mortality Growth Rate", y='Log of Initial Mortality Rate') + 
    ggtitle("Female") +
    theme(plot.title = element_text(hjust = 0.5)), 
  "images/p100/f3a/p100-f3a-lnh_vs_g-female"
)

save_plots(
  Stage1_model %>% 
    filter(Gender == "Male") %>% 
    rename(`lnh[i]`=lnh, `g[i]`=g) %>% 
    ggplot(aes(`g[i]`, `lnh[i]`)) + geom_point(color='blue') +
    geom_smooth(size=1/4, method='lm', se=F, col='black') +
    geom_text(aes(label=`Country_ID`), size=2, nudge_x=-.0007) +
    labs(x="Mortality Growth Rate", y='Log of Initial Mortality Rate') + 
    ggtitle("Male") +
    theme(plot.title = element_text(hjust = 0.5)),
  "images/p100/f3b/p100-fig3b-lnh_vs_g-male"
)

Stage3_model %>% select(Country_ID, Age, Gender, LRAG_Age) %>% 
  filter( Age == 55 ) %>% 
  group_by(Gender) %>%
  save_plots_by_group(
    name='images/p100/f13.5',
    just_plot=F,
    exts=c('.eps'),
    plot_fcn = function(d, k) d %>%
      ggplot() + 
      geom_point(aes(x=LRAG_Age, y=Country_ID), size=0.5) +
      geom_vline(xintercept=55, linetype=3) +
      labs(x="LARG Age", y="Income Percentile", title=paste0(k, " Chronological Age 55"))
  )
