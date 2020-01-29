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

dt %>% write_rds("data/03_p100/00_raw.rds")

#################################################
# Stage One - Estimating Sub-Group Parameters
#################################################
Stage1_model <- dt %>% group_by(Country_ID, Gender) %>% compute_stage1()

Stage1_model %>% write_rds("data/03_p100/Stage1_model.rds")
#Stage1_model = read_rds("data/03_p100/Stage1_model.rds")
#################################################
# Stage Two - CLaM
#################################################
Stage1_model %>% group_by(Gender) %>% compute_stage2 -> Stage2_model

Stage2_model %>% write_rds("data/03_p100/Stage2_model.rds")
#Stage2_model = read_rds("data/03_p100/Stage2_model.rds")
#################################################
# Stage Three+Four - Compute B-Age
#################################################
Stage2_model %>% compute_stage3 -> Stage3_model

Stage3_model %>% write_rds("data/03_p100/Stage3_model.rds")
#Stage3_model = read_rds("data/03_p100/Stage3_model.rds")

#################################################
# Displays
#################################################
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
  out_file = "reports/p100/2011_Tables.tex"
)

