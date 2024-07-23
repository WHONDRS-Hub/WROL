# clean_up_dd.R
# Date Created: 2024-07-23
# Author: Bibi Powers-McCormack

# Objective: clean up dd

### Update dd ################################################################

# load libraries
library(tidyverse)

# read in file
dd <- read_csv("./data_package_preparation/Ryan_2024_WROL_YRB_DOM_Diversity_dd.csv", trim_ws = T) # wd is set to this repo, also trim white space when reading it in

# add periods to ends of definitions
updated_dd <- dd %>% 
  mutate(Definition = case_when(str_detect(Definition, "\\.$") ~ Definition, TRUE ~ str_c(Definition, ".")))

# view units
updated_dd %>% 
  count(Unit) %>% 
  arrange(Unit) %>% 
  print(n = 50)

# fix units
updated_dd <- updated_dd %>% 
  mutate(Unit = case_when(Unit == "kJ_per_C_mol" ~ "kilograms_per_Compound_mol",
                          Unit == "kJ_per_Compound_mol" ~ "kilograms_per_Compound_mol", 
                          Unit == "centimeter_per_hour" ~ "centimeters_per_hour",
                          Unit == "decimal_degrees" ~ "decimal_degrees_WGS1984",
                          Unit == "meter" ~ "meters",
                          Unit == "millimiters" ~ "millimeters",
                          Unit == "mm" ~ "millimeters",
                          T ~ Unit))


### Export dd ##################################################################

write_csv(updated_dd, "./data_package_preparation/Ryan_2024_WROL_YRB_DOM_Diversity_dd.csv", na = "")
