# clean_up_flmd_dd.R
# Date Created: 2024-07-23
# Date Updated: 2024-07-30
# Author: Bibi Powers-McCormack

# Objective: clean up flmd and dd

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

### Update flmd ################################################################

# read in file
flmd <- read_csv("./Ryan_2024_WROL_YRB_DOM_Diversity_flmd.csv", trim_ws = T)

# add periods to ends of descriptions
updated_flmd <- flmd %>% 
  mutate(File_Description = case_when(str_detect(File_Description, "\\.$") ~ File_Description, TRUE ~ str_c(File_Description, ".")))



### Export #####################################################################

write_csv(updated_dd, "./data_package_preparation/Ryan_2024_WROL_YRB_DOM_Diversity_dd.csv", na = "")

write_csv(updated_flmd, "./Ryan_2024_WROL_YRB_DOM_Diversity_flmd.csv", na = "")



