### rename_column_headers.R ####################################################
# Date Created: 2024-02-26
# Author: Bibi Powers-McCormack; bibi.powers-mccormack@pnnl.gov

# Objective: clean up column headers
  # remove spaces
  # remove special characters

# Inputs: 
  # data loaded from the `load_tabular_data()` function (this function is included in the `rcsfa-data_processing_for_publication` repo)

# Outputs: 
  # edited .csv files

# Assumptions: 
  # only .csv files needed to be edited (not .tsvs)
  # all edited files don't have extra header info (are not Boye or Goldman formatted)
  

### Prep Script ################################################################

# load libraries
library(tidyverse)
library(rlog)

# set working directory to this GitHub repo
current_path <- rstudioapi::getActiveDocumentContext()$path # get current path
setwd(dirname(current_path)) # set wd to current path
rm(current_path)
setwd("../....") # move wd back to the parent of the repo directory
getwd()

# load data - before running this code, run the `data_package_data <- load_tabular_data()` function to load in the data
    # the load_tabular_data() function loads each tabular data file in as a list with the relative file path full name stored as the list name
      # e.g., data > /rcsfa-RC4-WROL-YRB_DOM_Diversity/data/output/dat_wide.csv > dataframe with the csv data
data <- data_package_data$data


### Create renaming function ###################################################

# this function takes a df, the new col name you want, and the old (incorrect) col name as arguments
rename_headers <- function(df, new_name, old_name) {
  
  # if the column is in the df, it renames it
  if (old_name %in% colnames(df)) {
    df <- df %>% 
      rename(!!new_name := !!old_name)
  }
  return(df)
}


### Rename headers #############################################################

# create empty list for dfs
renamed_dfs <- list()

# iterate through every df in the list and rename columns if they need to be renamed
# returns only the dfs that were edited
for (i in 1:length(data)) {
  
  # get current data name
  current_data_name <- names(data)[i]
    
  # get current data
  current_data <- data[[i]]
  
  log_info(paste0("Checking '", current_data_name, "'"))
  
  # rename headers
  renamed_df <- current_data %>% 
    rename_headers(., "Amino_Sugar", "Amino Sugar") %>% 
    rename_headers(., "Amino_Sugar_norm", "Amino Sugar_norm") %>% 
    rename_headers(., "Carbohydrate;Amino Sugar", "") %>% 
    rename_headers(., "Cond_Hydrocarbon", "Cond Hydrocarbon") %>% 
    rename_headers(., "Cond_Hydrocarbon_norm", "Cond Hydrocarbon_norm") %>% 
    rename_headers(., "DOC_SD", "DOC SD") %>% 
    rename_headers(., "Fe2_Flag", "Fe2 Flag") %>% 
    rename_headers(., "Lignin_Amino_Sugar", "Lignin;Amino Sugar") %>% 
    rename_headers(., "Lignin_Tannin", "Lignin;Tannin") %>% 
    rename_headers(., "Lipid_Protein", "Lipid;Protein") %>% 
    rename_headers(., "Protein_Amino_Sugar", "Protein;Amino Sugar") %>% 
    rename_headers(., "Protein_Lignin", "Protein;Lignin") %>% 
    rename_headers(., "Protein_Lignin_Amino_Sugar", "Protein;Lignin;Amino Sugar") %>% 
    rename_headers(., "Sampling_Event", "Sampling Event") %>% 
    rename_headers(., "Tannin_Cond_Hydrocarbon", "Tannin;Cond Hydrocarbon") %>% 
    rename_headers(., "TN_SD", "TN SD") %>% 
    rename_headers(., "Total_Fe_Flag", "Total Fe Flag") %>% 
    rename_headers(., "Unsat_Hydrocarbon", "Unsat Hydrocarbon") %>% 
    rename_headers(., "Unsat_Hydrocarbon_Cond_Hydrocarbon", "Unsat Hydrocarbon;Cond Hydrocarbon") %>% 
    rename_headers(., "Unsat_Hydrocarbon_norm", "Unsat Hydrocarbon_norm")
  
  if (identical(names(current_data), names(renamed_df))) {
    
    log_info("No headers were renamed.")
    
  } else {
    
    log_info(paste0("'", current_data_name, "' was updated."))
    
    log_info(paste0("Column Names: ", paste(colnames(current_data), collapse = ", ")))
    
    # add df to list
    renamed_dfs[[current_data_name]] <- renamed_df
    
  }
    
  
}


### Export #####################################################################

# WARNING: some of the files in this DP have extra header information at the top of the file
  # if any of those files were edited, then the following code will overwrite critical information

# I had to manually check the `renamed_dfs` list to make sure the files listed do NOT have extra header info before I ran this code: 
for (file_path in names(renamed_dfs)) {
  write_csv(renamed_dfs[[file_path]], paste0(".", file_path), na = "")
}





