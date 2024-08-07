##################################
# Author: Vanessa Garayburu-Caruso
# Pacific Northwest National Laboratory
# email = vanessa.garayburu-caruso@pnnl.gov

# Readme: FT-ICR-MS raw data (XML files) were processed following instructions in the “FTICR_Instructions-Report_Generation_SOP_v3.pdf” document present in the ESS-DIVE data package. Molecular formula were assigned using [Formultitude](https://github.com/PNNL-Comp-Mass-Spec/Formultitude) (formerly Formularity) software. Data were further processed and sample molecular properties were assigned using the R package “ftmsRanalysis”. The outputs from these steps are referenced as "Formultitude_Output_Folder" within the scripts in this data package.  

# This script removes poorly calibrated samples from the dataset and merges sample replicates from each site where a peak was kept in the merged sample if it was present in at least one of the reps. 
#######################################

rm(list=ls(all=T))
library(dplyr)
library(tidyr); library (reshape2)

options(digits=10) # Sig figs in mass resolution data

# Set working directory
setwd("Formultitude_Output_Folder") #See GitHub Readme for more details on data processing

# Load in ICR data
data = read.csv("Processed_WROL_RC2_Data.csv", check.names = F, row.names = 1) 
mol = read.csv("Processed_WROL_RC2_Mol.csv", check.names = F, row.names = 1)


poor.cal = read.csv("WROL_RC2_Poorly_Calibrated_Samples.csv")
  
# Convert data to presence/absence
data[data > 0] = 1
# Fix naming issues identified
#Rename sample WROL2019_084-1 to WROL2019_078-1 in NPOC and ICR data, metadata, and IGSN.
colnames(data) = gsub('WROL2019_084_ICR.1','WROL2019_078_ICR.1',colnames(data))

# Fixing colnames
colnames(data)=gsub("ICR.1","ICR-1",colnames(data))
colnames(data)=gsub("ICR.2","ICR-2",colnames(data))
colnames(data)=gsub("ICR.3","ICR-3",colnames(data))

# Remove poorly calibrated samples
data = data[,-which(colnames(data) %in% poor.cal$samples)]

# Removing rows that are completely zero across the mol and data file
if(length(which(rowSums(data) == 0)) > 0){
  mol = mol[-which(rowSums(data) == 0),]
  data = data[-which(rowSums(data) == 0),]
}



# Generate factors
factors = data.frame(Sample = colnames(data), Site = gsub("_IC.*$", "", colnames(data)))

uniq.site = unique(factors$Site)
uniq_n = length(uniq.site)

# Generate object to store merged data
merge.data = data.frame(matrix(nrow = nrow(data), ncol = uniq_n, dimnames = list(row.names(data), uniq.site)))

# Loop through sites
for(i in 1:uniq_n){
  # Select sites
  w = which(factors$Site %in% uniq.site[i])
  if (length(w)<2){
    w = c(w,w) # This happens if a sample only has one rep so the merging becomes tricky
  }
  # Create temporary object
  merge.data[,i] = rowSums(data[,w])
}

# Combining sites where a peak is in any of the samples
data = merge.data; data[data < 1] = 0


# Ensuring presence/absence
data[data > 0] = 1

# Writing merged and cleaned data
write.csv(data, "Processed_WROL_RC2_Data_Clean_083023.csv", quote = F)
write.csv(mol, "Processed_WROL_RC2_Mol_Clean_083023.csv", quote = F)


