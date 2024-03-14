rm(list=ls(all=T))
library(dplyr)
library(tidyr); library (reshape2)

options(digits=10) # Sig figs in mass resolution data

# Set working directory
setwd("C:/Users/gara009/Documents/WROL_RC2/")

# Load in ICR data
data = read.csv("Processed_WROL_RC2_Data.csv", check.names = F, row.names = 1)
mol = read.csv("Processed_WROL_RC2_Mol.csv", check.names = F, row.names = 1)


poor.cal = read.csv("WROL_RC2_Poorly_Calibrated_Samples.csv")
  
# Convert data to presence/absence
data[data > 0] = 1
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
write.csv(data, "Processed_WROL_RC2_Data_Clean.csv", quote = F)
write.csv(mol, "Processed_WROL_RC2_Mol_Clean.csv", quote = F)

# Writing out the number of reps each site has
reps = as.data.frame(matrix(NA, ncol = 2,nrow= length(uniq.site)))
colnames(reps) = c("Site","Number_of_reps")

for (i in 1:uniq_n){
  reps$Site[i] = uniq.site[i]
  reps$Number_of_reps[i] = nrow(subset(factors, factors$Site == uniq.site[i]))
}

write.csv(reps, "Samples_and_reps.csv", row.names = F)
