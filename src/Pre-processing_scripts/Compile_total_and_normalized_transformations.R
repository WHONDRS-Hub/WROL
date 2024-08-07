##################################
# Author: Vanessa Garayburu-Caruso
# Pacific Northwest National Laboratory
# email = vanessa.garayburu-caruso@pnnl.gov
#######################################

rm(list=ls(all=T))
library(dplyr)
library(tidyr); library (reshape2)

options(digits=10) # Sig figs in mass resolution data

# Set working directory
setwd("data/WROL-RC2_Processed_FTICR_Data")
trans.path = "src/Pre-processing_scripts/"

Sample_Name = "RC2_WROL"
# Read in the data
data = read.csv("Processed_WROL_RC2_Data_Clean_083023.csv")
profiles.of.trans =  read.csv("Transformation_Database_07-2020.csv")


# Since we had to split the data multiple times for the transformations code not to crash, we have to calculate again the total transformations and the transformation profiles for the 362 samples and then perform the normalization
files = list.files(path = trans.path, full.names = T)
tot.trans = numeric()

for (i in 1:length(files)) {

Distance_Results = read.csv(files[i])

# sum up the number of transformations and update the matrix
tot.trans = rbind(tot.trans,c(Distance_Results$sample[1],nrow(Distance_Results)))

# generate transformation profile for the sample
trans.profile = as.data.frame(tapply(X = Distance_Results$Trans.name,INDEX = Distance_Results$Trans.name,FUN = 'length'));

colnames(trans.profile) = Distance_Results$sample[1]

# update the profile matrix
profiles.of.trans = merge(x = profiles.of.trans,y = trans.profile,by.x = "Name",by.y = 0,all.x = T)
profiles.of.trans[is.na(profiles.of.trans[,Distance_Results$sample[1]]),Distance_Results$sample[1]] = 0 # Changing all the NA values to zero
}

# format the total transformations matrix and write it out
tot.trans = as.data.frame(tot.trans)
colnames(tot.trans) = c('sample','total.transformations')
tot.trans$sample = as.character(tot.trans$sample)
tot.trans$total.transformations = as.numeric(as.character(tot.trans$total.transformations))
str(tot.trans)


# Calculating number of peaks per each sample and normalizing the transformations

tot.trans$number.of.peaks = NA

for (i in 1:nrow(tot.trans)){
  temp = as.data.frame(data[which(colnames(data)==tot.trans$sample[i])])
  tot.trans$number.of.peaks[i] = sum(temp)
}

tot.trans$normalized.transformations = tot.trans$total.transformations/tot.trans$number.of.peaks

write.csv(tot.trans,paste(Sample_Name,"_Total_and_Normalized_Transformations_083023.csv", sep=""),quote = F,row.names = F)
