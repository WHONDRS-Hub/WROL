require(easycsv)
require(tidyverse)

# User Input
Sample_Name = "WROL_RC2" # Sample name for output; user will need to change to "YDE21_7T_OG" if using 7T data


### Load in data ###
#setwd(easycsv::choose_dir())

edata = read.csv(list.files(pattern = "*Data_Clean_083023.csv"), row.names = 1, check.names = F)
edata$Mass = rownames(edata) # doing it like this to keep as much of the decimals as possible

emeta = read.csv(list.files(pattern = "*Mol_Clean_083023.csv"), row.names = 1)
emeta$Mass = rownames(emeta)

############### Summary generation ############### 
  # Storing row names
  row.names(edata) = edata$Mass; edata = edata[,-which(colnames(edata) %in% "Mass")]
  row.names(emeta) = emeta$Mass; emeta = emeta[,-which(colnames(emeta) %in% "Mass")]
  
  #### Compound class summary
  # Finding unique compound classes
  uniq.comp = unique(emeta$bs1_class)
  
  # Looping through each sample to obtain some summary categories
  classes = matrix(nrow = ncol(edata), ncol = length(uniq.comp)) # Creating empty matrix to store stats
  colnames(classes) = uniq.comp
  row.names(classes) = colnames(edata)
  
  name.temp = NULL
  
   for(i in 1:ncol(edata)){
    temp = edata[which(edata[,i] > 0), i, drop = F] # Need to keep names, looking at columns
    temp = emeta[which(row.names(emeta) %in% row.names(temp)),]
    
    for(j in 1:length(uniq.comp)){
      classes[i,j] = length(which(temp$bs1 %in% uniq.comp[j]))
    }
    
    name.temp = c(name.temp, colnames(edata)[i])
  } 
  
  classes = as.data.frame(classes)
  
  write.csv(classes, paste(Sample_Name, "_Compound_Class_Summary.csv", sep = ""), quote = F)
  
  
  #### Elemental Composition Summary
  uniq.elem = unique(emeta$El_comp)
  uniq.elem = uniq.elem[-which(uniq.elem %in% "")]
  
  # Looping through each sample to obtain some summary categoreies
  elem.comp = matrix(nrow = ncol(edata), ncol = length(uniq.elem)) # Creating empty matrix to store stats
  colnames(elem.comp) = uniq.elem
  row.names(elem.comp) = colnames(edata)
  
  name.temp = NULL
  
  for(i in 1:ncol(edata)){
    temp = edata[which(edata[,i] > 0), i, drop = F] # Need to keep names, looking at columns
    temp = emeta[which(row.names(emeta) %in% row.names(temp)),]
    
    for(j in 1:length(uniq.elem)){
      elem.comp[i,j] = length(which(temp$El_comp %in% uniq.elem[j]))
    }
    
    name.temp = c(name.temp, colnames(edata)[i])
  } 
  
  elem.comp = as.data.frame(elem.comp)
  
  write.csv(elem.comp, paste(Sample_Name, "_Elemental_Composition_Summary.csv", sep = ""), quote = F)
  
  
  #### Characteristics summary
  # Looping through each sample to obtain some summary stats of the peaks
  characteristics = data.frame(AI.mean = rep(NA, length(colnames(edata))), AI.median = NA, AI.sd = NA,
                               AI_Mod.mean = NA, AI_Mod.median = NA, AI_Mod.sd = NA,
                               DBE.mean = NA, DBE.median = NA, DBE.sd = NA,
                               DBE_O.mean = NA, DBE_O.median = NA, DBE_O.sd = NA,
                               KenMass.mean = NA, KenMass.median = NA, KenMass.sd = NA,
                               KenDef.mean = NA, KenDef.median = NA, KenDef.sd = NA,
                               NOSC.mean = NA, NOSC.median = NA, NOSC.sd = NA,
                               HtoC.mean = NA, HtoC.median = NA, HtoC.sd = NA,
                               OtoC.mean = NA, OtoC.median = NA, OtoC.sd = NA,
                               NtoC.mean = NA, NtoC.median = NA, NtoC.sd = NA,
                               PtoC.mean = NA, PtoC.median = NA, PtoC.sd = NA,
                               NtoP.mean = NA, NtoP.median = NA, NtoP.sd = NA,
                               GFE_0.mean = NA, GFE_0.median = NA, GFE_0.sd = NA,
                               GFE_7.mean = NA, GFE_7.median = NA, GFE_7.sd = NA,
                               GFEperC_0.mean = NA, GFEperC_0.median = NA, GFEperC_0.sd = NA,
                               GFEperC_7.mean = NA, GFEperC_7.median = NA, GFEperC_7.sd = NA,
                               lambdaO2_0.mean = NA, lambdaO2_0.median = NA, lambdaO2_0.sd = NA,
                               lambdaO2_7.mean = NA, lambdaO2_7.median = NA, lambdaO2_7.sd = NA,
                               mass.mean = NA, mass.median = NA, mass.sd = NA,
                               number.of.peaks = NA, peaks.with.formula = NA,
                               row.names = colnames(edata))
  
  for(i in 1:ncol(edata)){
    temp = edata[which(edata[,i] > 0), i, drop = F] # Need to keep names, looking at columns
    characteristics$number.of.peaks[i] = nrow(temp)
    
    temp = emeta[row.names(temp),]
    temp2 = temp %>% filter(C > 0)
    options(digits=12)
    temp2$Mass = as.numeric(rownames(temp2))
    
    characteristics$peaks.with.formula[i] = nrow(temp2)
    
    
    #Mass with formula
    characteristics$mass.mean[i] = mean(temp2$Mass, na.rm = T)
    characteristics$mass.median[i] = median(temp2$Mass, na.rm = T)
    characteristics$mass.sd[i] = sd(temp2$Mass, na.rm = T)
    
     # AI
    characteristics$AI.mean[i] = mean(temp$AI, na.rm = T)
    characteristics$AI.median[i] = median(temp$AI, na.rm = T)
    characteristics$AI.sd[i] = sd(temp$AI, na.rm = T)
    
    # AI_Mod
    characteristics$AI_Mod.mean[i] = mean(temp$AI_Mod, na.rm = T)
    characteristics$AI_Mod.median[i] = median(temp$AI_Mod, na.rm = T)
    characteristics$AI_Mod.sd[i] = sd(temp$AI_Mod, na.rm = T)
    
    # DBE
    characteristics$DBE.mean[i] = mean(temp$DBE_1, na.rm = T)
    characteristics$DBE.median[i] = median(temp$DBE_1, na.rm = T)
    characteristics$DBE.sd[i] = sd(temp$DBE_1, na.rm = T)
    
    # DBE-O
    characteristics$DBE_O.mean[i] = mean(temp$DBE_O, na.rm = T)
    characteristics$DBE_O.median[i] = median(temp$DBE_O, na.rm = T)
    characteristics$DBE_O.sd[i] = sd(temp$DBE_O, na.rm = T)
    
    # Kendrick Mass
    characteristics$KenMass.mean[i] = mean(temp$kmass, na.rm = T)
    characteristics$KenMass.median[i] = median(temp$kmass, na.rm = T)
    characteristics$KenMass.sd[i] = sd(temp$kmass, na.rm = T)
    
    # Kendrick Defect
    characteristics$KenDef.mean[i] = mean(temp$kdefect, na.rm = T)
    characteristics$KenDef.median[i] = median(temp$kdefect, na.rm = T)
    characteristics$KenDef.sd[i] = sd(temp$kdefect, na.rm = T)
    
    # NOSC
    characteristics$NOSC.mean[i] = mean(temp$NOSC, na.rm = T)
    characteristics$NOSC.median[i] = median(temp$NOSC, na.rm = T)
    characteristics$NOSC.sd[i] = sd(temp$NOSC, na.rm = T)
    
    # H:C Ratio
    characteristics$HtoC.mean[i] = mean(temp$HtoC_ratio, na.rm = T)
    characteristics$HtoC.median[i] = median(temp$HtoC_ratio, na.rm = T)
    characteristics$HtoC.sd[i] = sd(temp$HtoC_ratio, na.rm = T)
    
    # O:C Ratio
    characteristics$OtoC.mean[i] = mean(temp$OtoC_ratio, na.rm = T)
    characteristics$OtoC.median[i] = median(temp$OtoC_ratio, na.rm = T)
    characteristics$OtoC.sd[i] = sd(temp$OtoC_ratio, na.rm = T)
    
    # N:C Ratio
    characteristics$NtoC.mean[i] = mean(temp$NtoC_ratio, na.rm = T)
    characteristics$NtoC.median[i] = median(temp$NtoC_ratio, na.rm = T)
    characteristics$NtoC.sd[i] = sd(temp$NtoC_ratio, na.rm = T)
    
    # P:C Ratio
    characteristics$PtoC.mean[i] = mean(temp$PtoC_ratio, na.rm = T)
    characteristics$PtoC.median[i] = median(temp$PtoC_ratio, na.rm = T)
    characteristics$PtoC.sd[i] = sd(temp$PtoC_ratio, na.rm = T)
    
    # N:P Ratio
    characteristics$NtoP.mean[i] = mean(temp$NtoP_ratio, na.rm = T)
    characteristics$NtoP.median[i] = median(temp$NtoP_ratio, na.rm = T)
    characteristics$NtoP.sd[i] = sd(temp$NtoP_ratio, na.rm = T)
    
    # GFE_0
    characteristics$GFE_0.mean[i] = mean(temp$delGd0, na.rm = T)
    characteristics$GFE_0.median[i] = median(temp$delGd0, na.rm = T)
    characteristics$GFE_0.sd[i] = sd(temp$delGd0, na.rm = T)
    
    # GFE_7
    characteristics$GFE_7.mean[i] = mean(temp$delGd, na.rm = T)
    characteristics$GFE_7.median[i] = median(temp$delGd, na.rm = T)
    characteristics$GFE_7.sd[i] = sd(temp$delGd, na.rm = T)
    
    # GFEperC_0
    characteristics$GFEperC_0.mean[i] = mean(temp$delGcox0PerCmol, na.rm = T)
    characteristics$GFEperC_0.median[i] = median(temp$delGcox0PerCmol, na.rm = T)
    characteristics$GFEperC_0.sd[i] = sd(temp$delGcox0PerCmol, na.rm = T)
    
    # GFEperC_7
    characteristics$GFEperC_7.mean[i] = mean(temp$delGcoxPerCmol, na.rm = T)
    characteristics$GFEperC_7.median[i] = median(temp$delGcoxPerCmol, na.rm = T)
    characteristics$GFEperC_7.sd[i] = sd(temp$delGcoxPerCmol, na.rm = T)
    
    # lambdaO2_0
    characteristics$lambdaO2_0.mean[i] = mean(temp$lamO20, na.rm = T)
    characteristics$lambdaO2_0.median[i] = median(temp$lamO20, na.rm = T)
    characteristics$lambdaO2_0.sd[i] = sd(temp$lamO20, na.rm = T)
    
    # lambda02_7
    characteristics$lambdaO2_7.mean[i] = mean(temp$lamO2, na.rm = T)
    characteristics$lambdaO2_7.median[i] = median(temp$lamO2, na.rm = T)
    characteristics$lambdaO2_7.sd[i] = sd(temp$lamO2, na.rm = T)
    
  } # I'm not sure how to do this without the for-loop, but I'm simply just finding the mean/median for peak stats
  
  write.csv(characteristics, paste(Sample_Name, "_MolInfo_Summary.csv", sep = ""), quote = F)
  

