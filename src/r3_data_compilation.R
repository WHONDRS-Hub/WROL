#0.0 Setup----
library(here)
here <- here()
library(tidyverse)
library(lubridate)


#1.0 Input data----
#Load various sources of data and assign sample identification information
meta <- readRDS(paste0(here, "/output/meta.Rds"))

##1.2 Load geospatial - complete for all sites (ID = site)
comids <- read_csv(paste0(here, "/data/travel_time/COMIDs.csv")) %>% 
  select(-Watershed)
geospat <- readRDS(paste0(here, "/output/geospat/geospat_indices.Rds")) %>% 
  left_join(., comids, by = c("site" = "Site"))


##1.3 Load FTICR-MS indices (ID = sample)
#Peak mass difference analysis
trans <- read_csv(paste0(here, "/data/WROL-RC2_Transformations_Updated_20230828/RC2_WROL_Total_and_Normalized_Transformations_083023.csv"))

#Compound Class Summary
compound_class <- read_csv(paste0(here, "/data/WROL-RC2_MolecularInfo_Updated_202402/WROL_RC2_Compound_Class_Summary.csv"))

#Elemental Composition Summary
elemental_comp <- read_csv(paste0(here, "/data/WROL-RC2_MolecularInfo_Updated_202402/WROL_RC2_Elemental_Composition_Summary.csv"))

#Molecular Indices Summary
mol_info <- read_csv(paste0(here, "/data/WROL-RC2_MolecularInfo_Updated_202402/WROL_RC2_MolInfo_Summary.csv")) %>% 
  select(-number.of.peaks)

indices <- left_join(trans, mol_info, by = "sample") %>% 
  left_join(., compound_class, by = "sample") %>% 
  left_join(., elemental_comp, by = "sample") %>% 
  select(-contains(c(".median", ".sd", ";", "NA"))) %>% 
  mutate(across(Lignin:CHOSP, .fns= ~./peaks.with.formula*100, .names= "{.col}_norm"))
glimpse(indices)


##1.4 Load Other Chemistry  (ID = dataset, sample #)
##' WROL 
chem_wrol <- read_csv(paste0(here, "/data/ancillary_chemistry/wrol_chem_20230915.csv")) %>% 
  drop_na(wrol_sample_id) %>% 
  mutate(.after = whondrs_id,
         dataset = "WROL",
         sample_n = as.numeric(whondrs_id),
         date = dmy(sample_date)) %>% 
  drop_na(sample_n) %>% 
  left_join(., meta %>% select(sample, dataset, sample_n),
            by = c("dataset", "sample_n")) %>% 
  drop_na(sample) %>% #Some HRMS samples excluded due to poor mass calibration
  rename("DOC_mgL" = "DOC",
         "TN_mgL" = "TN") %>% 
  select(sample, DOC_mgL, TN_mgL) %>% 
  mutate(across(.cols = DOC_mgL:TN_mgL, .fns = as.numeric))


#Check for duplicates
chem_wrol %>% count(sample) %>% filter(n>1)
#No duplicates

##' Yakima from RC2 data release (ID = Sample Name)
chem_yrb <- read_csv(paste0(here, "/data/ancillary_chemistry/RC2_NPOC_TN_DIC_TSS_Ions_Summary_2021-2022.csv"),
                     skip = 2) %>% 
  slice(-c(1:11, 205)) %>% 
  select(-c(Field_Name, Material, Mean_Missing_Reps)) %>% 
  rename("sample" = "Sample_Name",
         "DOC_mgL" = "Mean_00681_NPOC_mg_per_L_as_C",
         "TN_mgL" = "Mean_00602_TN_mg_per_L_as_N",
         "TSS_mgL" = "Mean_00530_TSS_mg_per_L") %>% 
  select(sample, DOC_mgL, TN_mgL, TSS_mgL) %>% 
  mutate(across(.cols = DOC_mgL:TSS_mgL, .fns = as.numeric)) %>% 
  mutate(across(.cols = DOC_mgL:TSS_mgL,
                .fns = ~case_when(.x < 0 ~ NA_real_,
                                  TRUE ~ .x)))

#Join water chemistry data
chem <- bind_rows(chem_wrol, chem_yrb)

##1.5 Load surface water residence time data
##' WROL sites (ID = dataset, sample_n)
tt <- read_csv(paste0(here, "/data/travel_time/meta_tt_fromTedB_18Aug2023_tb.csv")) %>% 
  select(sample, tt_hr, q_daily_cms) %>% 
  mutate(across(.cols = tt_hr:q_daily_cms, .fns = as.numeric))

##1.6 Water Temperature
YRB_watT <- read_csv(paste0(here, "/data/waterTemp/RC2_Ultrameter_WaterChem_Summary.csv")) %>% 
  select(Date, Site_ID, Ultrameter_Temperature_Mean) %>% 
  rename("Site" = "Site_ID",
         "Twat_degC" = "Ultrameter_Temperature_Mean") %>% 
  left_join(meta, ., by = c("Site", "Date")) %>% 
  select(sample, Twat_degC) %>% 
  drop_na(Twat_degC)

WROL_watT <- read_csv(paste0(here, "/data/waterTemp/meta_waterTemp_degC_WROL_tb.csv")) %>% 
  rename("Twat_degC" = "watTemp_degC") %>% 
  select(sample, Twat_degC) %>% 
  drop_na(Twat_degC)

#Join
watT <- bind_rows(YRB_watT, WROL_watT)

##1.7 Damkohler Numbers per sample
Da <- read_csv(paste0(here, "/data/Da_ShaodaLiu.csv")) %>% 
  rename("depth_m" = "depth",
         "vf_m_day" = "vf_md") %>% 
  select(sample, depth_m, vf_m_day, Da)


#2.0 Save input data as list ----
dat <- list(meta = meta,
          geospat = geospat,
          indices = indices,
          chem = chem, 
          tt = tt,
          watT = watT,
          Da = Da)

saveRDS(dat, paste0(here, "/output/dat_compiled_ls.Rds"))

#clean environment
to_remove <- ls() %>% as_tibble() %>%
  filter(str_detect(value, "here", negate = TRUE)) %>%
  pull()
rm(list = to_remove)


#3.0 Merge data wide ----
list2env(x=readRDS(paste0(here, "/output/dat_compiled_ls.Rds")),
         envir = globalenv())

dat <- inner_join(meta, geospat, by = c("Site" = "site")) %>% 
  left_join(., indices, by = "sample") %>% 
  left_join(., tt, by = "sample") %>% 
  left_join(., watT, by = "sample") %>% 
  left_join(., Da, by = "sample") %>% 
  left_join(., chem, by = "sample")
glimpse(dat)

#Add additional calculated variables
dat2 <- dat %>%
  mutate(.after= q_daily_cms,
         q_daily_mm = q_daily_cms/WsAreaSqKm/1000)
glimpse(dat2)

#SAVE compiled wide dataframe
saveRDS(dat2, paste0(here, "/output/dat_wide.Rds"))
write_csv(dat2, paste0(here, "/output/dat_wide.csv"))

#clean environment
to_remove <- ls() %>% as_tibble() %>%
  filter(str_detect(value, "here", negate = TRUE)) %>%
  pull()
rm(list = to_remove)
#END
