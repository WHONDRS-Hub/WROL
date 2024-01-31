#0.0 Setup----
library(here)
here <- here()
library(tidyverse)
library(lubridate)


#1.0 Input data----
#Load various sources of data and assign sample identification information
meta <- readRDS(paste0(here, "/output/meta.Rds"))

##Note: metadata missing 3 WROL samples?

##1.2 Load geospatial - complete for all sites (ID = Site ID)
comids <- read_csv(paste0(here, "/data/travel time/COMIDs.csv")) %>% 
  select(-Watershed)
geospat <- readRDS(paste0(here, "/output/geospat/geospat_indices.Rds")) %>% 
  left_join(., comids, by = c("site" = "Site"))


##1.3 Load FT ICR-MS indices (ID = sample name)
#Mass difference transformation analysis results
# trans <- read_csv(paste0(here, "/output/transformation analysis/RC2_WROL_Total_and_Normalized_Transformations_082823.csv"))
trans <- read_csv(paste0(here, "/data/WROL-RC2 Transformations Updated 20230828/RC2_WROL_Total_and_Normalized_Transformations_083023.csv"))

#Compound Class Summary
compound_class <- read_csv(paste0(here, "/data/WROL-RC2 Transformations Updated 20230828/WROL_RC2_Compound_Class_Summary.csv")) %>% rename("sample" = "...1")
#Elemental Composition Summary
elemental_comp <- read_csv(paste0(here, "/data/WROL-RC2 Transformations Updated 20230828/WROL_RC2_Elemental_Composition_Summary.csv")) %>% rename("sample" = "...1")

#Molecular Indices Summary
mol_info <- read_csv(paste0(here, "/data/WROL-RC2 Transformations Updated 20230828/WROL_RC2_MolInfo_Summary.csv")) %>% rename("sample" = "...1")

indices <- left_join(trans, mol_info, by = "sample") %>% 
  left_join(., compound_class, by = "sample") %>% 
  left_join(., elemental_comp, by = "sample") %>% 
  select(-contains(c(".median", ".sd", ";", "NA"))) %>% 
  mutate(across(Lignin:CHOSP, .fns= ~./number.of.peaks, .names= "{.col}_norm"))
glimpse(indices)

##1.4 Load Other Chemistry  (ID = dataset, sample #)
##' WROL 
chem_wrol <- read_csv(paste0(here, "/data/ancillary chemistry/wrol_chem_20230915.csv")) %>% 
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
  select(sample, DOC_mgL, TN_mgL, t, c, uva254, S275295, suva254) %>% 
  mutate(across(.cols = DOC_mgL:suva254, .fns = as.numeric))


#Check for duplicates
chem_wrol %>% count(sample) %>% filter(n>1)


##' Yakima from RC2 data release (ID = Sample Name)
#Does this need to be updated after revising data release?
chem_yrb <- read_csv(paste0(here, "/data/ancillary chemistry/RC2_NPOC_TN_DIC_TSS_Ions_Summary_2021-2022.csv"),
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

chem <- bind_rows(chem_wrol, chem_yrb)

##1.5 Load travel time
##' WROL sites (ID = dataset, sample_n)
##' 
tt <- read_csv(paste0(here, "/data/travel time/meta_tt_fromTedB_18Aug2023_tb.csv")) %>% 
  select(sample, tt_hr, q_daily_cms) %>% 
  mutate(across(.cols = tt_hr:q_daily_cms, .fns = as.numeric))

##1.6 Water Temperature
YRB_watT <- read_csv(paste0(here, "/data/waterTemp/RC2_Ultrameter_WaterChem_Summary.csv"),
                     skip = 26) %>% 
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

watT <- bind_rows(YRB_watT, WROL_watT)

##1.7 Damkohler Numbers per sample
Da <- read_csv(paste0(here, "/data/waterTemp/Da_ShaodaLiu.csv")) %>% 
  rename("depth_m" = "depth",
         "vf_m_day" = "vf_md") %>% 
  select(sample, depth_m, vf_m_day, Da)

#2.0 Save input data ----

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

#SAVE
saveRDS(dat2, paste0(here, "/output/dat_wide.Rds"))
write_csv(dat2, paste0(here, "/output/dat_wide.csv"))

#clean environment
to_remove <- ls() %>% as_tibble() %>%
  filter(str_detect(value, "here", negate = TRUE)) %>%
  pull()
rm(list = to_remove)
#END

#' Field data - INCOMPLETE - need WROL field data
# Yakima (ID = Site, Date)
# field_yrb <- read_csv(paste0(here, "/data/ancillary chemistry/RC2_Ultrameter_WaterChem_Summary.csv"),
#                       skip = 26)

# field_wrol <- 



# tt_wrol <- read_csv(paste0(here, "/data/travel time/travTime_WROL_withRes_v5_230407_ed_for_DOM.csv")) %>%
#   mutate(dataset = "WROL",
#          sample_n = wrol_sample_id) %>% 
#   left_join(meta, ., by = c("dataset", "sample_n")) %>% 
#   drop_na(sample) %>% 
#   select(sample, q_daily_m3s, tt_hr, Notes...16, Notes...21) %>% 
#   rename("tt_note1" = "Notes...16",
#          "tt_note2" = "Notes...21")
# 
# #Check for duplicates
# tt_wrol %>% count(sample) %>% filter(n>1)
# 
# 
# ##' Yakima basin (ID = site description, Date)
# tt_yrb <- read_csv(paste0(here, "/data/travel time/Final_YRB_Temporal_Daily_Discharge_tb_calc.csv")) %>% 
#   mutate(site_description = case_when(Location == "Kiona" ~ "Yakima at Kiona",
#                           Location == "Union Gap" ~ "Yakima at Union Gap",
#                           Location == "Mabton" ~ "Yakima at Mabton",
#                           Location == "Naches- Craig Road 1" ~ "Naches River Craig Road 2",
#                           Location == "Little Naches" ~ "Little Naches at Nile",
#                           TRUE ~ Location)) %>% 
#   rename("Date" = "Date_[yyyy-mm-dd]",
#          "q_daily_m3s" = "daily_q_cms") %>% 
#   left_join(., meta, by = c("site_description", "Date")) %>% 
#   drop_na(sample) %>% 
#   select(sample, q_daily_m3s, tt_hr)
# 
# 
# #Check for duplicates
# tt_yrb %>% count(sample) %>% filter(n>1)
# 
# #bind rows
# tt <- bind_rows(tt_wrol, tt_yrb)
