#0.0 Setup----
library(here)
here <- here()
library(tidyverse)
library(lubridate)

#0.1 Generate sample ID key called "meta" ----
#putative biochemical transformation data has the sample name for all samples

trans <- read_csv(paste0(here, "/data/WROL-RC2_Transformations_Updated_20230828/RC2_WROL_Total_and_Normalized_Transformations_083023.csv"))

#parse sample name into dataset and sample numbers
sample_ids <- trans %>% select(sample) %>% 
  mutate(dataset = case_when(str_detect(sample, "RC2_") ~ "YRB",
                             str_detect(sample, "WROL") ~ "WROL"),
         sample_n = as.numeric(str_extract(sample, "(?<=_)[^_]+")))

#Check for duplicates
sample_ids %>% group_by(dataset, sample_n) %>% count() %>% filter(n>1)
#no duplicates

#Get site, date and sample number from "Watershed Rules of Life" (WROL) dataset
wrol_meta <- read_csv(paste0(here, "/data/ancillary_chemistry/wrol_chem_20230915.csv")) %>% 
  mutate(.after = whondrs_id,
         dataset = "WROL",
         sample_n = as.numeric(whondrs_id),
         Date = dmy(sample_date),
         site_description = Watershed) %>%
  select(Site, Date, sample_n, Watershed, site_description, 
         season_tb, wrol_mo_end_member) %>% 
  drop_na(sample_n) 

#Check for duplicates
wrol_meta %>% count(sample_n) %>% filter(n>1)
#No duplicates

#Add the sample names
wrol_meta2 <- sample_ids %>% filter(dataset == "WROL") %>% 
  left_join(., wrol_meta, by = "sample_n")

#Get site, sample, date from Yakima River Basin (YRB) dataset
yrb_meta <- read_csv(paste0(here, "/data/RC2_Sample_Field_Metadata_krDateEdit20230828.csv")) %>%
  mutate(Watershed = "Yakima") %>% 
  select(Site_ID, Sample_Name, Date, Watershed, Site_Name) %>% 
  rename("Site" = "Site_ID",
         "sample" = "Sample_Name",
         "site_description" = "Site_Name")
yrb_meta2 <- sample_ids %>% filter(dataset == "YRB") %>% 
  left_join(., yrb_meta, by = "sample")

#Check for duplicates
yrb_meta2 %>% count(sample_n) %>% filter(n>1)
#No duplicates

#Add manual season classification
seasons_yrb <- read_csv(paste0(here, "/data/ancillary_chemistry/seasons_yrb.csv")) %>% 
  select(sample, season_tb)

yrb_meta3 <- yrb_meta2 %>% left_join(., seasons_yrb, by = "sample")

#1.0 Combine metadata tables together
meta <- bind_rows(wrol_meta2, yrb_meta3)

#1.1 Add additional season classes ---- 
meta2 <- meta %>% 
  mutate(Month = month(Date),
         season = factor(case_when(Month %in% c(12, 1, 2) ~ "Winter",
                                   Month %in% 3:5 ~ "Spring",
                                   Month %in% 6:8 ~ "Summer",
                                   Month %in% 9:11 ~ "Fall")),
         season_n = factor(case_when(Month %in% c(12, 1, 2) ~ 1,
                                     Month %in% 3:5 ~ 2,
                                     Month %in% 6:8 ~ 3,
                                     Month %in% 9:11 ~ 4)))


saveRDS(meta2, paste0(here, "/output/meta.Rds"))
write_csv(meta2, paste0(here, "/output/meta.csv"))




#clean environment
to_remove <- ls() %>% as_tibble() %>%
  filter(str_detect(value, "here", negate = TRUE)) %>%
  pull()
rm(list = to_remove)
