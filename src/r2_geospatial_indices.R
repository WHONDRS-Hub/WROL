# Author: Kevin A. Ryan, karyan@usgs.gov
# 2024

#0.0 Setup----
library(here)
here <- here()
library(tidyverse)
install.packages("naniar")
library(naniar)
library(rstatix)



#1.0 Input data----
#load metadata and geospatial dataframes
meta <- readRDS(paste0(here, "/output/meta.Rds")) 
sites <- meta$Site %>% as_factor() #52 Sites

geospat <- read_csv(paste0(here, "/data/geospat/Geospatial_all_YRB_WROL_2023-05-01.csv"))
#Only 49 Sites from geospatial analysis

sites_missing <- setdiff(sites, geospat$site)
#Missing HJA-WS2, HJA-WS1, CT-W9

geospat %>% count(site) %>% filter(n>1) #no duplicated site names
glimpse(geospat)

#Add additional site info
geospat_add <- read_csv(paste0(here, "/data/geospat/geospat_additional_sites_20230915.csv"))

#Join
geospat2 <- bind_rows(geospat, geospat_add) 
nrow(geospat2)

#Select geospatial variables of interest and join to site metadata
geospat3 <- geospat2 %>% 
  select(site, streamorde, ElevWs, #mean elevation in meters
         TmeanSite, #mean 30 year temperature in Celsius for selected years
         PrecipSite, #mean 30 year precipitation (mm)
         RunoffWs, #mean runoff (mm)
         areasqkm, CatAreaSqKm, WsAreaSqKm, slope,
         PctOw2019Ws, #percent open water in 2019
         PctMxFst2019Ws, #percent mixed forest
         PctDecid2019Ws, #percent deciduous forest
         PctConif2019Ws, #percent deciduous forest
         PctCrop2019Ws, #percent crop land use
         PctWdWet2019Ws, #percent woody wetland cover
         PctHbWet2019Ws, #percent herbaceous wetland cover
         PctUrbHi2019Ws, #percent high intensity urban development 
         PctImp2019Ws, #mean percent impervious land cover for 2019
         PopDen2010Ws, #percent population density in 2010 Census
         RdDensWs, #mean road density (km/sq.km)
         KffactWs, #mean soil erodibility factor (Kf, unitless)
         WWTPAllDensWs, #waster water treatment plant density (#/sq.km)
         OmWs, #mean percent organic matter soil content
         DamDensWs, #dams/sq.km
         HydrlCondWs, #mean hydraulic conductivity of surface geologic (micrometers/second)
         ClayWs, #mean percent clay content of soils
         SandWs, #mean percent sand content of soils
         PermWs, #mean permeability of soils (cm/hour)
  ) %>% 
  left_join(., meta %>% select(Site, Watershed), by = c("site" = "Site")) %>% 
  select(Watershed, site, everything()) %>% 
  distinct(site, .keep_all = TRUE)
  
glimpse(geospat3)


#2.0 Land Use Diversity Index-----
# geospat2 <- readRDS(paste0(here, "/output/geospat/geospat_indices.Rds"))

#number of types
k <- geospat3 %>% select(contains("Pct")) %>% colnames() %>% length()

#Adapted Shannon Diversity Evenness
#-sum(proportion type*log proportion for each site)/log(k)
geospat4 <- geospat3 %>% 
  # rownames_to_column("site") %>% 
  select(Watershed, site, contains("Pct")) %>% 
  pivot_longer(cols = contains("Pct")) %>% 
  mutate(log_pct = case_when(value == 0 ~ 0,
                             TRUE ~ log(value/100)),
         pct_log_pct = value/100 * log_pct) %>% 
  group_by(site) %>% 
  mutate(lulc_H = -sum(pct_log_pct),
         lulc_evenness = lulc_H/log(k)) %>% 
  ungroup() %>% 
  distinct(site, .keep_all = TRUE) %>% 
  select(site, lulc_H, lulc_evenness) %>% 
  left_join(geospat3, ., by = "site")



#3.0 SAVE
saveRDS(geospat4 %>% select(-Watershed),
        paste0(here, "/output/geospat/geospat_indices.Rds"))
write_csv(geospat4 %>% select(-Watershed),
        paste0(here, "/output/geospat/geospat_indices.csv"))


#clean environment
to_remove <- ls() %>% as_tibble() %>%
  filter(str_detect(value, "here", negate = TRUE)) %>%
  pull()
rm(list = to_remove)
