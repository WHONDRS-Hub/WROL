### ESS-DIVE_landing_page_coordinates.R ########################################
# Date Created: 2024-02-23
# Date Updated: 2024-07-22
# Author: Bibi Powers-McCormack

# Objective: 
  # Pull the coordinates from the following 2 datasets:
    # WROL (v3): https://data.ess-dive.lbl.gov/view/doi:10.15485/1895159
    # YRB (v2): https://data.ess-dive.lbl.gov/view/doi:10.15485/1898912
  # Clean up by removing columns so there are only location descriptions and site coordinates for each site
  # Export as .csv to feed into landing page upload


### Prep Script ################################################################

# load libraries
library(tidyverse)


# read in data
WROL_v1 <- read_csv("Z:/00_Cross-SFA_ESSDIVE-Data-Package-Upload/01_Study-Data-Package-Folders/00_ARCHIVE-WHEN-PUBLISHED/WHONDRS_WROL2019_Data_Package/WHONDRS_WROL_Geospatial.csv")
WROL <- read_csv("Z:/00_Cross-SFA_ESSDIVE-Data-Package-Upload/01_Study-Data-Package-Folders/00_ARCHIVE-WHEN-PUBLISHED/WHONDRS_WROL2019_Data_Package_v3/v3_WHONDRS_WROL2019_Data_Package/v3_WHONDRS_WROL_Field_Metadata.csv")

YRB <- read_csv("Z:/00_Cross-SFA_ESSDIVE-Data-Package-Upload/01_Study-Data-Package-Folders/00_ARCHIVE-WHEN-PUBLISHED/RC2_TemporalStudy_2021-2022_SampleData_v2/v2_RC2_TemporalStudy_2021-2022_SampleData/v2_RC2_Sample_Field_Metadata.csv")


### Clean ######################################################################
# Objective: we want to select columns Site_ID, Basin,State, Country, Latitude, Longitude for each site
# Assumptions:
  # WROL v1 has the state and country for each site.
  # WROL has the IDs and correct coords. 
  # YRB has the Site IDs and correct coords. All are in Yakima River Basin, Washington, United States.

# WROL ----
WROL_v1 <- WROL_v1 %>% 
  select(Locality, Physiographic_Feature_Name, State_or_Province, Country) %>% 
  distinct() %>% 
  rename(Site_ID = Locality,
         State = State_or_Province,
         Location_Name = Physiographic_Feature_Name)

locations_WROL <- WROL %>% 
  select(Site_ID, Latitude, Longitude) %>% 
  distinct() %>% 
  left_join(WROL_v1) %>% 
  mutate(Source = "WROL: https://data.ess-dive.lbl.gov/view/doi:10.15485/1895159")

# YRB ----
locations_YRB <- YRB %>% 
  select(Site_ID, Site_Name, Latitude, Longitude) %>% 
  rename(Location_Name = Site_Name) %>% 
  mutate(State = "Washington",
         Country = "United States") %>% 
  distinct() %>% 
  mutate(Source = "YRB: https://data.ess-dive.lbl.gov/view/doi:10.15485/1898912")

# Combine ----
locations <- rbind(locations_WROL, locations_YRB) %>% 
  select(Site_ID, Location_Name, State, Country, Latitude, Longitude, Source) %>% 
  arrange(Site_ID) %>% 
  mutate(Description = paste0(Site_ID, ": ", Location_Name, ", ", State, ", ", Country)) %>%
  select(Description, Latitude, Longitude)


### Export #####################################################################

# export locations
write_csv(locations, "./data_package_preparation/manuscript_geospatial.csv")
