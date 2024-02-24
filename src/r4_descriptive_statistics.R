#0.0 Setup----
library(here)
here <- here()
library(tidyverse)
library(naniar)
library(rstatix)

#clean environment
to_remove <- ls() %>% as_tibble() %>%
  filter(str_detect(value, "here", negate = TRUE)) %>%
  pull()
rm(list = to_remove)

#1.0 Input and Rearrange Data----
dat <- readRDS(paste0(here, "/output/dat_wide.Rds")) %>% 
  convert_as_factor(vars= c("Watershed", "season_tb", "Site", "streamorde"))
glimpse(dat)

#Visualize missing data by variable
dat %>% gg_miss_var()
glimpse(dat)

#Select Variables
dat2 <- dat %>% 
  select(Site, Date, Watershed, WsAreaSqKm, tt_hr, streamorde,
         Da,
         season_tb,
         q_daily_mm,
         PctDecid2019Ws, PctConif2019Ws,
         lulc_evenness,
         DOC_mgL, 
         TN_mgL,
         CHON,
         CHON_norm,
         number.of.peaks,
         peaks.with.formula,
         total.transformations, normalized.transformations,
         AI_Mod.mean)

#Select explanatory and dependent variable names
expl_vars <- dat2 %>% 
  select(WsAreaSqKm, tt_hr, Da, lulc_evenness, PctDecid2019Ws, PctConif2019Ws) %>% 
  colnames()
dep_vars <- dat2 %>% 
  select(DOC_mgL, number.of.peaks, peaks.with.formula,
         AI_Mod.mean, CHON_norm, CHON,
         # total.transformations, 
         normalized.transformations) %>% 
  colnames()



#2.0 Generate descriptive statistics by site -----
##2.1 table of number of samples per site and variable -----
dat_site_n <- dat2 %>% 
  select(Watershed, Site, all_of(expl_vars), all_of(dep_vars)) %>% 
  group_by(Watershed, Site) %>%  
  get_summary_stats(type = "common", show = "n") %>%
  pivot_wider(names_from = "variable", values_from = n,
              names_sort = TRUE)
write_csv(dat_site_n, paste0(here, "/output/tables/A_tab_site_n.csv"))

##2.2 table of descriptive statistics by site -----
dat_site_ds <- dat2 %>% 
  select(Watershed, Site, all_of(expl_vars), all_of(dep_vars)) %>% 
  group_by(Watershed, Site) %>%  
  get_summary_stats(type = "full", show = c("n", "mean", "sd", "min", "max")) %>% 
  mutate(cv = sd/mean) %>% 
  unite(col = "n_mean_sd_min_max_cv", n, mean, sd, min, max, cv) %>% 
  unite(col = "sites", Watershed, Site) %>% 
  pivot_wider(names_from = "variable", values_from = n_mean_sd_min_max_cv,
              names_sort = TRUE)

dat_site_ds2 <- names(dat_site_ds) %>% 
  map_dfc(~dat_site_ds %>% select(all_of(.x)) %>%
            separate(.x, into = paste0(.x, c("_n", "_mean", "_sd", "_min", "_max", "_cv")),
                     sep = "_")) %>%
  select(-c(sites_sd, sites_min, sites_max, sites_cv)) %>%
  rename(Watershed = sites_n,
         Site = sites_mean)
write_csv(dat_site_ds2, paste0(here, "/output/tables/B_tab_site_ds.csv"))

##2.3 Stats across all sites -----
dat_watershed_ds <- dat2 %>% 
  select(Watershed, Site, all_of(expl_vars), all_of(dep_vars)) %>% 
  group_by(Watershed) %>%  
  get_summary_stats(type = "full", show = c("n", "mean", "sd", "min", "max")) %>% 
  mutate(cv = sd/mean) %>% 
  unite(col = "n_mean_sd_min_max_cv", n, mean, sd, min, max, cv) %>% 
  pivot_wider(names_from = "variable", values_from = n_mean_sd_min_max_cv,
              names_sort = TRUE)

dat_watershed_ds2 <- names(dat_watershed_ds) %>% 
  map_dfc(~dat_watershed_ds %>% select(all_of(.x)) %>%
            separate(.x, into = paste0(.x, c("_n", "_mean", "_sd", "_min", "_max", "_cv")),
                     sep = "_")) %>%
  select(-c(Watershed_mean, Watershed_sd, Watershed_min, Watershed_max, Watershed_cv)) %>%
  rename(Watershed = Watershed_n)

write_csv(dat_watershed_ds2, paste0(here, "/output/tables/C_tab_watershed_ds.csv"))

#Descriptive statistics for comparison by watershed
dat_watershed_ds3 <- dat_watershed_ds2 %>% 
  pivot_longer(-Watershed) %>%
  pivot_wider(names_from = Watershed) %>% 
  mutate(across(-name, as.numeric)) %>% 
  # select(contains("_n")) %>%
  rowwise() %>%
  mutate(min = min(across(-name)),
         max = max(across(-name)))

write_csv(dat_watershed_ds3, paste0(here, "/output/tables/D_tab_watershed_ds2.csv"))


#clean environment
to_remove <- ls() %>% as_tibble() %>%
  filter(str_detect(value, "here", negate = TRUE)) %>%
  pull()
rm(list = to_remove)

