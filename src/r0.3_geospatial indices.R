#0.0 Setup----
library(here)
here <- here()
library(tidyverse)
library(naniar)
library(rstatix)
library(FactoMineR)
library("devtools")
# install_github("kassambara/factoextra")
library(factoextra)
library(corrplot)
library(ggpubr)
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
  
#Select targeted geospatial variables
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
  )
glimpse(geospat3)


geospat4 <- geospat3 %>% 
  convert_as_factor(vars = c("site"#, "streamorde"
                             )) %>% 
  left_join(., meta %>% select(Site, Watershed), by = c("site" = "Site")) %>% 
  select(Watershed, site, everything()) %>% 
  distinct(site, .keep_all = TRUE) %>% 
  rowid_to_column(var = "rowid") %>%
  column_to_rownames(var = "site") %>% 
  drop_na(streamorde)

gg_miss_var(geospat4) #3 sites missing some data



#2.0 PCA----
##2.1 Define input variables----
vars_active <- names(geospat4[,3:length(geospat4)])
vars_supl <- names(geospat4[,1:2])
vars_watershed <- geospat4 %>% 
  select(streamorde,
         ElevWs, #mean elevation in meters
         TmeanSite, #mean 30 year temperature in Celsius for selected years
         PrecipSite, #mean 30 year precipitation (mm)
         RunoffWs, #mean runoff (mm)
         # areasqkm, CatAreaSqKm, 
         WsAreaSqKm,
         # slope,
         # RdDensWs, #mean road density (km/sq.km)
         # KffactWs, #mean soil erodibility factor (Kf, unitless)
         OmWs, #mean percent organic matter soil content
         # DamDensWs, #dams/sq.km
         # HydrlCondWs, #mean hydraulic conductivity of surface geologic (micrometers/second)
         ClayWs, #mean percent clay content of soils
         SandWs, #mean percent sand content of soils
         # PermWs, #mean permeability of soils (cm/hour)
         PctDecid2019Ws, #percent deciduous forest
         PctConif2019Ws #percent deciduous forest
  ) %>% names()

vars_lulc <- geospat4 %>% 
  select(PctConif2019Ws, #percent deciduous forest,
         PctOw2019Ws, #percent open water in 2019
         PctMxFst2019Ws, #percent mixed forest
         PctDecid2019Ws, #percent deciduous forest
         PctCrop2019Ws, #percent crop land use
         PctWdWet2019Ws, #percent woody wetland cover
         PctHbWet2019Ws, #percent herbaceous wetland cover
         PctUrbHi2019Ws, #percent high intensity urban development 
         # PctImp2019Ws, #mean percent impervious land cover for 2019
         # PopDen2010Ws, #percent population density in 2010 Census
         # RdDensWs, #mean road density (km/sq.km)
         # WWTPAllDensWs, #waster water treatment plant density (#/sq.km)
         OmWs, #mean percent organic matter soil content
         # DamDensWs, #dams/sq.km
         # HydrlCondWs, #mean hydraulic conductivity of surface geologic (micrometers/second)
         ClayWs, #mean percent clay content of soils
         SandWs, #mean percent sand content of soils
         # PermWs, #mean permeability of soils (cm/hour)
  ) %>% names()

#plot QQ plots to visually inspect for normality
geospat4 %>% 
  pivot_longer(all_of(vars_active)) %>% 
  group_by(name) %>% 
  ggqqplot("value", facet.by = "name", scale = "free")
geospat4 %>% 
  pivot_longer(all_of(vars_watershed)) %>% 
  group_by(name) %>% 
  ggqqplot("value", facet.by = "name", scale = "free")
geospat4 %>% 
  pivot_longer(all_of(vars_lulc)) %>% 
  group_by(name) %>% 
  ggqqplot("value", facet.by = "name", scale = "free")

#Assess overall correlations
corr <- cor(geospat4 %>% select(all_of(vars_active)))
png(height=6, width=6, units = "in", res = 500,
    file=paste0(here, "/output/geospat/corrplot_active.png"),
    type = "cairo")
corrplot(corr, diag = TRUE,
         title = "Correlations",
         mar=c(0,0,1,0), tl.col = "black")
dev.off()


#get column indices for PCA input
pca_vars_active <- match(vars_active, names(geospat4))
pca_vars_supl <- match(vars_supl, names(geospat4))
pca_vars_watershed <- match(vars_watershed, names(geospat4))
pca_vars_lulc <- match(vars_lulc, names(geospat4))

##2.2 Run PCA----
rownames(geospat4)
res.pca_active <- PCA(X = geospat4, scale.unit = TRUE,
               quali.sup = pca_vars_supl)
res.pca_watershed <-PCA(X = geospat4 %>% 
                          select(all_of(c(pca_vars_supl, pca_vars_watershed))),
                        scale.unit = TRUE,
                        quali.sup = pca_vars_supl)
res.pca_lulc <-PCA(X = geospat4 %>% 
                          select(all_of(c(pca_vars_supl, pca_vars_lulc))),
                        scale.unit = TRUE,
                        quali.sup = pca_vars_supl)
res.pca_lulc_west <-PCA(X = geospat4 %>% 
                          filter(Watershed != "Connecticut") %>% 
                          select(all_of(c(pca_vars_supl, pca_vars_lulc))),
                   scale.unit = TRUE,
                   quali.sup = pca_vars_supl)

##2.3 Plot PCA Results----
#Visualize the eigenvalues, scree plots
fviz_eig(res.pca_active, ncp = 5,
         title = "A) PCA Scree Plot for All Variables")
ggsave(paste0(here, "/output/geospat/pca/pca_active_scree.png"))
fviz_eig(res.pca_watershed, ncp = 5,
         title = "A) PCA Scree Plot for Watershed Variables")
ggsave(paste0(here, "/output/geospat/pca/pca_watershed_scree.png"))
fviz_eig(res.pca_lulc, ncp = 5,
         title = "A) PCA Scree Plot for LULC Variables")
ggsave(paste0(here, "/output/geospat/pca/pca_lulc_scree.png"))
fviz_eig(res.pca_lulc_west, ncp = 5,
         title = "A) PCA Scree Plot for LULC West Variables")
ggsave(paste0(here, "/output/geospat/pca/pca_lulc_west_scree.png"))

#Extract the results for individuals and variables, respectively
#Visualize the results individuals and variables, respectively.
## Principal Component Analysis Results for variables
##   Name       Description                                    
## 1 "$coord"   "Coordinates for the variables"                
## 2 "$cor"     "Correlations between variables and dimensions"
## 3 "$cos2"    "Cos2 for the variables"                       
## 4 "$contrib" "contributions of the variables"
pca_var_active <- get_pca_var(res.pca_active) 
pca_var_watershed <- get_pca_var(res.pca_watershed) 
pca_var_lulc <- get_pca_var(res.pca_lulc) 
pca_var_lulc_west <- get_pca_var(res.pca_lulc_west) 

#Visualize Variables
fviz_pca_var(res.pca_active,
             arrowsize = 1,
             labelsize = 3,
             col.var = "contrib",
             col.quanti.sup = "blue",
             repel = TRUE,
             title = "A")
ggsave(paste0(here, "/output/geospat/pca/pca_vars_active.png"))
fviz_pca_var(res.pca_watershed,
             arrowsize = 1,
             labelsize = 3,
             col.var = "contrib",
             col.quanti.sup = "blue",
             repel = TRUE,
             title = "A")
ggsave(paste0(here, "/output/geospat/pca/pca_vars_watershed.png"))
fviz_pca_var(res.pca_lulc,
             arrowsize = 1,
             labelsize = 3,
             col.var = "contrib",
             col.quanti.sup = "blue",
             repel = TRUE,
             title = "A")
ggsave(paste0(here, "/output/geospat/pca/pca_vars_lulc.png"))
fviz_pca_var(res.pca_lulc_west,
             arrowsize = 1,
             labelsize = 3,
             col.var = "contrib",
             col.quanti.sup = "blue",
             repel = TRUE,
             title = "A")
ggsave(paste0(here, "/output/geospat/pca/pca_vars_lulc_west.png"))

#Visualize Individuals
pca_ind_active <- get_pca_ind(res.pca_active)$coord
pca_ind_watershed <- get_pca_ind(res.pca_watershed)
pca_ind_lulc <- get_pca_ind(res.pca_lulc)
pca_ind_lulc_west <- get_pca_ind(res.pca_lulc_west)

fviz_pca_ind(res.pca_active, repel = TRUE, 
             habillage = 2, addEllipses = TRUE, 
             col.ind.sup = "black", label = "none",
             title = "PCA Individuals")
ggsave(paste0(here, "/output/geospat/pca/pca_active_ind.png"))
fviz_pca_ind(res.pca_watershed, repel = TRUE, 
             habillage = 2, addEllipses = TRUE, 
             col.ind.sup = "black", label = "none",
             title = "PCA Individuals")
ggsave(paste0(here, "/output/geospat/pca/pca_watershed_ind.png"))
fviz_pca_ind(res.pca_lulc, repel = TRUE, 
             habillage = 2, addEllipses = TRUE, 
             col.ind.sup = "black", label = "none",
             title = "PCA Individuals")
ggsave(paste0(here, "/output/geospat/pca/pca_lulc_ind.png"))
fviz_pca_ind(res.pca_lulc_west, repel = TRUE, 
             habillage = 2, addEllipses = TRUE, 
             col.ind.sup = "black", label = "none",
             title = "PCA Individuals")
ggsave(paste0(here, "/output/geospat/pca/pca_lulc_west_ind.png"))



#PCA Components Corrplots
#cannot use ggsave for corrplot objects, but use base functions
png(height=6, width=6, units = "in", res = 500,
    file=paste0(here, "/output/geospat/pca/pca_corr_active.png"),
    type = "cairo")
corrplot(pca_var_active$cor, title = "PCA Correlations",
         mar=c(0,0,1,0), tl.col = "black")
dev.off()
png(height=6, width=6, units = "in", res = 500,
    file=paste0(here, "/output/geospat/pca/pca_corr_watershed.png"),
    type = "cairo")
corrplot(pca_var_watershed$cor, title = "PCA Correlations",
         mar=c(0,0,1,0), tl.col = "black")
dev.off()
png(height=6, width=6, units = "in", res = 500,
    file=paste0(here, "/output/geospat/pca/pca_corr_lulc.png"),
    type = "cairo")
corrplot(pca_var_lulc$cor, title = "PCA Correlations",
         mar=c(0,0,1,0), tl.col = "black")
dev.off()
png(height=6, width=6, units = "in", res = 500,
    file=paste0(here, "/output/geospat/pca/pca_corr_lulc_west.png"),
    type = "cairo")
corrplot(pca_var_lulc_west$cor, title = "PCA Correlations",
         mar=c(0,0,1,0), tl.col = "black")
dev.off()

#to further investigate which variables are explained by each principal component
fviz_cos2(res.pca_active, choice = "var", axes = 1)
fviz_cos2(res.pca_active, choice = "var", axes = 2)
fviz_cos2(res.pca_active, choice = "var", axes = 3)

fviz_cos2(res.pca_watershed, choice = "var", axes = 1)
fviz_cos2(res.pca_watershed, choice = "var", axes = 2)
fviz_cos2(res.pca_watershed, choice = "var", axes = 3)

fviz_cos2(res.pca_lulc, choice = "var", axes = 1)
fviz_cos2(res.pca_lulc, choice = "var", axes = 2)
fviz_cos2(res.pca_lulc, choice = "var", axes = 3)

fviz_cos2(res.pca_lulc_west, choice = "var", axes = 1)
fviz_cos2(res.pca_lulc_west, choice = "var", axes = 2)
fviz_cos2(res.pca_lulc_west, choice = "var", axes = 3)

# Contributions of variables to PCs
fviz_contrib(res.pca_active, choice = "var", axes = 1, top = 100)
fviz_contrib(res.pca_active, choice = "var", axes = 2, top = 100)

fviz_contrib(res.pca_watershed, choice = "var", axes = 1, top = 100)
fviz_contrib(res.pca_watershed, choice = "var", axes = 2, top = 100)

fviz_contrib(res.pca_lulc, choice = "var", axes = 1, top = 100)
fviz_contrib(res.pca_lulc, choice = "var", axes = 2, top = 100)

fviz_contrib(res.pca_lulc_west, choice = "var", axes = 1, top = 100)
fviz_contrib(res.pca_lulc_west, choice = "var", axes = 2, top = 100)

###Sand, Clay, Deciduous, and Conifer are driving variability###

##2.4Extract PCA loadings----
geospat5 <- pca_ind_lulc$coord %>% as_tibble() %>% 
  rownames_to_column(var = "rowid") %>% 
  mutate(across(rowid, as.integer)) %>% 
  left_join(geospat4 %>% rownames_to_column("site"), ., by = "rowid") %>% 
  select(-c(rowid, Dim.3, Dim.4, Dim.5)) %>% 
  rename("lulc_pca1" = "Dim.1",
         "lulc_pca2" = "Dim.2") %>% 
  mutate(decid_conif = PctDecid2019Ws/PctConif2019Ws,
         clay_sand = ClayWs/SandWs,
         claysand_decidconif = clay_sand/decid_conif)

#Plot principal components
ggplot(geospat5) +
  geom_point(aes(x= lulc_pca1, y= lulc_pca2, color = Watershed),
             size = 4) +
  theme_bw()
ggsave(paste0(here, "/output/geospat/lulc_pca1v2.png"))

# ggplot(geospat2) +
#   geom_point(aes(x= claysand_decidconif, y= WsAreaSqKm, color = Watershed),
#              size = 4) +
#   scale_x_log10()+
#   theme_bw()
# saveRDS(geospat4, paste0(here, "/output/geospat/geospat_indices.Rds"))

#clean environment
# to_remove <- ls() %>% as_tibble() %>%
#   filter(str_detect(value, "here", negate = TRUE)) %>%
#   pull()
# rm(list = to_remove)


#3.0 Land Use Diversity Index-----
# geospat2 <- readRDS(paste0(here, "/output/geospat/geospat_indices.Rds"))

#number of types
k <- geospat5 %>% select(contains("Pct")) %>% colnames() %>% length()

#Adapted Shannon Diversity Evenness
#-sum(proportion type*log proportion for each site)/log(k)
geospat6 <- geospat5 %>% 
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
  left_join(geospat5, ., by = "site")

# geospat6 <- geospat5 %>% select(-Watershed)
# saveRDS(geospat6, paste0(here, "/output/geospat/geospat_indices.Rds"))

#4.0 Plots -----

ggplot(geospat6) +
  geom_boxplot(aes(x=Watershed, y= lulc_evenness)) +
  geom_jitter(aes(x=Watershed, y= lulc_evenness, color = site), size = 3) +
  theme_bw()
ggsave(paste0(here, "/output/geospat/lulc_eveness.png"))

ggplot(geospat6) +
  geom_boxplot(aes(x=Watershed, y= lulc_H)) +
  geom_jitter(aes(x=Watershed, y= lulc_H, color = site), size = 3) +
  theme_bw()
ggsave(paste0(here, "/output/geospat/lulc_H.png"))

ggplot(geospat6) +
  geom_point(aes(x=SandWs, y= ClayWs, color = OmWs, shape = Watershed),
             size = 4) +
  theme_bw()
ggsave(paste0(here, "/output/geospat/clay_sand_om.png"))

ggplot(geospat6) +
  geom_point(aes(y=ClayWs/SandWs, x= PctConif2019Ws/PctDecid2019Ws, color = Watershed),
             size = 4) +
  scale_x_log10() +
  # geom_point(aes(x=PctConif2019Ws, y= SandWs, color = Watershed),
  #            size = 4) +
  theme_bw()
ggsave(paste0(here, "/output/geospat/ConDec_ClaySand.png"))

#5.0 SAVE
saveRDS(geospat6 %>% select(-Watershed),
        paste0(here, "/output/geospat/geospat_indices.Rds"))


#clean environment
to_remove <- ls() %>% as_tibble() %>%
  filter(str_detect(value, "here", negate = TRUE)) %>%
  pull()
rm(list = to_remove)


#old code
#Save remaining sites for filling data
# geospat_to_fill <- geospat2[0,] %>% add_row(site = sites_missing) %>% drop_na(site)
# write_csv(geospat_to_fill, paste0(here, "/output/geospat/geospat_to_fill.csv"))
# geospat_filled <- read_csv(paste0(here, "/output/geospat/geospat_to_fill_kr.csv"),
#                            skip = 1) %>% 
#   convert_as_factor(vars = c("site"#, "streamorde"
#   ))
