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

dat <- readRDS(paste0(here, "/output/dat_wide.Rds"))
glimpse(dat)



#2.0 PCA----
##2.1 Define input variables----
# vars_active <- dat %>% select(where(is.numeric)) %>% names()
# vars_geospat <- dat %>% select(ElevWs:lulc_evenness) %>% names()
vars_watershed <- dat %>% select(WsAreaSqKm, streamorde,
                                 tt_hr, q_daily_mm,
                                 ElevWs, #mean elevation in meters
                                 TmeanSite, #mean 30 year temperature in Celsius for selected years
                                 PrecipSite, #mean 30 year precipitation (mm)
                                 RunoffWs, #mean runoff (mm)
                                 ) %>% names()
vars_lulc <- dat %>% select(#PctDecid2019Ws, PctConif2019Ws,
                            # PctOw2019Ws, #percent open water in 2019
                            # PctMxFst2019Ws, #percent mixed forest
                            # PctCrop2019Ws, #percent crop land use
                            # PctWdWet2019Ws, #percent woody wetland cover
                            # PctHbWet2019Ws, #percent herbaceous wetland cover
                            # PctUrbHi2019Ws, #percent high intensity urban development 
                            # ClayWs, SandWs, OmWs,
                            # lulc_pca1, lulc_pca2,
                            # decid_conif, clay_sand, claysand_decidconif,
                            lulc_evenness) %>%
  names()
vars_chem <- dat %>% select(#DOC_mgL, 
                            # TN_mgL,
                            # number.of.peaks, 
                            # total.transformations, 
                            normalized.transformations,
                            AI_Mod.mean, DBE.mean, NOSC.mean,
                            # GFE_0.mean, lambdaO2_0.mean,
                            # Lignin, Tannin, Protein, Lipid, Carbohydrate, Other,
                            contains("_norm"),
                            HtoC.mean, NtoC.mean,
                            OtoC.mean) %>% names()

vars_supl <- dat %>% select(dataset, Watershed, Site, season_tb) %>% names()


#plot QQ plots to visually inspect for normality
dat %>% 
  pivot_longer(all_of(vars_chem)) %>% 
  group_by(name) %>% 
  ggqqplot("value", facet.by = "name", scale = "free")
dat %>% 
  pivot_longer(all_of(vars_lulc)) %>% 
  group_by(name) %>% 
  ggqqplot("value", facet.by = "name", scale = "free")

gg_miss_var(dat) #Check variables with missing data
#5 samples with missing DOC
#6 infinite values converted to NAs

#Assess overall correlations
#Chemical
corr <- cor(dat %>% select(all_of(vars_chem)))
png(height=6, width=6, units = "in", res = 500,
    file=paste0(here, "/output/multivar/corrplot_chem.png"),
    type = "cairo")
corrplot(corr, diag = TRUE,
         title = "Correlations",
         mar=c(0,0,1,0), tl.col = "black")
dev.off()


#Data for multivariate analysis
#All selected
dat_mv <- dat %>% 
  rowid_to_column(var = "rowid") %>%
  column_to_rownames(var = "sample") %>% 
  select(all_of(c(vars_supl, vars_watershed, vars_lulc, vars_chem))) %>% 
  mutate(across(where(is.numeric), ~na_if(., Inf)))
glimpse(dat_mv)

#Column indices for PCA input
pca_vars_supl <- match(vars_supl, names(dat_mv))
pca_vars_watershed <- match(vars_watershed, names(dat_mv))
pca_vars_lulc <- match(vars_lulc, names(dat_mv))
pca_vars_chem <- match(vars_chem, names(dat_mv))




##2.2 Run PCA----
rownames(dat_mv)
res.pca_all <-PCA(X = dat_mv, scale.unit = TRUE,
                        quali.sup = pca_vars_supl)
res.pca_chem <-PCA(X = dat_mv,
                   scale.unit = TRUE,
                   quali.sup = pca_vars_supl,
                   quanti.sup = c(pca_vars_watershed))
res.pca_chem_wrol <-PCA(X = dat_mv %>% filter(Watershed != "Yakima"),
                        scale.unit = TRUE,
                        quali.sup = pca_vars_supl,
                        quanti.sup = c(pca_vars_watershed, pca_vars_lulc))

##2.3 Plot PCA Results----
#Visualize the eigenvalues, scree plots
fviz_eig(res.pca_all, ncp = 5,
         title = "A) PCA Scree Plot for All Variables")
ggsave(paste0(here, "/output/multivar/pca/pca_all_scree.png"))
fviz_eig(res.pca_chem, ncp = 5,
         title = "A) PCA Scree Plot for Chemistry Variables")
ggsave(paste0(here, "/output/multivar/pca/pca_chem_scree.png"))
fviz_eig(res.pca_chem, ncp = 5,
         title = "A) PCA Scree Plot for Chemistry Variables WROL")
ggsave(paste0(here, "/output/multivar/pca/pca_chem_wrol_scree.png"))


#Extract the results for individuals and variables, respectively
#Visualize the results individuals and variables, respectively.
## Principal Component Analysis Results for variables
##   Name       Description                                    
## 1 "$coord"   "Coordinates for the variables"                
## 2 "$cor"     "Correlations between variables and dimensions"
## 3 "$cos2"    "Cos2 for the variables"                       
## 4 "$contrib" "contributions of the variables"
pca_var_all <- get_pca_var(res.pca_all) 
pca_var_chem <- get_pca_var(res.pca_chem) 
pca_var_chem_wrol <- get_pca_var(res.pca_chem) 


#Visualize Variables
fviz_pca_var(res.pca_all,
             arrowsize = 1,
             labelsize = 3,
             col.var = "contrib",
             col.quanti.sup = "blue",
             repel = TRUE,
             title = "A")
ggsave(paste0(here, "/output/multivar/pca/pca_vars_all.png"))
fviz_pca_var(res.pca_chem,
             arrowsize = 1,
             labelsize = 3,
             col.var = "contrib",
             col.quanti.sup = "blue",
             repel = TRUE,
             title = "A")
ggsave(paste0(here, "/output/multivar/pca/pca_vars_chem.png"))
fviz_pca_var(res.pca_chem_wrol,
             arrowsize = 1,
             labelsize = 3,
             col.var = "contrib",
             col.quanti.sup = "blue",
             repel = TRUE,
             title = "A")
ggsave(paste0(here, "/output/multivar/pca/pca_vars_chem_wrol.png"))

#Visualize Individuals
pca_ind_all <- get_pca_ind(res.pca_all)
pca_ind_chem <- get_pca_ind(res.pca_chem)
pca_ind_chem_wrol <- get_pca_ind(res.pca_chem_wrol)

fviz_pca_ind(res.pca_all, repel = TRUE, 
             habillage = 2, addEllipses = TRUE, 
             col.ind.sup = "black", label = "none",
             title = "PCA Individuals")
ggsave(paste0(here, "/output/multivar/pca/pca_all_ind.png"))
fviz_pca_ind(res.pca_chem, repel = TRUE, 
             habillage = 2, addEllipses = TRUE, 
             col.ind.sup = "black", label = "none",
             title = "PCA Individuals")
ggsave(paste0(here, "/output/multivar/pca/pca_chem_ind.png"))
fviz_pca_ind(res.pca_chem_wrol, repel = TRUE, 
             habillage = 2, addEllipses = TRUE, 
             col.ind.sup = "black", label = "none",
             title = "PCA Individuals")
ggsave(paste0(here, "/output/multivar/pca/pca_chem_wrol_ind.png"))

#PCA Components Corrplots
#cannot use ggsave for corrplot objects, but use base functions
png(height=6, width=6, units = "in", res = 500,
    file=paste0(here, "/output/multivar/pca/pca_corr_all.png"),
    type = "cairo")
corrplot(pca_var_all$cor, title = "PCA Correlations",
         mar=c(0,0,1,0), tl.col = "black")
dev.off()
png(height=6, width=6, units = "in", res = 500,
    file=paste0(here, "/output/multivar/pca/pca_corr_chem.png"),
    type = "cairo")
corrplot(pca_var_chem$cor, title = "PCA Correlations",
         mar=c(0,0,1,0), tl.col = "black")
dev.off()
png(height=6, width=6, units = "in", res = 500,
    file=paste0(here, "/output/multivar/pca/pca_corr_chem_wrol.png"),
    type = "cairo")
corrplot(pca_var_chem_wrol$cor, title = "PCA Correlations",
         mar=c(0,0,1,0), tl.col = "black")
dev.off()

#to further investigate which variables are explained by each principal component
fviz_cos2(res.pca_all, choice = "var", axes = 1)
fviz_cos2(res.pca_all, choice = "var", axes = 2)
fviz_cos2(res.pca_all, choice = "var", axes = 3)

fviz_cos2(res.pca_chem, choice = "var", axes = 1)
fviz_cos2(res.pca_chem, choice = "var", axes = 2)
fviz_cos2(res.pca_chem, choice = "var", axes = 3)

fviz_cos2(res.pca_chem_wrol, choice = "var", axes = 1)
fviz_cos2(res.pca_chem_wrol, choice = "var", axes = 2)
fviz_cos2(res.pca_chem_wrol, choice = "var", axes = 3)


# Contributions of variables to PCs
fviz_contrib(res.pca_all, choice = "var", axes = 1, top = 100)
fviz_contrib(res.pca_all, choice = "var", axes = 2, top = 100)

fviz_contrib(res.pca_chem, choice = "var", axes = 1, top = 100)
fviz_contrib(res.pca_chem, choice = "var", axes = 2, top = 100)

fviz_contrib(res.pca_chem_wrol, choice = "var", axes = 1, top = 100)
fviz_contrib(res.pca_chem_wrol, choice = "var", axes = 2, top = 100)


##2.4Extract PCA loadings----
dat_pca <- pca_ind_chem$coord %>% as.data.frame() %>% 
  rownames_to_column(var = "sample") %>% 
  select(-c(Dim.4, Dim.5)) %>% 
  rename("chem_pca1" = "Dim.1",
         "chem_pca2" = "Dim.2",
         "chem_pca3" = "Dim.3") %>% 
  left_join(dat, ., by = "sample")
glimpse(dat_pca)

#Plot principal components
ggplot(dat_pca) +
  geom_point(aes(x= chem_pca1, y= chem_pca2, color = Watershed),
             size = 4) +
  theme_bw()
ggsave(paste0(here, "/output/multivar/pca/chem_pca1v2.png"))


saveRDS(dat_pca, paste0(here, "/output/dat_wide_pca.Rds"))


#Too see what PCA looks like without Yakima:
dat_pca_wrol <- pca_ind_chem_wrol$coord %>% as.data.frame() %>% 
  rownames_to_column(var = "sample") %>% 
  select(-c(Dim.4, Dim.5)) %>% 
  rename("chem_pca1" = "Dim.1",
         "chem_pca2" = "Dim.2",
         "chem_pca3" = "Dim.3") %>% 
  left_join(dat, ., by = "sample")

#Plot principal components
ggplot(dat_pca_wrol) +
  geom_point(aes(x= chem_pca1, y= chem_pca2, color = Watershed),
             size = 4) +
  theme_bw()




#clean environment
to_remove <- ls() %>% as_tibble() %>%
  filter(str_detect(value, "here", negate = TRUE)) %>%
  pull()
rm(list = to_remove)


