# setup ----
library(tidyverse)
library(vegan)
library(here)
here <- here()
here


dat <- readRDS(paste0(here, "/output/dat_wide.Rds"))
glimpse(dat)

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
vars_lulc <- dat %>% select(PctDecid2019Ws, PctConif2019Ws,
  # PctOw2019Ws, #percent open water in 2019
  # PctMxFst2019Ws, #percent mixed forest
  # PctCrop2019Ws, #percent crop land use
  # PctWdWet2019Ws, #percent woody wetland cover
  # PctHbWet2019Ws, #percent herbaceous wetland cover
  # PctUrbHi2019Ws, #percent high intensity urban development 
  ClayWs, SandWs, #OmWs,
  # lulc_pca1, lulc_pca2,
  # decid_conif, clay_sand, claysand_decidconif,
  lulc_evenness) %>%
  names()
vars_chem <- dat %>% select(#DOC_mgL, 
  # TN_mgL,
  # number.of.peaks, 
  # total.transformations, 
  normalized.transformations,
  AI_Mod.mean, DBE.mean, 
  # NOSC.mean,
  # GFE_0.mean, lambdaO2_0.mean,
  # Lignin, Tannin, Protein, Lipid, Carbohydrate, Other,
  HtoC.mean, NtoC.mean,
  OtoC.mean,
  contains("_norm")) %>% names()

vars_supl <- dat %>% select(dataset, Watershed, Site, season_tb) %>% names()


#Data for multivariate analysis
#All selected
dat_mv <- dat %>% 
  column_to_rownames(var = "sample") %>% 
  select(all_of(c(#vars_supl, vars_watershed, vars_lulc, 
    vars_chem))) %>% 
  # mutate(across(where(is.numeric), ~na_if(., c(Inf, -Inf)))) %>% 
  as.matrix() 
is.matrix(dat_mv)
rownames(dat_mv)

#NMDS
set.seed(3)
nmds <- metaMDS(comm = dat_mv , distance = "bray", k=3,
                autotransform = TRUE, na.rm =TRUE)

plot(nmds)
stressplot(nmds) #low scatter indicates nmds ordination good representation of original data
stress <- nmds$stress #stress should be below 0.1

#Get ordination data
#nmds$points contains the positions of each sample in nmds dimensions
#scores() vegan function that gets $points
ord_samp <- scores(nmds, "sites") %>% as_tibble(rownames = "sample") 
ord_vars <- scores(nmds, "species") %>% as_tibble(rownames = "vars")

#Join back to ancillary environmental variables
ord_samp2 <- left_join(ord_samp, dat, by = "sample")

#plot samples in NMDS space
ggplot()+
  geom_point(data= ord_samp2,
             aes(x=NMDS1, y=NMDS2, color= Watershed), size = 4)+
  # geom_point(data= ord_samp2,
  #            aes(x=NMDS1, y=NMDS2, color= Site), size = 4)+
  geom_point(data= ord_vars,
             aes(x=NMDS1, y=NMDS2, shape = vars), size = 5)+
  theme_bw()
ggsave(paste0(here, "/output/multivar/nmds/nmds_chem.png"))


#fit environmental vectors
#fit vectors that point in direction of linearly increasing abundance through ordination space
env_ord <- envfit(ord = nmds,
                  env = dat %>% 
                select(all_of(c(vars_supl, vars_watershed, vars_lulc))),
                  na.rm = TRUE)

#extract vector data for plotting
arrows <- env_ord$vectors$arrows %>% as_tibble(rownames = "env_vars")
r <- env_ord$vectors$r %>% as_tibble(rownames = "env_vars") %>% 
  rename("r" = "value")
pval <- env_ord$vectors$pval %>% as_tibble(rownames = "env_vars") %>% 
  rename("pval" = "value")
vectors <- left_join(arrows, r, by = "env_vars") %>% 
  left_join(., pval, by= "env_vars") %>% 
  filter(pval < 0.1) %>% 
  mutate(across(.cols = c(NMDS1,NMDS2), ~.*sqrt(r)))


# spp.scrs <- as.data.frame(scores(env_ord, display = "vectors"))
# arrow_factor <- ordiArrowMul(envfit)
#plot vectors
p_nmds <- ggplot() +
  geom_point(data= ord_samp2,
             aes(x=NMDS1, y=NMDS2, color= Watershed),
             size = 5, alpha= 0.8)+
  geom_point(data= ord_vars,
             aes(x=NMDS1, y=NMDS2, shape = vars), size = 5)+
  geom_segment(data = vectors,
               aes(x = 0, xend = NMDS1*1,
                   y = 0, yend = NMDS2*1),
               arrow = arrow(length = unit(0.25, "cm")), color = "black") +
  geom_text(data = vectors,
            aes(x = NMDS1*1,
                y = NMDS2*1, label = env_vars),
            color = "black", size = 3) +
  labs(title = "NMDS Environmental Variable Overlay") +
  theme_bw()
p_nmds

ggsave(plot = p_nmds, 
       filename =  paste0(here, "/output/multivar/nmds/nmds_env.png"),
       device = "png", width=8, height=8)




#clean environment
to_remove <- ls() %>% as_tibble() %>%
  filter(str_detect(value, "here", negate = TRUE)) %>%
  pull()
rm(list = to_remove)
