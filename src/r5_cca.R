# setup ----
library(tidyverse)
library(vegan)
library(here)
here <- here()
here
#https://rfunctions.blogspot.com/2016/11/canonical-correspondence-analysis-cca.html


dat <- readRDS(paste0(here, "/output/dat_wide.Rds"))
glimpse(dat)

##2.1 Define input variables----
# vars_active <- dat %>% select(where(is.numeric)) %>% names()
# vars_geospat <- dat %>% select(ElevWs:lulc_evenness) %>% names()
vars_watershed <- dat %>% select(WsAreaSqKm, 
                                 # tt_hr, q_daily_mm,
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
  OtoC.mean) %>% names()

vars_supl <- dat %>% select(dataset, Watershed, 
                            # Site,
                            season_n, season_tb) %>% names()


#Data for multivariate analysis
#Cummunity data
dat_mv <- dat %>% 
  column_to_rownames(var = "sample") %>% 
  select(all_of(c(#vars_supl, vars_watershed, vars_lulc, 
    vars_chem))) 
rownames(dat_mv)

dat_env <- dat %>% 
  column_to_rownames(var = "sample") %>% 
  select(all_of(c(vars_supl, vars_lulc, vars_watershed))) %>% 
  mutate(across(where(is.character), factor))

# dat_condition <- dat %>% 
#   column_to_rownames(var = "sample") %>% 
#   select(all_of(c(vars_watershed))) %>% 
#   mutate(across(where(is.character), factor))

#CCA
# cca <- cca(X= dat_mv, Y= dat_env, Z= dat_condition)
# cca <- cca(X= dat_mv, Y= dat_env)
cca <- cca(formula= dat_mv ~ lulc_evenness+PctConif2019Ws+PctDecid2019Ws+
             WsAreaSqKm+ClayWs+SandWs+season_tb,
           data = dat)

cca
summary(cca, scaling = "site", display = NULL)
plot(cca)
screeplot(cca)

#Get ordination data
#scores() vegan function
ord_samp <- scores(cca, "sites", choices = c(1,2)) %>%
  as_tibble(rownames = "sample") 
ord_vars <- scores(cca, "species", choices = c(1,2)) %>%
  as_tibble(rownames = "vars")
ord_env <- scores(cca, choices= c(1,2), tidy=TRUE) %>% 
  as_tibble(rownames = "env_vars")

#Obtain scale factor for arrow vectors for plotting
library(ggvegan)
mul <- ggvegan:::arrow_mul(arrows= ord_env %>% 
                           filter(score == "biplot") %>% 
                           select(CCA1, CCA2),
                         data= ord_samp %>% 
                           select(CCA1, CCA2))

#add ancillary environmental variables
ord_samp2 <- left_join(ord_samp, dat, by = "sample")

p_cca <- ggplot()+
  geom_point(data= ord_samp2,
             aes(x=CCA1, y=CCA2, color= Watershed), size = 4) +
  geom_segment(data = ord_vars,
               aes(x = 0, xend = CCA1,
                   y = 0, yend = CCA2),
               arrow = arrow(length = unit(0.25, "cm")), colour = "black") +
  geom_text(data = ord_vars,
            aes(x = CCA1*1,
                y = CCA2*1, label= vars),
            color = "black", size = 3) +
  geom_segment(data= ord_env %>% filter(score == "biplot"),
             aes(x= 0, xend= CCA1*mul,
                 y= 0, yend= CCA2*mul)) +
  geom_text(data = ord_env %>% filter(score == "biplot"),
            aes(x = CCA1*mul,
                y = CCA2*mul, label= env_vars),
            color = "black", size = 3) +
  labs(title = "CCA") +
  theme_bw() #+ coord_fixed()
p_cca
# coord_fixed()

library(plotly)
ggplotly(p_cca)
ggsave(plot = p_cca, 
       filename =  paste0(here, "/output/multivar/cca/cca_.png"),
       device = "png", width=8, height=8)



# anova_mod <- anova(cca)
# anova_env <- anova.cca(finalmodel, by="terms")
# anova_ord <- anova.cca(finalmodel, by="axis")

# t <- goodness(cca)
finalmodel <- ordistep(cca, scope=formula(cca))
vif.cca(finalmodel)

#clean environment
to_remove <- ls() %>% as_tibble() %>%
  filter(str_detect(value, "here", negate = TRUE)) %>%
  pull()
rm(list = to_remove)
