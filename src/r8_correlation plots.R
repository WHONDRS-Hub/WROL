#0.0 Setup----
library(here)
here <- here()
library(tidyverse)
library(ggpubr)
library(naniar)
library(rstatix)

#1.0 Input and Rearrange Data----
dat <- readRDS(paste0(here, "/output/dat_wide.Rds")) %>% 
  convert_as_factor(vars= c("Watershed", "season_tb", "Site", "streamorde"))
glimpse(dat)

#check missing data by variable
dat %>% gg_miss_var()
glimpse(dat)

#Select Variables
dat2 <- dat %>% 
  select(Site, Date, Watershed, WsAreaSqKm, tt_hr, streamorde,
         season_tb,
         q_daily_mm,
         PctDecid2019Ws, PctConif2019Ws,
         lulc_evenness,
         DOC_mgL, 
         TN_mgL,
         number.of.peaks,
         total.transformations, normalized.transformations,
         AI_Mod.mean)

expl_vars <- dat2 %>% 
  select(WsAreaSqKm, tt_hr, lulc_evenness, PctDecid2019Ws, PctConif2019Ws) %>% 
  colnames()
dep_vars <- dat2 %>% 
  select(DOC_mgL, number.of.peaks, AI_Mod.mean, total.transformations, normalized.transformations) %>% 
  colnames()

#pivot long for DOM metrics representing richness, diversity, and composition (AI)
#pivot long for all explanatory and dependent variables
dat3 <- dat2 %>% 
  pivot_longer(cols = all_of(dep_vars),
               names_to= "dep_names", values_to = "dep_values") %>% 
  pivot_longer(cols = all_of(expl_vars),
               names_to= "expl_names", values_to = "expl_values") %>% 
  mutate(across(where(is.character), as.factor),
         dep_names = fct_relevel(dep_names, "DOC_mgL", "number.of.peaks",
                                 "AI_Mod.mean", 
                                 "total.transformations", "normalized.transformations"),
         expl_names = fct_relevel(expl_names, "WsAreaSqKm", "tt_hr", 
                                  "lulc_evenness", "PctConif2019Ws", "PctDecid2019Ws"))

#pivot longer all dependent variables
dat4 <- dat2 %>% 
  pivot_longer(cols = all_of(dep_vars),
               names_to= "dep_names", values_to = "dep_values") %>% 
  mutate(across(where(is.character), as.factor),
         dep_names = fct_relevel(dep_names, "DOC_mgL", "number.of.peaks",
                                 "AI_Mod.mean", 
                                 "total.transformations", "normalized.transformations"),
         Watershed = fct_relevel(Watershed, "Yakima", "Deschutes", 
                                 "Willamette", "Gunnison", "Connecticut"))


#3.0 Preliminary Plots ----
theme_set(theme_bw()+
            theme(strip.background = element_rect(color="black", fill="white")))

ggplot(dat) +
  geom_point(mapping=aes(x=lulc_evenness, y= normalized.transformations,
                         color = Watershed))
ggplot(dat) +
  geom_point(mapping=aes(x= AI_Mod.mean, y= NOSC.mean,
                         color = Watershed)) 
ggplot(dat) +
  geom_point(mapping=aes(x= OtoC.mean, y= HtoC.mean,
                         color = streamorde)) 
ggplot(dat) +
  geom_point(mapping=aes(x= q_daily_mm, y= 1/tt_hr,
                         color = streamorde)) 

##3.1 Plot of all explanatory vs dependent variables, all samples combined ----

dat3 %>% ggplot() +
  geom_jitter(mapping= aes(x= expl_values, y= dep_values), width = 0.02) + 
  geom_smooth(mapping= aes(x= expl_values, y= dep_values),
              method = "lm", se=TRUE) +
  # stat_cor(mapping= aes(x= expl_values, y= dep_values), method= "pearson",
  #          label.x.npc = "middle", label.y.npc = "bottom",
  #          size = 2) +
  scale_x_log10() +
  facet_grid(rows = vars(dep_names), cols = vars(expl_names), scales = "free") +
  labs(x = "Explanatory Variables", y = "Dependent Variables") 
  
ggsave(paste0(here, "/figs/f1_DepVarsVsExplVars_AllSamples.png"),
       width = 11, height = 8)

#4.0 Plot Individual Explanatory Variables vs dependent variables, facet by watershed ----
#Note using stat_cor with scale_x_log10 calculates the correlation on the log transform, not on the untransformed underlying data 


##4.1 Dependent vars vs Watershed Area----
dat4 %>% ggplot() +
  geom_jitter(mapping= aes(x= WsAreaSqKm, y= dep_values), width = 0.02) + 
  geom_smooth(mapping= aes(x= WsAreaSqKm, y= dep_values),
              method = "lm", se=TRUE) +
  stat_cor(mapping= aes(x= WsAreaSqKm, y= dep_values), method= "pearson",
           label.x.npc = "middle", label.y.npc = "bottom",
           size = 2) +
  # coord_trans(x = 'log10') +
  scale_x_log10() +
  facet_grid(rows = vars(dep_names), cols = vars(Watershed), scales = "free") +
  labs(x = "Watershed Area (square km)", y = "Dependent Variables")

ggsave(paste0(here, "/figs/f2_DepVarsVsWsArea_ByWatershed.png"),
       width = 11, height = 8)

##4.2 Dependent vars vs Transit Time ----
dat4 %>% ggplot() +
  geom_jitter(mapping= aes(x= tt_hr, y= dep_values), width = 0.02) + 
  geom_smooth(mapping= aes(x= tt_hr, y= dep_values),
              method = "lm", se=TRUE) +
  stat_cor(mapping= aes(x= tt_hr, y= dep_values), method= "pearson",
           label.x.npc = "middle", label.y.npc = "bottom",
           size = 2) +
  # coord_trans(x = 'log10') +
  scale_x_log10() +
  facet_grid(rows = vars(dep_names), cols = vars(Watershed), scales = "free") +
  labs(x = "Transit Time (hours)", y = "Dependent Variables")

ggsave(paste0(here, "/figs/f3_DepVarsVsTransTime_ByWatershed.png"),
       width = 11, height = 8)

##4.3 Dependent vars vs LULC evenness ----
dat4 %>% ggplot() +
  geom_jitter(mapping= aes(x= lulc_evenness, y= dep_values), width = 0.01) + 
  geom_smooth(mapping= aes(x= lulc_evenness, y= dep_values),
              method = "lm", se=TRUE) +
  stat_cor(mapping= aes(x= lulc_evenness, y= dep_values), method= "pearson",
           label.x.npc = "middle", label.y.npc = "bottom",
           size = 2) +
  # coord_trans(x = 'log10') +
  # scale_x_log10() +
  facet_grid(rows = vars(dep_names), cols = vars(Watershed), scales = "free") +
  labs(x = "LULC Evenness", y = "Dependent Variables")

ggsave(paste0(here, "/figs/f4_DepVarsVsLulcEvenness_ByWatershed.png"),
       width = 11, height = 8)

##4.4 Dependent vars vs % Coniferous ----
dat4 %>% ggplot() +
  geom_jitter(mapping= aes(x= PctConif2019Ws, y= dep_values), width = 0.02) + 
  geom_smooth(mapping= aes(x= PctConif2019Ws, y= dep_values),
              method = "lm", se=TRUE) +
  stat_cor(mapping= aes(x= PctConif2019Ws, y= dep_values), method= "pearson",
           label.x.npc = "middle", label.y.npc = "bottom",
           size = 2) +
  # coord_trans(x = 'log10') +
  # scale_x_log10() +
  facet_grid(rows = vars(dep_names), cols = vars(Watershed), scales = "free") +
  labs(x = "Percent Coniferous (%)", y = "Dependent Variables")

ggsave(paste0(here, "/figs/f5_DepVarsVsPctConif_ByWatershed.png"),
       width = 11, height = 8)

##4.5 Dependent vars vs % Deciduous ----
#Exclude watersheds with mostly <1% deciduous cover
dat4 %>% 
  filter(Watershed %in% c("Gunnison", "Connecticut")) %>% 
  ggplot() +
  geom_jitter(mapping= aes(x= PctDecid2019Ws, y= dep_values), width = 0.02) + 
  geom_smooth(mapping= aes(x= PctDecid2019Ws, y= dep_values),
              method = "lm", se=TRUE) +
  stat_cor(mapping= aes(x= PctDecid2019Ws, y= dep_values), method= "pearson",
           label.x.npc = "middle", label.y.npc = "bottom",
           size = 2) +
  # coord_trans(x = 'log10') +
  # scale_x_log10() +
  facet_grid(rows = vars(dep_names), cols = vars(Watershed), scales = "free") +
  labs(x = "Percent Deciduous (%)", y = "Dependent Variables")

ggsave(paste0(here, "/figs/f6_DepVarsVsPctDecid_ByWatershed.png"),
       width = 8, height = 10)


#5.0 Supplementary Figures ----
##5.1 Box plot of dependent variables by watershed ----
dat4 %>% 
  ggplot() +
  geom_jitter(mapping= aes(x= Watershed, y = dep_values), color = "dark grey", alpha=0.9) + 
  geom_boxplot(mapping= aes(x= Watershed, y = dep_values), alpha=0, size=1) + 
  facet_grid(rows = vars(dep_names), scales = "free") +
  labs(x = "Watershed", y = "Dependent Variables")

ggsave(paste0(here, "/figs/fs1_DepVarsBoxplot_ByWatershed.png"),
       width = 8, height = 8)

##5.2 Box plot of TN by explanatory variables ----
dat2 %>% 
  pivot_longer(cols = all_of(expl_vars),
               names_to= "expl_names", values_to = "expl_values") %>% 
  mutate(across(where(is.character), as.factor),
         expl_names = fct_relevel(expl_names, "WsAreaSqKm", "tt_hr", 
                                  "lulc_evenness", "PctConif2019Ws", "PctDecid2019Ws"),
         Watershed = fct_relevel(Watershed, "Yakima", "Deschutes", 
                                 "Willamette", "Gunnison", "Connecticut")) %>% 
  ggplot() +
  geom_jitter(mapping= aes(x= expl_values, y = TN_mgL)) + 
  geom_smooth(mapping= aes(x= expl_values, y= TN_mgL),
              method = "lm", se=TRUE) +
  stat_cor(mapping= aes(x= expl_values, y= TN_mgL), method= "pearson",
           label.x.npc = "middle", label.y.npc = "bottom",
           size = 2) +
  scale_x_log10() +
  facet_grid(rows = vars(Watershed),
             cols = vars(expl_names),
             scales = "free") +
  labs(x = "Explanatory Variables", y = "TN (mg/L)")

ggsave(paste0(here, "/figs/fs2_TNvsExplVars_ByWatershed.png"),
       width = 11, height = 8)

##5.3 Box plot of Specific Discharge by explanatory variables ----
dat4 %>% ggplot() +
  geom_jitter(mapping= aes(x= q_daily_mm, y= dep_values), width = 0.02) + 
  geom_smooth(mapping= aes(x= q_daily_mm, y= dep_values),
              method = "lm", se=TRUE) +
  stat_cor(mapping= aes(x= q_daily_mm, y= dep_values), method= "pearson",
           label.x.npc = "middle", label.y.npc = "bottom",
           size = 2) +
  scale_x_log10() +
  facet_grid(rows = vars(dep_names), cols = vars(Watershed), scales = "free") +
  labs(x = "Specific Discharge (mm)", y = "Dependent Variables")

ggsave(paste0(here, "/figs/fs3_DepVarsVsQ_ByWatershed.png"),
       width = 11, height = 8)


##5.4 Box plot of dependent variables by season ----
dat4 %>% 
  ggplot() +
  geom_jitter(mapping= aes(x= season_tb, y = dep_values), color = "dark grey", alpha=0.9) + 
  geom_boxplot(mapping= aes(x= season_tb, y = dep_values), alpha=0, size=1) + 
  facet_grid(rows = vars(dep_names), scales = "free") +
  labs(x = "Season", y = "Dependent Variables")

ggsave(paste0(here, "/figs/fs4_DepVarsBoxplot_BySeason.png"),
       width = 8, height = 8)



#clean environment
to_remove <- ls() %>% as_tibble() %>%
  filter(str_detect(value, "here", negate = TRUE)) %>%
  pull()
rm(list = to_remove)

