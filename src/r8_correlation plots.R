#0.0 Setup----
library(here)
here <- here()
library(tidyverse)
# library(ggpubr)
library(ggpmisc)
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

#pivot longer only all dependent variables
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

# label_expl <- as_labeller(c(`WsAreaSqKm` = bquote("WA"~Km^2)))
# labels <- c(`WsAreaSqKm` = `bquote("WA"~Km^2)`)
labels <- c(`Yakima` = "Yakima",
            `Deschutes` = "Deschutes", 
            `Willamette` = "Willamette", 
            `Gunnison` = "Gunnison", 
            `Connecticut` = "Connecticut",
            `WsAreaSqKm` = "Area (sq. km)",
            `tt_hr` = "WRT (h)",
            `lulc_evenness` = "LULC Evenness",
            `PctConif2019Ws` = "Conif. (%)",
            `PctDecid2019Ws` = "Decid. (%)",
            `DOC_mgL` = "DOC (mg/L)",
            `number.of.peaks` = "Num. Peaks",
            `AI_Mod.mean` = "AI",
            `total.transformations` = "Tot. Trans.",
            `normalized.transformations` = "Norm. Trans.")

# ggplot(dat) +
#   geom_point(mapping=aes(x=lulc_evenness, y= normalized.transformations,
#                          color = Watershed))
# ggplot(dat) +
#   geom_point(mapping=aes(x= AI_Mod.mean, y= NOSC.mean,
#                          color = Watershed)) 
# ggplot(dat) +
#   geom_point(mapping=aes(x= OtoC.mean, y= HtoC.mean,
#                          color = streamorde)) 
# ggplot(dat) +
#   geom_point(mapping=aes(x= q_daily_mm, y= 1/tt_hr,
#                          color = streamorde)) 

##3.1 Plot of all explanatory vs dependent variables, all samples combined ----

dat3 %>% ggplot(mapping= aes(x= expl_values, y= dep_values)) +
  geom_jitter(width = 0.02) + 
  geom_smooth(method = "lm", se=TRUE) +
  scale_x_log10() +
  facet_grid(rows = vars(dep_names), cols = vars(expl_names),
             labeller = as_labeller(labels),
             scales = "free") +
  labs(x = "Explanatory Variables", y = "Dependent Variables") 
  
ggsave(paste0(here, "/figs/fs1_DepVarsVsExplVars_AllSamples.png"),
       width = 11, height = 8)

#4.0 Plot Individual Explanatory Variables vs dependent variables, facet by watershed ----
#Note using stat_cor with scale_x_log10 calculates the correlation on the log transform, not on the untransformed underlying data 


##4.1 Dependent vars vs Watershed Area----
dat4 %>% ggplot(mapping= aes(x= WsAreaSqKm, y= dep_values)) +
  geom_jitter(width = 0.02) + 
  geom_smooth(method = "lm", se=TRUE) +
  scale_x_log10() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  facet_grid(rows = vars(dep_names), cols = vars(Watershed),
             labeller = as_labeller(labels), scales = "free") +
  labs(x = bquote("Watershed Area ("*km^2*")"), y = "Dependent Variables") +
  stat_correlation(use_label(c("rr", "p")),
                   small.r = TRUE, small.p = TRUE) 
  
ggsave(paste0(here, "/figs/f1_DepVarsVsWsArea_ByWatershed.png"),
       width = 11, height = 8)

##4.2 Dependent vars vs Transit Time ----
dat4 %>% ggplot(mapping= aes(x= tt_hr, y= dep_values)) +
  geom_jitter(width = 0.01) + 
  geom_smooth(method = "lm", se=TRUE) +
  scale_x_log10() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  facet_grid(rows = vars(dep_names), cols = vars(Watershed),
             labeller = as_labeller(labels), scales = "free") +
  labs(x = "Water Residence Time (h)", y = "Dependent Variables") +
  stat_correlation(use_label(c("rr", "p")),
                   small.r = TRUE, small.p = TRUE) 

ggsave(paste0(here, "/figs/f2_DepVarsVsWRT_ByWatershed.png"),
       width = 11, height = 8)

##4.3 Dependent vars vs LULC evenness, no log transform ----
dat4 %>% ggplot(mapping= aes(x= lulc_evenness, y= dep_values)) +
  geom_jitter(width = 0.01) + 
  geom_smooth(method = "lm", se=TRUE) +
  # scale_x_log10() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  facet_grid(rows = vars(dep_names), cols = vars(Watershed),
             labeller = as_labeller(labels), scales = "free") +
  labs(x = "LULC Evenness", y = "Dependent Variables") +
  stat_correlation(use_label(c("rr", "p")),
                   small.r = TRUE, small.p = TRUE)

ggsave(paste0(here, "/figs/f3_DepVarsVsLulcEvenness_ByWatershed.png"),
       width = 11, height = 8)

##4.4 Dependent vars vs % Coniferous ----
dat4 %>% ggplot(mapping= aes(x= PctConif2019Ws, y= dep_values)) +
  geom_jitter(width = 0.02) + 
  geom_smooth(method = "lm", se=TRUE) +
  # scale_x_log10() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  facet_grid(rows = vars(dep_names), cols = vars(Watershed),
             labeller = as_labeller(labels), scales = "free") +
  labs(x = "Coniferous (%)", y = "Dependent Variables") +
  stat_correlation(use_label(c("rr", "p")),
                   small.r = TRUE, small.p = TRUE)

ggsave(paste0(here, "/figs/f4_DepVarsVsPctConif_ByWatershed.png"),
       width = 11, height = 8)

##4.5 Dependent vars vs % Deciduous ----
#Exclude watersheds with mostly <1% deciduous cover
dat4 %>% 
  filter(Watershed %in% c("Gunnison", "Connecticut")) %>%
  ggplot(mapping= aes(x= PctDecid2019Ws, y= dep_values)) +
  geom_jitter(width = 0.02) + 
  geom_smooth(method = "lm", se=TRUE) +
  # scale_x_log10() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  facet_grid(rows = vars(dep_names), cols = vars(Watershed),
             labeller = as_labeller(labels), scales = "free") +
  labs(x = "Deciduous (%)", y = "Dependent Variables") +
  stat_correlation(use_label(c("rr", "p")),
                   small.r = TRUE, small.p = TRUE)

ggsave(paste0(here, "/figs/f5_DepVarsVsPctDecid_ByWatershed.png"),
       width = 8, height = 10)


#5.0 Supplementary Figures ----
##5.1 Box plot of dependent variables by watershed ----
dat4 %>% 
  ggplot() +
  geom_jitter(mapping= aes(x= Watershed, y = dep_values), color = "dark grey", alpha=0.9) + 
  geom_boxplot(mapping= aes(x= Watershed, y = dep_values), alpha=0, size=1) + 
  facet_grid(rows = vars(dep_names),
             labeller = as_labeller(labels), scales = "free") +
  labs(x = "Watershed", y = "Dependent Variables")

ggsave(paste0(here, "/figs/fs2_DepVarsBoxplot_ByWatershed.png"),
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
  ggplot(mapping= aes(x= expl_values, y = TN_mgL)) +
  geom_jitter() + 
  geom_smooth(method = "lm", se=TRUE) +
  stat_correlation(use_label(c("rr", "p")),
                   small.r = TRUE, small.p = TRUE) +
  scale_x_log10() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  facet_grid(rows = vars(Watershed),
             cols = vars(expl_names),
             labeller = as_labeller(labels),
             scales = "free") +
  labs(x = "Explanatory Variables", y = "TN (mg/L)")

ggsave(paste0(here, "/figs/fs3_TNvsExplVars_ByWatershed.png"),
       width = 11, height = 8)

##5.3 Box plot of Specific Discharge by explanatory variables ----
dat4 %>% ggplot(mapping= aes(x= q_daily_mm, y= dep_values)) +
  geom_jitter(width = 0.02) + 
  geom_smooth(method = "lm", se=TRUE) +
  stat_correlation(use_label(c("rr", "p")),
                   small.r = TRUE, small.p = TRUE) +
  scale_x_log10() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  facet_grid(rows = vars(dep_names), cols = vars(Watershed),
             labeller = as_labeller(labels), scales = "free") +
  labs(x = "Specific Discharge (mm)", y = "Dependent Variables")

ggsave(paste0(here, "/figs/fs4_DepVarsVsQ_ByWatershed.png"),
       width = 11, height = 8)


##5.4 Box plot of dependent variables by season ----
dat4 %>% 
  ggplot() +
  geom_jitter(mapping= aes(x= season_tb, y = dep_values), color = "dark grey", alpha=0.9) + 
  geom_boxplot(mapping= aes(x= season_tb, y = dep_values), alpha=0, size=1) + 
  facet_grid(rows = vars(dep_names),
             labeller = as_labeller(labels), scales = "free") +
  labs(x = "Season", y = "Dependent Variables")

ggsave(paste0(here, "/figs/fs5_DepVarsBoxplot_BySeason.png"),
       width = 8, height = 8)

#6.0 Correlation Tables ----
##6.1 Linear correlation with all variables, Watersheds combined ----
lm_mods_all <- dat3 %>%
  group_by(expl_names, dep_names) %>% 
  nest() %>% 
  mutate(model = map(data,
                     .f = ~lm(dep_values ~ expl_values,
                              data = .)),
         glance = map(.x = model, .f = ~broom::glance(.x)),
         preds = map(model, broom::augment),
         RMSE = map_dbl(preds, .f = ~sqrt(mean(.x$.resid^2)))) %>% 
  unnest(glance)

lm_mods_all2 <- lm_mods_all %>% 
  select(dep_names, expl_names, nobs, p.value, adj.r.squared, RMSE, statistic, df) %>% 
  mutate(p = case_when(p.value < 0.05 ~ "< 0.05",
                       TRUE ~ "n.s"),
         r.sq.adj = format(round(adj.r.squared, 4), scientific = F))

write_csv(lm_mods_all2, paste0(here, "/figs/tables/lm_mods_all.csv"))

##6.2 Linear correlation with all variables, Watersheds combined log transformed ----
lm_log_mods_all <- dat3 %>% 
  mutate(expl_values2 = case_when(expl_names == "PctDecid2019Ws" & 
                                   expl_values == 0 ~ 0.01,
                                 TRUE ~ expl_values),
         expl_values_log = log10(expl_values2)) %>% 
  group_by(expl_names, dep_names) %>% 
  nest() %>% 
  mutate(model = map(data,
                     .f = ~lm(dep_values ~ expl_values_log,
                              data = .)),
         glance = map(.x = model, .f = ~broom::glance(.x)),
         preds = map(model, broom::augment),
         RMSE = map_dbl(preds, .f = ~sqrt(mean(.x$.resid^2)))) %>% 
  unnest(glance)

lm_log_mods_all2 <- lm_log_mods_all %>% 
  select(dep_names, expl_names, nobs, p.value, adj.r.squared, RMSE, statistic, df) %>% 
  mutate(p = case_when(p.value < 0.05 ~ "< 0.05",
                       TRUE ~ "n.s"),
         r.sq.adj = format(round(adj.r.squared, 4), scientific = F))

write_csv(lm_log_mods_all2, paste0(here, "/figs/tables/lm_log_mods_all.csv"))

##6.3 Linear correlations by Watershed ----
lm_mods_watershed <- dat3 %>% 
  group_by(Watershed, expl_names, dep_names) %>% 
  nest() %>% 
  mutate(model = map(data,
                     .f = ~lm(dep_values ~ expl_values,
                              data = .)),
         glance = map(.x = model, .f = ~broom::glance(.x)),
         preds = map(model, broom::augment),
         RMSE = map_dbl(preds, .f = ~sqrt(mean(.x$.resid^2)))) %>% 
  unnest(glance)

lm_mods_watershed2 <- lm_mods_watershed %>% 
  select(dep_names, expl_names, nobs, p.value, adj.r.squared, RMSE, statistic, df) %>% 
  mutate(p = case_when(p.value < 0.05 ~ "< 0.05",
                       TRUE ~ "n.s"),
         r.sq.adj = format(round(adj.r.squared, 4), scientific = F))

write_csv(lm_mods_watershed2, paste0(here, "/figs/tables/lm_mods_watershed.csv"))

##6.4 Linear correlation with all variables, by watershed,log transformed ----
lm_log_mods_watershed <- dat3 %>% 
  mutate(expl_values2 = case_when(expl_names == "PctDecid2019Ws" & 
                                    expl_values == 0 ~ 0.01,
                                  TRUE ~ expl_values),
         expl_values_log = log10(expl_values2)) %>% 
  group_by(Watershed, expl_names, dep_names) %>% 
  nest() %>% 
  mutate(model = map(data,
                     .f = ~lm(dep_values ~ expl_values_log,
                              data = .)),
         glance = map(.x = model, .f = ~broom::glance(.x)),
         preds = map(model, broom::augment),
         RMSE = map_dbl(preds, .f = ~sqrt(mean(.x$.resid^2)))) %>% 
  unnest(glance)

lm_log_mods_watershed2 <- lm_log_mods_watershed %>% 
  select(dep_names, expl_names, nobs, p.value, adj.r.squared, RMSE, statistic, df) %>% 
  mutate(p = case_when(p.value < 0.05 ~ "< 0.05",
                       TRUE ~ "n.s"),
         r.sq.adj = format(round(adj.r.squared, 4), scientific = F))

write_csv(lm_log_mods_watershed2, paste0(here, "/figs/tables/lm_log_mods_watershed.csv"))



#clean environment
to_remove <- ls() %>% as_tibble() %>%
  filter(str_detect(value, "here", negate = TRUE)) %>%
  pull()
rm(list = to_remove)

