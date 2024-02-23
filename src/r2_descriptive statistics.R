#0.0 Setup----
library(here)
here <- here()
library(tidyverse)
# library(ggpubr)
# library(ggpmisc)
library(naniar)
library(rstatix)

#test
#clean environment
to_remove <- ls() %>% as_tibble() %>%
  filter(str_detect(value, "here", negate = TRUE)) %>%
  pull()
rm(list = to_remove)

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

expl_vars <- dat2 %>% 
  select(WsAreaSqKm, tt_hr, Da, lulc_evenness, PctDecid2019Ws, PctConif2019Ws) %>% 
  colnames()
dep_vars <- dat2 %>% 
  select(DOC_mgL, number.of.peaks, peaks.with.formula,
         AI_Mod.mean, CHON_norm, CHON,
         # total.transformations, 
         normalized.transformations) %>% 
  colnames()



#2.0 stats by site -----
##2.1 table of number of samples per site/variable -----
dat_site_n <- dat2 %>% 
  select(Watershed, Site, all_of(expl_vars), all_of(dep_vars)) %>% 
  group_by(Watershed, Site) %>%  
  get_summary_stats(type = "common", show = "n") %>%
  pivot_wider(names_from = "variable", values_from = n,
              names_sort = TRUE)
write_csv(dat_site_n, paste0(here, "/figs/tables/A_tab_site_n.csv"))

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
write_csv(dat_site_ds2, paste0(here, "/figs/tables/B_tab_site_ds.csv"))

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

write_csv(dat_watershed_ds2, paste0(here, "/figs/tables/C_tab_watershed_ds.csv"))

#Descriptive statistics for comparison by watershed
dat_watershed_ds3 <- dat_watershed_ds2 %>% 
  pivot_longer(-Watershed) %>%
  pivot_wider(names_from = Watershed) %>% 
  mutate(across(-name, as.numeric)) %>% 
  # select(contains("_n")) %>%
  rowwise() %>%
  mutate(min = min(across(-name)),
         max = max(across(-name)))

write_csv(dat_watershed_ds3, paste0(here, "/figs/tables/D_tab_watershed_ds2.csv"))



#clean environment
to_remove <- ls() %>% as_tibble() %>%
  filter(str_detect(value, "here", negate = TRUE)) %>%
  pull()
rm(list = to_remove)

# 
# #plots
# t <- dat_watershed_ds2 %>% 
#   select(Watershed, contains("_cv")) %>% 
#   pivot_longer(-Watershed) %>% 
#   convert_as_factor(Watershed)
# 
# ggplot(t) +
#   geom_col(mapping=aes(y=value, x = Watershed)) %>% 
#   facet_wrap(vars(name))
# 
# dat %>% 
#   ggplot() +
#   geom_point(mapping = aes(x = AI_Mod.mean, #x=DamDensWs,
#                            y= DBE.mean,
#                            color = Watershed)) +
#   facet_wrap(vars(Watershed), scales = "free")
# 
# #pivot long for DOM metrics representing richness, diversity, and composition (AI)
# #AND pivot long for all explanatory and dependent variables
# dat3 <- dat2 %>% 
#   pivot_longer(cols = all_of(dep_vars),
#                names_to= "dep_names", values_to = "dep_values") %>% 
#   pivot_longer(cols = all_of(expl_vars),
#                names_to= "expl_names", values_to = "expl_values") %>% 
#   mutate(across(where(is.character), as.factor),
#          dep_names = fct_relevel(dep_names, "DOC_mgL", "number.of.peaks",
#                                  "AI_Mod.mean", 
#                                  # "total.transformations", 
#                                  "normalized.transformations"),
#          expl_names = fct_relevel(expl_names, "WsAreaSqKm", "tt_hr", 
#                                   "lulc_evenness", "PctConif2019Ws", "PctDecid2019Ws"))
# 
# #pivot longer only all dependent variables
# dat4 <- dat2 %>% 
#   pivot_longer(cols = all_of(dep_vars),
#                names_to= "dep_names", values_to = "dep_values") %>% 
#   mutate(across(where(is.character), as.factor),
#          dep_names = fct_relevel(dep_names, "DOC_mgL", "number.of.peaks",
#                                  "AI_Mod.mean", 
#                                  # "total.transformations",
#                                  "normalized.transformations"),
#          Watershed = fct_relevel(Watershed, "Yakima", "Deschutes", 
#                                  "Willamette", "Gunnison", "Connecticut"))
# 
# #Dependent variable mean and standard deviation by site
# dat5 <- dat4 %>% 
#   group_by(Site, dep_names) %>% 
#   mutate(dep_mean = mean(dep_values, na.rm = TRUE),
#          dep_sd = sd(dep_values, na.rm=TRUE),
#          dep_n = n()) %>% 
#   ungroup() %>% 
#   distinct(Site, dep_names, .keep_all = TRUE) %>% 
#   select(-dep_values) %>% 
#   pivot_longer(cols = all_of(expl_vars),
#                names_to= "expl_names", values_to = "expl_values") %>% 
#   mutate(across(where(is.character), as.factor),
#          expl_names = fct_relevel(expl_names, "WsAreaSqKm", "tt_hr", 
#                                   "lulc_evenness", "PctConif2019Ws", "PctDecid2019Ws"))
# 
# #2.0 Plots Setup ----
# theme_set(theme_bw()+
#             theme(strip.background = element_rect(color="black", fill="white")))
# 
# labels <- c(`Yakima` = "Yakima",
#             `Deschutes` = "Deschutes", 
#             `Willamette` = "Willamette", 
#             `Gunnison` = "Gunnison", 
#             `Connecticut` = "Connecticut",
#             `WsAreaSqKm` = "Area (sq. km)",
#             `tt_hr` = "WRT (h)",
#             `lulc_evenness` = "LULC Evenness",
#             `PctConif2019Ws` = "Conif. (%)",
#             `PctDecid2019Ws` = "Decid. (%)",
#             `DOC_mgL` = "DOC (mg/L)",
#             `number.of.peaks` = "Num. Peaks",
#             `AI_Mod.mean` = "AI",
#             # `total.transformations` = "Tot. Trans.",
#             `normalized.transformations` = "Norm. Trans.")
# 
# dat2 %>% #filter(Watershed == "Willamette") %>% 
#   ggplot()+
#   geom_point(mapping=aes(x=tt_hr, y = Site))
# 
# #4.0 Correlations ----
# ## Pearson correlation, all variables, by watershed, explanatory vars log transformed ---- 
# lm_log_mods_watershed <- dat3 %>% 
#   mutate(expl_values2 = case_when(expl_names == "PctDecid2019Ws" & 
#                                     expl_values == 0 ~ 0.01,
#                                   TRUE ~ expl_values),
#          expl_values_log = log10(expl_values2)) %>% 
#   group_by(Watershed, expl_names, dep_names) %>% 
#   nest() %>% 
#   mutate(model = map(data,
#                      .f = ~lm(dep_values ~ expl_values_log,
#                               data = .)),
#          glance = map(.x = model, .f = ~broom::glance(.x)),
#          preds = map(model, broom::augment),
#          RMSE = map_dbl(preds, .f = ~sqrt(mean(.x$.resid^2))),
#          r = map_dbl(.x = data, .f = ~cor(.x$dep_values, .x$expl_values_log, use="complete.obs")),
#          p_cor = map_dbl(.x = data, .f = ~cor.test(.x$dep_values, .x$expl_values_log,
#                                                    alternative = "two.sided",
#                                                    method = "pearson")$p.value)) %>% 
#   unnest(glance)
# 
# lm_log_mods_watershed2 <- lm_log_mods_watershed %>% 
#   select(dep_names, expl_names, nobs, p.value, r.squared, adj.r.squared, RMSE, statistic, df, r) %>% 
#   mutate(p = case_when(p.value < 0.1 ~ "< 0.1",
#                        TRUE ~ "n.s.")) %>% 
#   mutate(r.sq.adj = case_when(p != "n.s." ~ 
#                                 formatC(round(adj.r.squared, 2), format = 'f',
#                                         digits = 2),
#                               TRUE ~ NA_character_),
#          r_labels = case_when(p != "n.s." ~ paste0("r~'= '*", 
#                                                    formatC(round(r, 2), format = 'f',
#                                                                       digits = 2))),
#          r2_labels = case_when(!is.na(r.sq.adj) ~ paste0("r^2~'= '*", r.sq.adj)))
# 
# write_csv(lm_log_mods_watershed2, paste0(here, "/figs/tables/lm_log_mods_watershed.csv"))
# 
# ## Pearson correlation, all variables, by watershed, explanatory vars log transformed, mean dep vars ----
# lm_log_mean_watershed <- dat5 %>% 
#   mutate(expl_values2 = case_when(expl_names == "PctDecid2019Ws" & 
#                                     expl_values == 0 ~ 0.01,
#                                   TRUE ~ expl_values),
#          expl_values_log = log10(expl_values2)) %>% 
#   group_by(Watershed, expl_names, dep_names) %>% 
#   nest() %>% 
#   mutate(model = map(data,
#                      .f = ~lm(dep_mean ~ expl_values_log,
#                               data = .)),
#          glance = map(.x = model, .f = ~broom::glance(.x)),
#          preds = map(model, broom::augment),
#          r = map_dbl(.x = data, .f = ~cor(.x$dep_mean, .x$expl_values_log, use="complete.obs")),
#          RMSE = map_dbl(preds, .f = ~sqrt(mean(.x$.resid^2))),
#          p_cor = map_dbl(.x = data, .f = ~cor.test(.x$dep_mean, .x$expl_values_log,
#                                                    alternative = "two.sided",
#                                                    method = "pearson")$p.value)) %>% 
#   unnest(glance)
# 
# lm_log_mean_watershed2 <- lm_log_mean_watershed %>% 
#   select(dep_names, expl_names, nobs, p.value, r.squared, adj.r.squared, RMSE, statistic, df, r) %>% 
#   mutate(p = case_when(p.value < 0.1 ~ "< 0.1",
#                        TRUE ~ "n.s.")) %>% 
#   mutate(r.sq.adj = case_when(p != "n.s." ~ 
#                                 formatC(round(adj.r.squared, 2), format = 'f',
#                                         digits = 2),
#                               TRUE ~ NA_character_),
#          r_labels = case_when(p != "n.s." ~ paste0("r~'= '*", 
#                                                    formatC(round(r, 2), format = 'f',
#                                                            digits = 2))),
#          r2_labels = case_when(!is.na(r.sq.adj) ~ paste0("r^2~'= '*", r.sq.adj)))
# 
# write_csv(lm_log_mean_watershed2, paste0(here, "/figs/tables/lm_log_mean_watershed.csv"))
# 
# ## Pearson correlation, all variables, by watershed ----
# lm_mods_watershed <- dat3 %>% 
#   group_by(Watershed, expl_names, dep_names) %>% 
#   nest() %>% 
#   mutate(model = map(data,
#                      .f = ~lm(dep_values ~ expl_values,
#                               data = .)),
#          glance = map(.x = model, .f = ~broom::glance(.x)),
#          preds = map(model, broom::augment),
#          r = map_dbl(.x = data, .f = ~cor(.x$dep_values, .x$expl_values, use="complete.obs")),
#          RMSE = map_dbl(preds, .f = ~sqrt(mean(.x$.resid^2)))) %>% 
#   unnest(glance)
# 
# lm_mods_watershed2 <- lm_mods_watershed %>% 
#   select(dep_names, expl_names, nobs, p.value, r.squared, adj.r.squared, RMSE, statistic, df, r) %>% 
#   mutate(p = case_when(p.value < 0.1 ~ "< 0.1",
#                        TRUE ~ "n.s.")) %>% 
#   mutate(r.sq.adj = case_when(p != "n.s." ~ 
#                                 formatC(round(adj.r.squared, 2), format = 'f',
#                                         digits = 2),
#                               TRUE ~ NA_character_),
#          r_labels = case_when(p != "n.s." ~ paste0("r~'= '*", 
#                                                    formatC(round(r, 2), format = 'f',
#                                                            digits = 2))),
#          r2_labels = case_when(!is.na(r.sq.adj) ~ paste0("r^2~'= '*", r.sq.adj)))
# 
# write_csv(lm_mods_watershed2, paste0(here, "/figs/tables/lm_mods_watershed.csv"))
# 
# ## Pearson correlation, all variables, by watershed, mean dep vars ----
# lm_mods_mean_watershed <- dat5 %>% 
#   group_by(Watershed, expl_names, dep_names) %>% 
#   nest() %>% 
#   mutate(model = map(data,
#                      .f = ~lm(dep_mean ~ expl_values,
#                               data = .)),
#          glance = map(.x = model, .f = ~broom::glance(.x)),
#          preds = map(model, broom::augment),
#          r = map_dbl(.x = data, .f = ~cor(.x$dep_mean, .x$expl_values, use="complete.obs")),
#          RMSE = map_dbl(preds, .f = ~sqrt(mean(.x$.resid^2)))) %>% 
#   unnest(glance)
# 
# lm_mods_mean_watershed2 <- lm_mods_mean_watershed %>% 
#   select(dep_names, expl_names, nobs, p.value, r.squared, adj.r.squared, RMSE, statistic, df, r) %>% 
#   mutate(p = case_when(p.value < 0.1 ~ "< 0.1",
#                        TRUE ~ "n.s.")) %>% 
#   mutate(r.sq.adj = case_when(p != "n.s." ~ 
#                                 formatC(round(adj.r.squared, 2), format = 'f',
#                                         digits = 2),
#                               TRUE ~ NA_character_),
#          r_labels = case_when(p != "n.s." ~ paste0("r~'= '*", 
#                                                    formatC(round(r, 2), format = 'f',
#                                                            digits = 2))),
#          r2_labels = case_when(!is.na(r.sq.adj) ~ paste0("r^2~'= '*", r.sq.adj)))
# 
# write_csv(lm_mods_mean_watershed2, paste0(here, "/figs/tables/lm_mods_mean_watershed.csv"))
# 
# 
# #5.0 Plots----
# ##5.1a Mean dependent vars vs Watershed Area----
# #plotting averaged data by site
# stats <- lm_log_mean_watershed2 %>% 
#   filter(expl_names == "WsAreaSqKm")
# 
# label_coord <- dat5 %>% 
#   group_by(dep_names, expl_names) %>% 
#   mutate(x_pos = max(expl_values, na.rm = T)*0.01,
#          y_pos = max(dep_mean, na.rm = T)*1.2) %>% 
#   ungroup() %>% 
#   distinct(Watershed, dep_names, expl_names, .keep_all = TRUE) %>% 
#   filter(expl_names == "WsAreaSqKm") %>% 
#   select(Watershed, dep_names, x_pos, y_pos)
# 
# stats <- stats %>% 
#   left_join(., label_coord, by = c("Watershed", "dep_names"))
# 
# dat5 %>% 
#   pivot_wider(names_from = expl_names, values_from = expl_values) %>% 
#   ggplot() +
#   geom_jitter(data = dat4, mapping= aes(x= WsAreaSqKm, y = dep_values),
#               width = 0.02, shape = 1, color = "grey") + 
#   geom_point(mapping= aes(x= WsAreaSqKm, y = dep_mean)) + 
#   geom_errorbar(mapping= aes(x= WsAreaSqKm, ymin = dep_mean-dep_sd, ymax= dep_mean+dep_sd)) +
#   geom_smooth(mapping= aes(x= WsAreaSqKm, y = dep_mean), method = "lm", se=TRUE, fill = "light blue") +
#   scale_x_log10(limits = c(1, 10000),
#                 breaks = c(1, 10, 100, 1000, 10000),
#                 labels = scales::trans_format("log10", scales::math_format(10^.x))) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
#   labs(x = bquote("Watershed Area ("*km^2*")"), y = NULL) +
#   geom_text(data = stats, 
#             mapping = aes(x = x_pos, y = y_pos, label = r_labels),
#             parse = TRUE,
#             size = 4) +
#   facet_grid(rows = vars(dep_names), cols = vars(Watershed),
#              labeller = as_labeller(labels), scales = "free",
#              switch = "y") +
#   theme(axis.text.x = element_text(size= 9),
#         strip.placement = "outside")
# 
# ggsave(paste0(here, "/figs/f1_DepVarsVsWsArea_mean.png"),
#        width = 8, height = 6)
# 
# ##5.1b Dependent vars vs Watershed Area ----
# stats <- lm_log_mods_watershed2 %>% 
#   filter(expl_names == "WsAreaSqKm")
# 
# label_coord <- dat3 %>% 
#   group_by(dep_names, expl_names) %>% 
#   mutate(x_pos = max(expl_values, na.rm = T)*0.05,
#          y_pos = max(dep_values, na.rm = T)*1.2) %>% 
#   ungroup() %>% 
#   distinct(Watershed, dep_names, expl_names, .keep_all = TRUE) %>% 
#   filter(expl_names == "WsAreaSqKm") %>% 
#   select(Watershed, dep_names, x_pos, y_pos)
# 
# stats <- stats %>% 
#   left_join(., label_coord, by = c("Watershed", "dep_names"))
# 
# dat4 %>% ggplot(mapping= aes(x= WsAreaSqKm, y= dep_values)) +
#   geom_jitter(width = 0.02) + 
#   geom_smooth(method = "lm", se=TRUE, fill = "light blue") +
#   scale_x_log10(limits = c(1, 10000),
#                 breaks = c(1, 10, 100, 1000, 10000),
#                 labels = scales::trans_format("log10", scales::math_format(10^.x))) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
#   labs(x = bquote("Watershed Area ("*km^2*")"), y = NULL) +
#   geom_text(data = stats, 
#             mapping = aes(x = x_pos, y = y_pos, label = r_labels),
#             parse = TRUE,
#             size = 4) +
#   facet_grid(rows = vars(dep_names), cols = vars(Watershed),
#              labeller = as_labeller(labels), scales = "free",
#              switch = "y") +
#   theme(axis.text.x = element_text(size= 9),
#         strip.placement = "outside")
#   
# ggsave(paste0(here, "/figs/f1_DepVarsVsWsArea.png"),
#        width = 8, height = 6)
# 
# ##5.2a Mean Dependent variable vs Water Residence Time----
# stats <- lm_log_mean_watershed2 %>% 
#   filter(expl_names == "tt_hr")
# 
# label_coord <- dat5 %>% 
#   group_by(dep_names, expl_names) %>% 
#   mutate(x_pos = max(expl_values, na.rm = T)*0.01,
#          y_pos = max(dep_mean, na.rm = T)*1.2) %>% 
#   ungroup() %>% 
#   distinct(Watershed, dep_names, expl_names, .keep_all = TRUE) %>% 
#   filter(expl_names == "tt_hr") %>% 
#   select(Watershed, dep_names, x_pos, y_pos)
# 
# stats <- stats %>% 
#   left_join(., label_coord, by = c("Watershed", "dep_names"))
# 
# dat5 %>% 
#   pivot_wider(names_from = expl_names, values_from = expl_values) %>% 
#   ggplot() +
#   geom_jitter(data = dat4, mapping= aes(x= tt_hr, y = dep_values),
#               width = 0.1, shape = 1, color = "grey") + 
#   geom_point(mapping= aes(x= tt_hr, y = dep_mean)) +
#   geom_errorbar(mapping= aes(x= tt_hr, ymin = dep_mean-dep_sd, ymax= dep_mean+dep_sd)) +
#   geom_smooth(mapping= aes(x= tt_hr, y = dep_mean), method = "lm", se=TRUE, fill = "light blue") +
#   scale_x_log10(limits = c(1, 10000),
#                 breaks = c(1, 10, 100, 1000, 10000),
#                 labels = scales::trans_format("log10", scales::math_format(10^.x))) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
#   labs(x = "Water Residence Time (h)", y = NULL) +
#   geom_text(data = stats,
#             mapping = aes(x = x_pos, y = y_pos, label = r_labels),
#             parse = TRUE,
#             size = 4) +
#   facet_grid(rows = vars(dep_names), cols = vars(Watershed),
#              labeller = as_labeller(labels), scales = "free",
#              switch= "y") +
#   theme(axis.text.x = element_text(size= 9),
#         strip.placement = "outside")
# 
# ggsave(paste0(here, "/figs/f2_DepVarsVsWRT_mean.png"),
#        width = 8, height = 6)
# 
# ##5.2b Dependent variable vs Water Residence Time ----
# stats <- lm_log_mods_watershed2 %>% 
#   filter(expl_names == "tt_hr")
# 
# label_coord <- dat3 %>% 
#   group_by(dep_names, expl_names) %>% 
#   mutate(x_pos = max(expl_values, na.rm = T)*0.05,
#          y_pos = max(dep_values, na.rm = T)*1.2) %>% 
#   ungroup() %>% 
#   distinct(Watershed, dep_names, expl_names, .keep_all = TRUE) %>% 
#   filter(expl_names == "tt_hr") %>% 
#   select(Watershed, dep_names, x_pos, y_pos)
# 
# stats <- stats %>% 
#   left_join(., label_coord, by = c("Watershed", "dep_names"))
# 
# dat4 %>% ggplot(mapping= aes(x= tt_hr, y= dep_values)) +
#   geom_jitter(width = 0.02) + 
#   geom_smooth(method = "lm", se=TRUE, fill = "light blue") +
#   scale_x_log10(limits = c(1, 10000),
#                 breaks = c(1, 10, 100, 1000, 10000),
#                 labels = scales::trans_format("log10", scales::math_format(10^.x))) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
#   labs(x = "Water Residence Time (h)", y = NULL) +
#   geom_text(data = stats, 
#             mapping = aes(x = x_pos, y = y_pos, label = r_labels),
#             parse = TRUE,
#             size = 4) +
#   facet_grid(rows = vars(dep_names), cols = vars(Watershed),
#              labeller = as_labeller(labels), scales = "free",
#              switch = "y") +
#   theme(axis.text.x = element_text(size= 9),
#         strip.placement = "outside")
# 
# ggsave(paste0(here, "/figs/f2_DepVarsVsWRT.png"),
#        width = 8, height = 6)
# 
# 
# 
# ##5.3a Dependent vars vs LULC evenness, no log transform ----
# stats <- lm_mods_watershed2 %>% 
#   filter(expl_names == "lulc_evenness")
# 
# label_coord <- dat3 %>% 
#   group_by(dep_names, expl_names) %>% 
#   mutate(x_pos = 0.25,#max(expl_values, na.rm = T)*0.9,
#          y_pos = max(dep_values, na.rm = T)*1.2) %>% 
#   ungroup() %>% 
#   distinct(Watershed, dep_names, expl_names, .keep_all = TRUE) %>% 
#   filter(expl_names == "lulc_evenness") %>% 
#   select(Watershed, dep_names, x_pos, y_pos)
# 
# stats <- stats %>% 
#   left_join(., label_coord, by = c("Watershed", "dep_names"))
# 
# dat4 %>% ggplot(mapping= aes(x= lulc_evenness, y= dep_values)) +
#   geom_jitter(width = 0.02) + 
#   geom_smooth(method = "lm", se=TRUE, fill="light blue") +
#   scale_x_continuous(limits = c(0,0.7)) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
#   labs(x = "LULC Evenness", y = NULL) +
#   geom_text(data = stats, 
#             mapping = aes(x = x_pos, y = y_pos, label = r_labels),
#             parse = TRUE,
#             size = 4) +
#   facet_grid(rows = vars(dep_names), cols = vars(Watershed),
#              labeller = as_labeller(labels), scales = "free",
#              switch = "y") +
#   theme(axis.text.x = element_text(size= 9),
#         strip.placement = "outside")
# 
# ggsave(paste0(here, "/figs/f3_DepVarsVsLulcEvenness.png"),
#        width = 8, height = 6)
# 
# ##5.3b  Mean dependent vars vs LULC evenness, no log transform ----
# stats <- lm_mods_mean_watershed2 %>% 
#   filter(expl_names == "lulc_evenness")
# 
# label_coord <- dat5 %>% 
#   group_by(dep_names, expl_names) %>% 
#   mutate(x_pos = 0.25,# max(expl_values, na.rm = T)*0.01,
#          y_pos = max(dep_mean, na.rm = T)*1.2) %>% 
#   ungroup() %>% 
#   distinct(Watershed, dep_names, expl_names, .keep_all = TRUE) %>% 
#   filter(expl_names == "lulc_evenness") %>% 
#   select(Watershed, dep_names, x_pos, y_pos)
# 
# stats <- stats %>% 
#   left_join(., label_coord, by = c("Watershed", "dep_names"))
# 
# dat5 %>% 
#   pivot_wider(names_from = expl_names, values_from = expl_values) %>% 
#   ggplot() +
#   geom_jitter(data = dat4, mapping= aes(x= lulc_evenness, y = dep_values),
#               width = 0.02, shape = 1, color = "grey") + 
#   geom_point(mapping= aes(x= lulc_evenness, y = dep_mean)) + 
#   geom_errorbar(mapping= aes(x= lulc_evenness, ymin = dep_mean-dep_sd, ymax= dep_mean+dep_sd)) +
#   geom_smooth(mapping= aes(x= lulc_evenness, y = dep_mean), method = "lm", se=TRUE, fill="light blue") +
#   scale_x_continuous(limits = c(0, 0.7)) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
#   labs(x = "LULC Evenness", y = NULL) +
#   geom_text(data = stats, 
#             mapping = aes(x = x_pos, y = y_pos, label = r_labels),
#             parse = TRUE,
#             size = 4) +
#   facet_grid(rows = vars(dep_names), cols = vars(Watershed),
#              labeller = as_labeller(labels), scales = "free",
#              switch = "y") +
#   theme(axis.text.x = element_text(size= 9),
#         strip.placement = "outside")
# 
# ggsave(paste0(here, "/figs/f3_DepVarsVsLulcEvenness_mean.png"),
#        width = 8, height = 6)
# 
# 
# 
# 
# ##5.4a Dependent vars vs coniferous, no log transform ----
# stats <- lm_mods_watershed2 %>% 
#   filter(expl_names == "PctConif2019Ws")
# 
# label_coord <- dat3 %>% 
#   group_by(dep_names, expl_names) %>% 
#   mutate(x_pos = 50, #max(expl_values, na.rm = T)*0.5,
#          y_pos = max(dep_values, na.rm = T)*1.2) %>% 
#   ungroup() %>% 
#   distinct(Watershed, dep_names, expl_names, .keep_all = TRUE) %>% 
#   filter(expl_names == "PctConif2019Ws") %>% 
#   select(Watershed, dep_names, x_pos, y_pos)
# 
# stats <- stats %>% 
#   left_join(., label_coord, by = c("Watershed", "dep_names"))
# 
# dat4 %>% ggplot(mapping= aes(x= PctConif2019Ws, y= dep_values)) +
#   geom_jitter(width = 0.02) + 
#   geom_smooth(method = "lm", se=TRUE, fill="light blue") +
#   scale_x_continuous(limits = c(0,100)) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
#   labs(x = "Coniferous (%)", y = NULL) +
#   geom_text(data = stats, 
#             mapping = aes(x = x_pos, y = y_pos, label = r_labels),
#             parse = TRUE,
#             size = 4) +
#   facet_grid(rows = vars(dep_names), cols = vars(Watershed),
#              labeller = as_labeller(labels), scales = "free",
#              switch = "y") +
#   theme(axis.text.x = element_text(size= 9),
#         strip.placement = "outside")
# 
# ggsave(paste0(here, "/figs/f4_DepVarsVsPctConif.png"),
#        width = 8, height = 6)
# 
# ##5.4b  Mean dependent vars vs LULC evenness, no log transform ----
# stats <- lm_mods_mean_watershed2 %>% 
#   filter(expl_names == "PctConif2019Ws")
# 
# label_coord <- dat5 %>% 
#   group_by(dep_names, expl_names) %>% 
#   mutate(x_pos = 50,# max(expl_values, na.rm = T)*0.01,
#          y_pos = max(dep_mean, na.rm = T)*1.2) %>% 
#   ungroup() %>% 
#   distinct(Watershed, dep_names, expl_names, .keep_all = TRUE) %>% 
#   filter(expl_names == "PctConif2019Ws") %>% 
#   select(Watershed, dep_names, x_pos, y_pos)
# 
# stats <- stats %>% 
#   left_join(., label_coord, by = c("Watershed", "dep_names"))
# 
# dat5 %>% 
#   pivot_wider(names_from = expl_names, values_from = expl_values) %>% 
#   ggplot() +
#   geom_jitter(data = dat4, mapping= aes(x= PctConif2019Ws, y = dep_values),
#               width = 0.02, shape = 1, color = "grey") + 
#   geom_point(mapping= aes(x= PctConif2019Ws, y = dep_mean)) + 
#   geom_errorbar(mapping= aes(x= PctConif2019Ws, ymin = dep_mean-dep_sd, ymax= dep_mean+dep_sd)) +
#   geom_smooth(mapping= aes(x= PctConif2019Ws, y = dep_mean), method = "lm", se=TRUE, fill="light blue") +
#   scale_x_continuous(limits = c(0, 100)) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
#   labs(x = "Coniferous (%)", y = NULL) +
#   geom_text(data = stats, 
#             mapping = aes(x = x_pos, y = y_pos, label = r_labels),
#             parse = TRUE,
#             size = 4) +
#   facet_grid(rows = vars(dep_names), cols = vars(Watershed),
#              labeller = as_labeller(labels), scales = "free",
#              switch = "y") +
#   theme(axis.text.x = element_text(size= 9),
#         strip.placement = "outside")
# 
# ggsave(paste0(here, "/figs/f4_DepVarsVsPctConif_mean.png"),
#        width = 8, height = 6)
# 
# 
# 
# 
# ##5.5a Mean Dependent vars vs % Deciduous ----
# #Exclude watersheds with mostly <1% deciduous cover
# stats <- lm_mods_mean_watershed2 %>% 
#   filter(expl_names == "PctDecid2019Ws")
# 
# label_coord <- dat5 %>% 
#   group_by(dep_names, expl_names) %>% 
#   mutate(x_pos = 50,# max(expl_values, na.rm = T)*0.01,
#          y_pos = max(dep_mean, na.rm = T)*1.2) %>% 
#   ungroup() %>% 
#   distinct(Watershed, dep_names, expl_names, .keep_all = TRUE) %>% 
#   filter(expl_names == "PctDecid2019Ws") %>% 
#   select(Watershed, dep_names, x_pos, y_pos)
# 
# stats <- stats %>% 
#   left_join(., label_coord, by = c("Watershed", "dep_names")) %>% 
#   filter(Watershed %in% c("Gunnison", "Connecticut"))
# 
# dat5 %>% 
#   filter(Watershed %in% c("Gunnison", "Connecticut")) %>% 
#   pivot_wider(names_from = expl_names, values_from = expl_values) %>% 
#   ggplot() +
#   geom_jitter(data = dat4 %>% filter(Watershed %in% c("Gunnison", "Connecticut")),
#               mapping= aes(x= PctDecid2019Ws, y = dep_values),
#               width = 0.02, shape = 1, color ="grey") + 
#   geom_point(mapping= aes(x= PctDecid2019Ws, y = dep_mean)) + 
#   geom_errorbar(mapping= aes(x= PctDecid2019Ws, ymin = dep_mean-dep_sd, ymax= dep_mean+dep_sd)) +
#   geom_smooth(mapping= aes(x= PctDecid2019Ws, y = dep_mean), method = "lm", se=TRUE, fill="light blue") +
#   scale_x_continuous(limits = c(0, 100)) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
#   labs(x = "Deciduous (%)", y = NULL) +
#   geom_text(data = stats, 
#             mapping = aes(x = x_pos, y = y_pos, label = r_labels),
#             parse = TRUE,
#             size = 4) +
#   facet_grid(rows = vars(dep_names), cols = vars(Watershed),
#              labeller = as_labeller(labels), scales = "free",
#              switch = "y") +
#   theme(axis.text.x = element_text(size= 9),
#         strip.placement = "outside")
# 
# 
# ggsave(paste0(here, "/figs/f5_DepVarsVsPctDecid_mean.png"),
#        width = 8, height = 10)
# 
# ##5.5b Dependent vars vs % Deciduous----
# stats <- lm_mods_watershed2 %>% 
#   filter(expl_names == "PctDecid2019Ws")
# 
# label_coord <- dat5 %>% 
#   group_by(dep_names, expl_names) %>% 
#   mutate(x_pos = 50,# max(expl_values, na.rm = T)*0.01,
#          y_pos = max(dep_mean, na.rm = T)*1.2) %>% 
#   ungroup() %>% 
#   distinct(Watershed, dep_names, expl_names, .keep_all = TRUE) %>% 
#   filter(expl_names == "PctDecid2019Ws") %>% 
#   select(Watershed, dep_names, x_pos, y_pos)
# 
# stats <- stats %>% 
#   left_join(., label_coord, by = c("Watershed", "dep_names")) %>% 
#   filter(Watershed %in% c("Gunnison", "Connecticut"))
# 
# dat4 %>%
#   filter(Watershed %in% c("Gunnison", "Connecticut")) %>% 
#   ggplot(mapping= aes(x= PctDecid2019Ws, y= dep_values)) +
#   geom_jitter(width = 0.02) + 
#   geom_smooth(method = "lm", se=TRUE, fill="light blue") +
#   scale_x_continuous(limits = c(0,100)) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
#   labs(x = "Deciduous (%)", y = NULL) +
#   geom_text(data = stats, 
#             mapping = aes(x = x_pos, y = y_pos, label = r_labels),
#             parse = TRUE,
#             size = 4) +
#   facet_grid(rows = vars(dep_names), cols = vars(Watershed),
#              labeller = as_labeller(labels), scales = "free",
#              switch = "y") +
#   theme(axis.text.x = element_text(size= 9),
#         strip.placement = "outside")
# 
# 
# ggsave(paste0(here, "/figs/f5_DepVarsVsPctDecid.png"),
#        width = 8, height = 10)
# 
# 
# #5.0 Supplementary Figures ----
# ##5.1 Box plot of dependent variables by watershed ----
# dat4 %>% 
#   ggplot() +
#   geom_jitter(mapping= aes(x= Watershed, y = dep_values), color = "dark grey", alpha=0.9) + 
#   geom_boxplot(mapping= aes(x= Watershed, y = dep_values), alpha=0, size=1) + 
#   facet_grid(rows = vars(dep_names),
#              labeller = as_labeller(labels), scales = "free") +
#   labs(x = "Watershed", y = "Dependent Variables")
# 
# ggsave(paste0(here, "/figs/fs2_DepVarsBoxplot_ByWatershed.png"),
#        width = 8, height = 8)
# 
# ##5.2 Box plot of TN by explanatory variables ----
# dat2 %>% 
#   pivot_longer(cols = all_of(expl_vars),
#                names_to= "expl_names", values_to = "expl_values") %>% 
#   mutate(across(where(is.character), as.factor),
#          expl_names = fct_relevel(expl_names, "WsAreaSqKm", "tt_hr", 
#                                   "lulc_evenness", "PctConif2019Ws", "PctDecid2019Ws"),
#          Watershed = fct_relevel(Watershed, "Yakima", "Deschutes", 
#                                  "Willamette", "Gunnison", "Connecticut")) %>% 
#   ggplot(mapping= aes(x= expl_values, y = TN_mgL)) +
#   geom_jitter() + 
#   geom_smooth(method = "lm", se=TRUE) +
#   stat_correlation(use_label(c("rr", "p")),
#                    small.r = TRUE, small.p = TRUE) +
#   scale_x_log10() +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
#   facet_grid(rows = vars(Watershed),
#              cols = vars(expl_names),
#              labeller = as_labeller(labels),
#              scales = "free") +
#   labs(x = "Explanatory Variables", y = "TN (mg/L)")
# 
# ggsave(paste0(here, "/figs/fs3_TNvsExplVars_ByWatershed.png"),
#        width = 11, height = 8)
# 
# ##5.3 Box plot of Specific Discharge by explanatory variables ----
# dat4 %>% ggplot(mapping= aes(x= q_daily_mm, y= dep_values)) +
#   geom_jitter(width = 0.02) + 
#   geom_smooth(method = "lm", se=TRUE) +
#   stat_correlation(use_label(c("rr", "p")),
#                    small.r = TRUE, small.p = TRUE) +
#   scale_x_log10() +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
#   facet_grid(rows = vars(dep_names), cols = vars(Watershed),
#              labeller = as_labeller(labels), scales = "free") +
#   labs(x = "Specific Discharge (mm)", y = "Dependent Variables")
# 
# ggsave(paste0(here, "/figs/fs4_DepVarsVsQ_ByWatershed.png"),
#        width = 11, height = 8)
# 
# 
# ##5.4 Box plot of dependent variables by season ----
# dat4 %>% 
#   ggplot() +
#   geom_jitter(mapping= aes(x= season_tb, y = dep_values), color = "dark grey", alpha=0.9) + 
#   geom_boxplot(mapping= aes(x= season_tb, y = dep_values), alpha=0, size=1) + 
#   facet_grid(rows = vars(dep_names),
#              labeller = as_labeller(labels), scales = "free") +
#   labs(x = "Season", y = "Dependent Variables")
# 
# ggsave(paste0(here, "/figs/fs5_DepVarsBoxplot_BySeason.png"),
#        width = 8, height = 8)
# 
# 
# 

# #clean environment
# to_remove <- ls() %>% as_tibble() %>%
#   filter(str_detect(value, "here", negate = TRUE)) %>%
#   pull()
# rm(list = to_remove)


#Other----
#6.0 Correlation Tables ----
# ##6.1 Linear correlation with all variables, Watersheds combined ----
# lm_mods_all <- dat3 %>%
#   group_by(expl_names, dep_names) %>% 
#   nest() %>% 
#   mutate(model = map(data,
#                      .f = ~lm(dep_values ~ expl_values,
#                               data = .)),
#          glance = map(.x = model, .f = ~broom::glance(.x)),
#          preds = map(model, broom::augment),
#          RMSE = map_dbl(preds, .f = ~sqrt(mean(.x$.resid^2)))) %>% 
#   unnest(glance)
# 
# lm_mods_all2 <- lm_mods_all %>% 
#   select(dep_names, expl_names, nobs, p.value, adj.r.squared, RMSE, statistic, df) %>% 
#   mutate(p = case_when(p.value < 0.05 ~ "< 0.05",
#                        TRUE ~ "n.s"),
#          r.sq.adj = format(round(adj.r.squared, 4), scientific = F))
# 
# write_csv(lm_mods_all2, paste0(here, "/figs/tables/lm_mods_all.csv"))
# 
# ##6.2 Linear correlation with all variables, Watersheds combined log transformed ----
# lm_log_mods_all <- dat3 %>% 
#   mutate(expl_values2 = case_when(expl_names == "PctDecid2019Ws" & 
#                                     expl_values == 0 ~ 0.01,
#                                   TRUE ~ expl_values),
#          expl_values_log = log10(expl_values2)) %>% 
#   group_by(expl_names, dep_names) %>% 
#   nest() %>% 
#   mutate(model = map(data,
#                      .f = ~lm(dep_values ~ expl_values_log,
#                               data = .)),
#          glance = map(.x = model, .f = ~broom::glance(.x)),
#          preds = map(model, broom::augment),
#          RMSE = map_dbl(preds, .f = ~sqrt(mean(.x$.resid^2)))) %>% 
#   unnest(glance)
# 
# lm_log_mods_all2 <- lm_log_mods_all %>% 
#   select(dep_names, expl_names, nobs, p.value, adj.r.squared, RMSE, statistic, df) %>% 
#   mutate(p = case_when(p.value < 0.05 ~ "< 0.05",
#                        TRUE ~ "n.s"),
#          r.sq.adj = format(round(adj.r.squared, 4), scientific = F))
# 
# write_csv(lm_log_mods_all2, paste0(here, "/figs/tables/lm_log_mods_all.csv"))
# 
# ##6.3 Linear correlations by Watershed ----
# lm_mods_watershed <- dat3 %>% 
#   group_by(Watershed, expl_names, dep_names) %>% 
#   nest() %>% 
#   mutate(model = map(data,
#                      .f = ~lm(dep_values ~ expl_values,
#                               data = .)),
#          glance = map(.x = model, .f = ~broom::glance(.x)),
#          preds = map(model, broom::augment),
#          RMSE = map_dbl(preds, .f = ~sqrt(mean(.x$.resid^2)))) %>% 
#   unnest(glance)
# 
# lm_mods_watershed2 <- lm_mods_watershed %>% 
#   select(dep_names, expl_names, nobs, p.value, r.squared, adj.r.squared, RMSE, statistic, df) %>% 
#   mutate(p = case_when(p.value < 0.05 ~ "< 0.05",
#                        TRUE ~ "n.s"),
#          r.sq.adj = format(round(adj.r.squared, 4), scientific = F))
# 
# write_csv(lm_mods_watershed2, paste0(here, "/figs/tables/lm_mods_watershed.csv"))
# 
# ##6.4 Linear correlation with all variables, by watershed,log transformed ----
# lm_log_mods_watershed <- dat3 %>% 
#   mutate(expl_values2 = case_when(expl_names == "PctDecid2019Ws" & 
#                                     expl_values == 0 ~ 0.01,
#                                   TRUE ~ expl_values),
#          expl_values_log = log10(expl_values2)) %>% 
#   group_by(Watershed, expl_names, dep_names) %>% 
#   nest() %>% 
#   mutate(model = map(data,
#                      .f = ~lm(dep_values ~ expl_values_log,
#                               data = .)),
#          glance = map(.x = model, .f = ~broom::glance(.x)),
#          preds = map(model, broom::augment),
#          RMSE = map_dbl(preds, .f = ~sqrt(mean(.x$.resid^2)))) %>% 
#   unnest(glance)
# 
# lm_log_mods_watershed2 <- lm_log_mods_watershed %>% 
#   select(dep_names, expl_names, nobs, p.value, r.squared, adj.r.squared, RMSE, statistic, df) %>% 
#   mutate(p = case_when(p.value < 0.05 ~ "< 0.05",
#                        TRUE ~ "n.s.")) %>% 
#   mutate(r.sq.adj = case_when(p != "n.s." ~ 
#                                 formatC(round(adj.r.squared, 2), format = 'f',
#                                         digits = 2),
#                               TRUE ~ NA_character_),
#          r2_labels = case_when(!is.na(r.sq.adj) ~ paste0("r^2~'= '*", r.sq.adj)))
# 
# write_csv(lm_log_mods_watershed2, paste0(here, "/figs/tables/lm_log_mods_watershed.csv"))
# 
# ## Plot of all explanatory vs dependent variables, all samples combined ----
# 
# # dat3 %>% ggplot(mapping= aes(x= expl_values, y= dep_values)) +
# #   geom_jitter(width = 0.02) + 
# #   geom_smooth(method = "lm", se=TRUE) +
# #   scale_x_log10() +
# #   facet_grid(rows = vars(dep_names), cols = vars(expl_names),
# #              labeller = as_labeller(labels),
# #              scales = "free") +
# #   labs(x = "Explanatory Variables", y = "Dependent Variables") 
# #   
# # ggsave(paste0(here, "/figs/fs1_DepVarsVsExplVars_AllSamples.png"),
# #        width = 11, height = 8)
# 
# 
# # ggplot(dat) +
# #   geom_point(mapping=aes(x=lulc_evenness, y= normalized.transformations,
# #                          color = Watershed))
# # ggplot(dat) +
# #   geom_point(mapping=aes(x= AI_Mod.mean, y= NOSC.mean,
# #                          color = Watershed)) 
# # ggplot(dat) +
# #   geom_point(mapping=aes(x= OtoC.mean, y= HtoC.mean,
# #                          color = streamorde)) 
# # ggplot(dat) +
# #   geom_point(mapping=aes(x= q_daily_mm, y= 1/tt_hr,
# #                          color = streamorde)) 

