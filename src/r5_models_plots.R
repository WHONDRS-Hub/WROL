# Author: Kevin A. Ryan, karyan@usgs.gov
# 2024

#0.0 Setup----
library(here)
here <- here()
library(tidyverse)
# library(ggpubr)
# library(ggpmisc)
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

#Select Variables
dat2 <- dat %>% 
  select(Site, Date, Watershed, WsAreaSqKm, tt_hr, streamorde, Da,
         season_tb,
         q_daily_mm,
         PctDecid2019Ws, PctConif2019Ws,
         lulc_evenness,
         DOC_mgL, 
         TN_mgL,
         number.of.peaks, peaks.with.formula,
         total.transformations, normalized.transformations,
         AI.Mod.mean, CHON_norm)

#Select explanatory and dependent variable names
expl_vars <- dat2 %>% 
  select(WsAreaSqKm, tt_hr, Da, lulc_evenness, PctDecid2019Ws, PctConif2019Ws) %>% 
  colnames()
dep_vars <- dat2 %>% 
  select(DOC_mgL, peaks.with.formula,
         AI.Mod.mean, CHON_norm,
         normalized.transformations) %>% 
  colnames()

#pivot long for DOM metrics representing richness, diversity, and composition (AI)
#AND pivot long for all explanatory and dependent variables
dat3 <- dat2 %>% 
  pivot_longer(cols = all_of(dep_vars),
               names_to= "dep_names", values_to = "dep_values") %>% 
  pivot_longer(cols = all_of(expl_vars),
               names_to= "expl_names", values_to = "expl_values") %>% 
  mutate(across(where(is.character), as.factor),
         dep_names = fct_relevel(dep_names, "DOC_mgL", "peaks.with.formula",
                                 "AI.Mod.mean", "CHON_norm",
                                 "normalized.transformations"),
         expl_names = fct_relevel(expl_names, "WsAreaSqKm", "tt_hr", "Da",
                                  "lulc_evenness", "PctConif2019Ws", "PctDecid2019Ws"))

#pivot longer only all dependent variables
dat4 <- dat2 %>% 
  pivot_longer(cols = all_of(dep_vars),
               names_to= "dep_names", values_to = "dep_values") %>% 
  mutate(across(where(is.character), as.factor),
         dep_names = fct_relevel(dep_names, "DOC_mgL", "peaks.with.formula",
                                 "AI.Mod.mean", "CHON_norm",
                                 "normalized.transformations"),
         Watershed = fct_relevel(Watershed, "Yakima", "Deschutes", 
                                 "Willamette", "Gunnison", "Connecticut"))

#Reduce data to dependent variable mean and standard deviation by site
dat5 <- dat4 %>% 
  group_by(Site, dep_names) %>% 
  mutate(dep_mean = mean(dep_values, na.rm = TRUE),
         dep_sd = sd(dep_values, na.rm=TRUE),
         dep_n = n()) %>% 
  ungroup() %>% 
  distinct(Site, dep_names, .keep_all = TRUE) %>% 
  select(-dep_values) %>% 
  pivot_longer(cols = all_of(expl_vars),
               names_to= "expl_names", values_to = "expl_values") %>% 
  mutate(across(where(is.character), as.factor),
         expl_names = fct_relevel(expl_names, "WsAreaSqKm", "tt_hr", "Da", 
                                  "lulc_evenness", "PctConif2019Ws", "PctDecid2019Ws"))

#Reduce data to mean dependent variable and mean explanatory variables by site
dat6 <- dat3 %>% 
  group_by(Site, dep_names, expl_names) %>% 
  mutate(dep_mean = mean(dep_values, na.rm = TRUE),
         dep_sd = sd(dep_values, na.rm=TRUE),
         dep_n = n(),
         expl_mean = mean(expl_values, na.rm = TRUE),
         expl_sd = sd(expl_values, na.rm=TRUE)) %>% 
  ungroup() %>% 
  distinct(Site, dep_names, expl_names, .keep_all = TRUE) %>% 
  select(-c(dep_values, expl_values)) %>% 
  mutate(across(where(is.character), as.factor),
         expl_names = fct_relevel(expl_names, "WsAreaSqKm", "tt_hr", "Da", 
                                  "lulc_evenness", "PctConif2019Ws", "PctDecid2019Ws"),
         Watershed = fct_relevel(Watershed, "Yakima", "Deschutes", 
                                 "Willamette", "Gunnison", "Connecticut"))
glimpse(dat6)

#2.0 Plots Setup ----
theme_set(theme_bw()+
            theme(strip.background = element_rect(color="black", fill="white")))

labels <- c(`Yakima` = "Yakima",
            `Deschutes` = "Deschutes", 
            `Willamette` = "Willamette", 
            `Gunnison` = "Gunnison", 
            `Connecticut` = "Connecticut",
            `WsAreaSqKm` = "Area (sq. km)",
            `tt_hr` = "WRT (h)",
            `Da` = "Da",
            `lulc_evenness` = "LULC Evenness",
            `PctConif2019Ws` = "Conif. (%)",
            `PctDecid2019Ws` = "Decid. (%)",
            `DOC_mgL` = "DOC (mg/L)",
            `peaks.with.formula` = "Richness",
            `AI.Mod.mean` = "AI_mod",
            `CHON_norm` = "CHON (% RA)",
            `normalized.transformations` = "Norm. Trans.")

#4.0 Correlations ----
## Pearson correlation, all variables, by watershed, explanatory variables log transformed ---- 
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
         RMSE = map_dbl(preds, .f = ~sqrt(mean(.x$.resid^2))),
         r = map_dbl(.x = data, .f = ~cor(.x$dep_values, .x$expl_values_log, use="complete.obs")),
         p_cor = map_dbl(.x = data, .f = ~cor.test(.x$dep_values, .x$expl_values_log,
                                                   alternative = "two.sided",
                                                   method = "pearson")$p.value)) %>% 
  unnest(glance)

lm_log_mods_watershed2 <- lm_log_mods_watershed %>% 
  select(dep_names, expl_names, nobs, p.value, r.squared, adj.r.squared, RMSE, statistic, df, r) %>% 
  mutate(p = case_when(p.value > 0.1 ~ "p > 0.1",,
                       p.value < 0.001 ~ "p < 0.001",
                       TRUE ~ paste0("p = ", substr(as.character(p.value), 1, 5)))) %>% 
  mutate(r.sq.adj = case_when(p != "p > 0.1" ~ 
                                formatC(round(adj.r.squared, 2), format = 'f',
                                        digits = 2),
                              TRUE ~ NA_character_),
         r_labels = case_when(p != "p > 0.1" ~ paste0("r~'= '*", 
                                                   formatC(round(r, 2), format = 'f',
                                                                      digits = 2))),
         r2_labels = case_when(!is.na(r.sq.adj) ~ paste0("r^2~'= '*", r.sq.adj)))

write_csv(lm_log_mods_watershed2, paste0(here, "/output/tables/lm_log_mods_watershed.csv"))

## Pearson correlation, all variables, by watershed, explanatory vars log transformed, mean variables ----
glimpse(dat6)
lm_log_mean_watershed <- dat6 %>% 
  mutate(expl_mean2 = case_when(expl_names == "PctDecid2019Ws" & 
                                    expl_mean == 0 ~ 0.01,
                                  TRUE ~ expl_mean),
         expl_values_log = log10(expl_mean2)) %>% 
  group_by(Watershed, expl_names, dep_names) %>% 
  nest() %>% 
  mutate(model = map(data,
                     .f = ~lm(dep_mean ~ expl_values_log,
                              data = .)),
         glance = map(.x = model, .f = ~broom::glance(.x)),
         preds = map(model, broom::augment),
         r = map_dbl(.x = data, .f = ~cor(.x$dep_mean, .x$expl_values_log, use="complete.obs")),
         RMSE = map_dbl(preds, .f = ~sqrt(mean(.x$.resid^2))),
         p_cor = map_dbl(.x = data, .f = ~cor.test(.x$dep_mean, .x$expl_values_log,
                                                   alternative = "two.sided",
                                                   method = "pearson")$p.value)) %>% 
  unnest(glance)

lm_log_mean_watershed_slopes <- lm_log_mean_watershed %>% 
  mutate(tidy = map(.x = model, .f = ~broom::tidy(.x))) %>% 
  select(Watershed, dep_names, expl_names, tidy, r, RMSE, p_cor) %>% 
  unnest(tidy) %>% 
  filter(dep_names == "normalized.transformations",
         expl_names %in% c("WsAreaSqKm", "tt_hr"),
         term != "(Intercept)") %>% 
  mutate(dep_incr = estimate * log10(100),
         pc_dep_incr = (dep_incr/estimate)*100)

lm_log_mean_watershed2 <- lm_log_mean_watershed %>% 
  select(dep_names, expl_names, nobs, p.value, r.squared, adj.r.squared, RMSE, statistic, df, r) %>% 
  mutate(p = case_when(p.value > 0.1 ~ "p > 0.1",
                       p.value < 0.001 ~ "p < 0.001",
                       TRUE ~ paste0("p = ", substr(as.character(p.value), 1, 5)))) %>% 
  mutate(r.sq.adj = case_when(p != "p > 0.1" ~ 
                                formatC(round(adj.r.squared, 2), format = 'f',
                                        digits = 2),
                              TRUE ~ NA_character_),
         r_labels = case_when(p != "p > 0.1" ~ paste0("r~'= '*", 
                                                   formatC(round(r, 2), format = 'f',
                                                           digits = 2))),
         r2_labels = case_when(!is.na(r.sq.adj) ~ paste0("r^2~'= '*", r.sq.adj)))

write_csv(lm_log_mean_watershed2, paste0(here, "/output/tables/lm_log_mean_watershed.csv"))

## Pearson correlation, all variables, by watershed ----
lm_mods_watershed <- dat3 %>% 
  group_by(Watershed, expl_names, dep_names) %>% 
  nest() %>% 
  mutate(model = map(data,
                     .f = ~lm(dep_values ~ expl_values,
                              data = .)),
         glance = map(.x = model, .f = ~broom::glance(.x)),
         preds = map(model, broom::augment),
         r = map_dbl(.x = data, .f = ~cor(.x$dep_values, .x$expl_values, use="complete.obs")),
         RMSE = map_dbl(preds, .f = ~sqrt(mean(.x$.resid^2)))) %>% 
  unnest(glance)

lm_mods_watershed2 <- lm_mods_watershed %>% 
  select(dep_names, expl_names, nobs, p.value, r.squared, adj.r.squared, RMSE, statistic, df, r) %>% 
  mutate(p = case_when(p.value > 0.1 ~ "p > 0.1",
                       p.value < 0.001 ~ "p < 0.001",
                       TRUE ~ paste0("p = ", substr(as.character(p.value), 1, 5)))) %>% 
  mutate(r.sq.adj = case_when(p != "p > 0.1" ~ 
                                formatC(round(adj.r.squared, 2), format = 'f',
                                        digits = 2),
                              TRUE ~ NA_character_),
         r_labels = case_when(p != "p > 0.1" ~ paste0("r~'= '*", 
                                                   formatC(round(r, 2), format = 'f',
                                                           digits = 2))),
         r2_labels = case_when(!is.na(r.sq.adj) ~ paste0("r^2~'= '*", r.sq.adj)))

write_csv(lm_mods_watershed2, paste0(here, "/output/tables/lm_mods_watershed.csv"))

## Pearson correlation, all variables, by watershed, mean variables ----
lm_mods_mean_watershed <- dat6 %>% 
  group_by(Watershed, expl_names, dep_names) %>% 
  nest() %>% 
  mutate(model = map(data,
                     .f = ~lm(dep_mean ~ expl_mean,
                              data = .)),
         glance = map(.x = model, .f = ~broom::glance(.x)),
         preds = map(model, broom::augment),
         r = map_dbl(.x = data, .f = ~cor(.x$dep_mean, .x$expl_mean, use="complete.obs")),
         RMSE = map_dbl(preds, .f = ~sqrt(mean(.x$.resid^2)))) %>% 
  unnest(glance)

lm_mods_mean_watershed2 <- lm_mods_mean_watershed %>% 
  select(dep_names, expl_names, nobs, p.value, r.squared, adj.r.squared, RMSE, statistic, df, r) %>%
  mutate(p = case_when(p.value > 0.1 ~ "p > 0.1",
                       p.value < 0.001 ~ "p < 0.001",
                       TRUE ~ paste0("p = ", substr(as.character(p.value), 1, 5)))) %>%
  mutate(r.sq.adj = case_when(p != "p > 0.1" ~ 
                                formatC(round(adj.r.squared, 2), format = 'f',
                                        digits = 2),
                              TRUE ~ NA_character_),
         r_labels = case_when(p != "p > 0.1" ~ paste0("r~'= '*", 
                                                   formatC(round(r, 2), format = 'f',
                                                           digits = 2))),
         r2_labels = case_when(!is.na(r.sq.adj) ~ paste0("r^2~'= '*", r.sq.adj)))

write_csv(lm_mods_mean_watershed2, paste0(here, "/output/tables/lm_mods_mean_watershed.csv"))


#5.0 Plots----
##5.1 Mean dependent vars vs log Watershed Area----
#plotting averaged dependent variables by site
stats <- lm_log_mean_watershed2 %>% 
  filter(expl_names == "WsAreaSqKm")

label_coord <- dat5 %>% 
  group_by(dep_names, expl_names) %>% 
  mutate(x_pos = max(expl_values, na.rm = T)*0.001,
         y_pos = max(dep_mean, na.rm = T)*1.25,
         x_pos2 = max(expl_values, na.rm = T)*0.05,
         y_pos2 = max(dep_mean, na.rm = T)*1.25) %>% 
  ungroup() %>% 
  distinct(Watershed, dep_names, expl_names, .keep_all = TRUE) %>% 
  filter(expl_names == "WsAreaSqKm") %>% 
  select(Watershed, dep_names, x_pos, y_pos, x_pos2, y_pos2)

stats <- stats %>% 
  left_join(., label_coord, by = c("Watershed", "dep_names"))

dat5 %>% 
  pivot_wider(names_from = expl_names, values_from = expl_values) %>% 
  ggplot() +
  geom_jitter(data = dat4, mapping= aes(x= WsAreaSqKm, y = dep_values),
              width = 0.02, shape = 1, color = "grey") + 
  geom_point(mapping= aes(x= WsAreaSqKm, y = dep_mean)) + 
  geom_errorbar(mapping= aes(x= WsAreaSqKm, ymin = dep_mean-dep_sd, ymax= dep_mean+dep_sd)) +
  geom_smooth(mapping= aes(x= WsAreaSqKm, y = dep_mean), method = "lm", se=TRUE, fill = "light blue") +
  scale_x_log10(limits = c(1, 10000),
                breaks = c(1, 10, 100, 1000, 10000),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(x = bquote("Watershed Area ("*km^2*")"), y = NULL) +
  geom_text(data = stats, 
            mapping = aes(x = x_pos, y = y_pos, label = r_labels),
            parse = TRUE,
            size = 3) +
  geom_text(data = stats, 
            mapping = aes(x = x_pos2, y = y_pos2, label = p),
            parse = FALSE,
            size = 3) +
  facet_grid(rows = vars(dep_names), cols = vars(Watershed),
             labeller = as_labeller(labels), scales = "free",
             switch = "y") +
  theme(axis.text.x = element_text(size= 9),
        strip.placement = "outside")

ggsave(paste0(here, "/output/plots/f2_DepVarsVsWsArea_mean.png"),
       width = 8, height = 6)


##5.2 Mean Dependent variables vs mean log Water Residence Time----
stats <- lm_log_mean_watershed2 %>% 
  filter(expl_names == "tt_hr")

label_coord <- dat5 %>% 
  group_by(dep_names, expl_names) %>% 
  mutate(x_pos = max(expl_values, na.rm = T)*0.001,
         y_pos = max(dep_mean, na.rm = T)*1.2,
         x_pos2 = max(expl_values, na.rm = T)*0.05,
         y_pos2 = max(dep_mean, na.rm = T)*1.2) %>% 
  ungroup() %>% 
  distinct(Watershed, dep_names, expl_names, .keep_all = TRUE) %>% 
  filter(expl_names == "tt_hr") %>% 
  select(Watershed, dep_names, x_pos, y_pos, x_pos2, y_pos2)

stats <- stats %>% 
  left_join(., label_coord, by = c("Watershed", "dep_names"))

dat6 %>% 
  pivot_wider(names_from = expl_names, values_from = expl_mean) %>% 
  ggplot() +
  geom_jitter(data = dat4, mapping= aes(x= tt_hr, y = dep_values),
              width = 0.1, shape = 1, color = "grey") + 
  geom_point(mapping= aes(x= tt_hr, y = dep_mean)) +
  geom_errorbar(mapping= aes(x= tt_hr, ymin = dep_mean-dep_sd, ymax= dep_mean+dep_sd)) +
  geom_smooth(mapping= aes(x= tt_hr, y = dep_mean), method = "lm", se=TRUE, fill = "light blue") +
  scale_x_log10(limits = c(1, 10000),
                breaks = c(1, 10, 100, 1000, 10000),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(x = "Water Residence Time (h)", y = NULL) +
  geom_text(data = stats, 
            mapping = aes(x = x_pos, y = y_pos, label = r_labels),
            parse = TRUE,
            size = 3) +
  geom_text(data = stats, 
            mapping = aes(x = x_pos2, y = y_pos2, label = p),
            parse = FALSE,
            size = 3) +
  facet_grid(rows = vars(dep_names), cols = vars(Watershed),
             labeller = as_labeller(labels), scales = "free",
             switch= "y") +
  theme(axis.text.x = element_text(size= 9),
        strip.placement = "outside")

ggsave(paste0(here, "/output/plots/f3_DepVarsVsWRT_mean.png"),
       width = 8, height = 6)


##5.3 Mean Dependent variables vs mean log Damkohler Number ----
stats <- lm_log_mean_watershed2 %>% 
  filter(expl_names == "Da")

label_coord <- dat5 %>% 
  group_by(dep_names, expl_names) %>% 
  mutate(x_pos = max(expl_values, na.rm = T)*0.0008,
         y_pos = max(dep_mean, na.rm = T)*1.2,
         x_pos2 = max(expl_values, na.rm = T)*0.07,
         y_pos2 = max(dep_mean, na.rm = T)*1.2) %>% 
  ungroup() %>% 
  distinct(Watershed, dep_names, expl_names, .keep_all = TRUE) %>% 
  filter(expl_names == "Da") %>% 
  select(Watershed, dep_names, x_pos, y_pos, x_pos2, y_pos2)

stats <- stats %>% 
  left_join(., label_coord, by = c("Watershed", "dep_names"))

dat6 %>% 
  pivot_wider(names_from = expl_names, values_from = expl_mean) %>% 
  ggplot() +
  geom_jitter(data = dat4, mapping= aes(x= Da, y = dep_values),
              width = 0.1, shape = 1, color = "grey") + 
  geom_point(mapping= aes(x= Da, y = dep_mean)) +
  geom_errorbar(mapping= aes(x= Da, ymin = dep_mean-dep_sd, ymax= dep_mean+dep_sd)) +
  geom_smooth(mapping= aes(x= Da, y = dep_mean), method = "lm", se=TRUE, fill = "light blue") +
  scale_x_log10(limits = c(0.001, 100),
                breaks = c(0.001, 0.01, 0.1, 1, 10, 100),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(x = "Da (dimensionless)", y = NULL) +
  geom_text(data = stats, 
            mapping = aes(x = x_pos, y = y_pos, label = r_labels),
            parse = TRUE,
            size = 3) +
  geom_text(data = stats, 
            mapping = aes(x = x_pos2, y = y_pos2, label = p),
            parse = FALSE,
            size = 3) +
  facet_grid(rows = vars(dep_names), cols = vars(Watershed),
             labeller = as_labeller(labels), scales = "free",
             switch= "y") +
  theme(axis.text.x = element_text(size= 9),
        strip.placement = "outside")

ggsave(paste0(here, "/output/plots/f4_DepVarsVsDa_mean.png"),
       width = 8, height = 6)


##5.4  Mean dependent variables vs LULC evenness ----
stats <- lm_mods_mean_watershed2 %>% 
  filter(expl_names == "lulc_evenness")

label_coord <- dat5 %>% 
  group_by(dep_names, expl_names) %>% 
  mutate(x_pos = 0.2,# max(expl_values, na.rm = T)*0.01,
         y_pos = max(dep_mean, na.rm = T)*1.25,
         x_pos2 = max(expl_values, na.rm = T)*0.8,
         y_pos2 = max(dep_mean, na.rm = T)*1.25) %>% 
  ungroup() %>% 
  distinct(Watershed, dep_names, expl_names, .keep_all = TRUE) %>% 
  filter(expl_names == "lulc_evenness") %>% 
  select(Watershed, dep_names, x_pos, y_pos, x_pos2, y_pos2)

stats <- stats %>% 
  left_join(., label_coord, by = c("Watershed", "dep_names"))

dat5 %>% 
  pivot_wider(names_from = expl_names, values_from = expl_values) %>% 
  ggplot() +
  geom_jitter(data = dat4, mapping= aes(x= lulc_evenness, y = dep_values),
              width = 0.02, shape = 1, color = "grey") + 
  geom_point(mapping= aes(x= lulc_evenness, y = dep_mean)) + 
  geom_errorbar(mapping= aes(x= lulc_evenness, ymin = dep_mean-dep_sd, ymax= dep_mean+dep_sd)) +
  geom_smooth(mapping= aes(x= lulc_evenness, y = dep_mean), method = "lm", se=TRUE, fill="light blue") +
  scale_x_continuous(limits = c(0, 0.7)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(x = "LULC Evenness", y = NULL) +
  geom_text(data = stats, 
            mapping = aes(x = x_pos, y = y_pos, label = r_labels),
            parse = TRUE,
            size = 3) +
  geom_text(data = stats, 
            mapping = aes(x = x_pos2, y = y_pos2, label = p),
            parse = FALSE,
            size = 3) +
  facet_grid(rows = vars(dep_names), cols = vars(Watershed),
             labeller = as_labeller(labels), scales = "free",
             switch = "y") +
  theme(axis.text.x = element_text(size= 9),
        strip.placement = "outside")

ggsave(paste0(here, "/output/plots/f5_DepVarsVsLulcEvenness_mean.png"),
       width = 8, height = 6)



##5.5 Mean dependent variables vs % Coniferous cover ----
stats <- lm_mods_mean_watershed2 %>% 
  filter(expl_names == "PctConif2019Ws")

label_coord <- dat5 %>% 
  group_by(dep_names, expl_names) %>% 
  mutate(x_pos = 25,# max(expl_values, na.rm = T)*0.01,
         y_pos = max(dep_mean, na.rm = T)*1.25,
         x_pos2 = max(expl_values, na.rm = T)*0.75,
         y_pos2 = max(dep_mean, na.rm = T)*1.25) %>% 
  ungroup() %>% 
  distinct(Watershed, dep_names, expl_names, .keep_all = TRUE) %>% 
  filter(expl_names == "PctConif2019Ws") %>% 
  select(Watershed, dep_names, x_pos, y_pos, x_pos2, y_pos2)

stats <- stats %>% 
  left_join(., label_coord, by = c("Watershed", "dep_names"))

dat5 %>% 
  pivot_wider(names_from = expl_names, values_from = expl_values) %>% 
  ggplot() +
  geom_jitter(data = dat4, mapping= aes(x= PctConif2019Ws, y = dep_values),
              width = 0.02, shape = 1, color = "grey") + 
  geom_point(mapping= aes(x= PctConif2019Ws, y = dep_mean)) + 
  geom_errorbar(mapping= aes(x= PctConif2019Ws, ymin = dep_mean-dep_sd, ymax= dep_mean+dep_sd)) +
  geom_smooth(mapping= aes(x= PctConif2019Ws, y = dep_mean), method = "lm", se=TRUE, fill="light blue") +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(x = "Coniferous (%)", y = NULL) +
  geom_text(data = stats, 
            mapping = aes(x = x_pos, y = y_pos, label = r_labels),
            parse = TRUE,
            size = 3) +
  geom_text(data = stats, 
            mapping = aes(x = x_pos2, y = y_pos2, label = p),
            parse = FALSE,
            size = 3) +
  facet_grid(rows = vars(dep_names), cols = vars(Watershed),
             labeller = as_labeller(labels), scales = "free",
             switch = "y") +
  theme(axis.text.x = element_text(size= 9),
        strip.placement = "outside")

ggsave(paste0(here, "/output/plots/f6_DepVarsVsPctConif_mean.png"),
       width = 8, height = 6)


##5.6 Mean Dependent variables vs % Deciduous cover ----
#Exclude watersheds with <1% deciduous cover
stats <- lm_mods_mean_watershed2 %>% 
  filter(expl_names == "PctDecid2019Ws")

label_coord <- dat5 %>% 
  group_by(dep_names, expl_names) %>% 
  mutate(x_pos = 25,# max(expl_values, na.rm = T)*0.01,
         y_pos = max(dep_mean, na.rm = T)*1.25,
         x_pos2 = max(expl_values, na.rm = T)*0.75,
         y_pos2 = max(dep_mean, na.rm = T)*1.25) %>% 
  ungroup() %>% 
  distinct(Watershed, dep_names, expl_names, .keep_all = TRUE) %>% 
  filter(expl_names == "PctDecid2019Ws") %>% 
  select(Watershed, dep_names, x_pos, y_pos, x_pos2, y_pos2)

stats <- stats %>% 
  left_join(., label_coord, by = c("Watershed", "dep_names")) %>% 
  filter(Watershed %in% c("Gunnison", "Connecticut"))

dat5 %>% 
  filter(Watershed %in% c("Gunnison", "Connecticut")) %>% 
  pivot_wider(names_from = expl_names, values_from = expl_values) %>% 
  ggplot() +
  geom_jitter(data = dat4 %>% filter(Watershed %in% c("Gunnison", "Connecticut")),
              mapping= aes(x= PctDecid2019Ws, y = dep_values),
              width = 0.02, shape = 1, color ="grey") + 
  geom_point(mapping= aes(x= PctDecid2019Ws, y = dep_mean)) + 
  geom_errorbar(mapping= aes(x= PctDecid2019Ws, ymin = dep_mean-dep_sd, ymax= dep_mean+dep_sd)) +
  geom_smooth(mapping= aes(x= PctDecid2019Ws, y = dep_mean), method = "lm", se=TRUE, fill="light blue") +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(x = "Deciduous (%)", y = NULL) +
  geom_text(data = stats, 
            mapping = aes(x = x_pos, y = y_pos, label = r_labels),
            parse = TRUE,
            size = 4) +
  geom_text(data = stats, 
            mapping = aes(x = x_pos2, y = y_pos2, label = p),
            parse = FALSE,
            size = 4) +
  facet_grid(rows = vars(dep_names), cols = vars(Watershed),
             labeller = as_labeller(labels), scales = "free",
             switch = "y") +
  theme(axis.text.x = element_text(size= 9),
        strip.placement = "outside")


ggsave(paste0(here, "/output/plots/f7_DepVarsVsPctDecid_mean.png"),
       width = 8, height = 10)


#6.0 Supplementary Figures ----
##6.1 Box plot of dependent variables by watershed ----
dat4 %>% 
  ggplot() +
  geom_jitter(mapping= aes(x= Watershed, y = dep_values), color = "dark grey", alpha=0.9) + 
  geom_boxplot(mapping= aes(x= Watershed, y = dep_values), alpha=0, size=1) + 
  facet_grid(rows = vars(dep_names),
             labeller = as_labeller(labels), scales = "free") +
  labs(x = "Watershed", y = "Dependent Variables")

ggsave(paste0(here, "/output/plots/fs1_DepVarsBoxplot_ByWatershed.png"),
       width = 8, height = 8)

# Perform ANOVA test on seasons
anova_res <- dat4 %>%
  group_by(dep_names)%>%
  nest() %>%
  mutate(anova= map(data,
                    ~anova_test(data = .x,
                                formula = dep_values~Watershed)))

##6.2 Box plot of dependent variables by season ----
dat4 %>% 
  ggplot() +
  geom_jitter(mapping= aes(x= season_tb, y = dep_values), color = "dark grey", alpha=0.9) + 
  geom_boxplot(mapping= aes(x= season_tb, y = dep_values), alpha=0, size=1) + 
  facet_grid(rows = vars(dep_names),
             labeller = as_labeller(labels), scales = "free") +
  labs(x = "Season", y = "Dependent Variables")

ggsave(paste0(here, "/output/plots/fs2_DepVarsBoxplot_BySeason.png"),
       width = 8, height = 8)

# Perform ANOVA test on seasons
anova_res2 <- dat4 %>%
  group_by(dep_names)%>%
  nest() %>%
  mutate(anova= map(data,
                     ~anova_test(data = .x,
                                 formula = dep_values~season_tb)))

##6.3 Plot of Mean dependent vars vs WRT and Da variables for Yakima only----

dat5 %>% 
  filter(Watershed == "Yakima",
         expl_names %in% c("tt_hr", "Da")) %>% 
  ggplot() +
  geom_point(data= dat3 %>%  filter(Watershed == "Yakima",
                                    expl_names %in% c("tt_hr", "Da")),
             mapping= aes(x= expl_values, y = dep_values),
             alpha = 0.1, fill = "grey") + 
  geom_point(mapping= aes(x= expl_values, y = dep_mean)) + 
  geom_errorbar(mapping= aes(x= expl_values, ymin = dep_mean-dep_sd,
                             ymax= dep_mean+dep_sd)) +
  scale_x_log10() +
  labs(x=NULL, y = NULL) +
  facet_grid(rows = vars(dep_names), cols = vars(expl_names),
             labeller = as_labeller(labels), scales = "free",
             switch = "y") +
  theme(axis.text.x = element_text(size= 9),
        strip.placement = "outside")

ggsave(paste0(here, "/output/plots/fs_Yakima_DepVarsVs_tt_Da.png"),
       width = 8, height = 6)

#clean environment
to_remove <- ls() %>% as_tibble() %>%
  filter(str_detect(value, "here", negate = TRUE)) %>%
  pull()
rm(list = to_remove)
