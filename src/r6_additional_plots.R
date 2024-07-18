#0.0 Setup----
library(here)
here <- here()
library(tidyverse)
# library(ggpubr)
# library(ggpmisc)
library(naniar)
library(rstatix)
library(FactoMineR)
library(ggpubr)
library(factoextra)
library(corrplot)


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
         AI.Mod.mean, CHON_norm) %>% 
  mutate(across(streamorde, as.numeric))

#Select explanatory and dependent variable names
expl_vars <- dat2 %>% 
  select(WsAreaSqKm, tt_hr, Da, lulc_evenness, PctDecid2019Ws, PctConif2019Ws, q_daily_mm, streamorde) %>% 
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
         expl_names = fct_relevel(expl_names, "streamorde", "q_daily_mm"))

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
         expl_names = fct_relevel(expl_names, "streamorde",
                                  "q_daily_mm"))

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
         expl_names = fct_relevel(expl_names, "streamorde", "q_daily_mm"),
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
            `streamorde` = "Stream Order",
            `q_daily_mm` = "Specific Discharge (mm)",
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
         expl_names %in% c("q_daily_mm"),
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

# write_csv(lm_log_mean_watershed2, paste0(here, "/output/tables/lm_log_mean_watershed.csv"))

##2.0a Pearson correlation, all variables, by watershed, mean variables ----
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

# write_csv(lm_mods_mean_watershed2, paste0(here, "/output/tables/lm_mods_mean_watershed.csv"))


##2.1 Mean dependent vars vs log specific discharge----
#plotting averaged dependent variables by site
stats <- lm_log_mean_watershed2 %>% 
  filter(expl_names == "q_daily_mm")

label_coord <- dat5 %>% 
  group_by(dep_names, expl_names) %>% 
  mutate(x_pos = 0.00001, #max(expl_values, na.rm = T)*0.01,
         y_pos = max(dep_mean, na.rm = T)*1.45,
         x_pos2 = 0.00001, #max(expl_values, na.rm = T)*0.011,
         y_pos2 = max(dep_mean, na.rm = T)*1.25) %>% 
  ungroup() %>% 
  distinct(Watershed, dep_names, expl_names, .keep_all = TRUE) %>% 
  filter(expl_names == "q_daily_mm") %>% 
  select(Watershed, dep_names, x_pos, y_pos, x_pos2, y_pos2)

stats <- stats %>% 
  left_join(., label_coord, by = c("Watershed", "dep_names"))

dat6 %>% 
  pivot_wider(names_from = expl_names, values_from = expl_mean) %>% 
  ggplot() +
  geom_jitter(data = dat4, mapping= aes(x= q_daily_mm, y = dep_values),
              width = 0.1, shape = 1, color = "grey") + 
  geom_point(mapping= aes(x= q_daily_mm, y = dep_mean)) +
  geom_errorbar(mapping= aes(x= q_daily_mm, ymin = dep_mean-dep_sd, ymax= dep_mean+dep_sd)) +
  geom_smooth(mapping= aes(x= q_daily_mm, y = dep_mean), method = "lm", se=TRUE, fill = "light blue") +
  scale_x_log10(#limits = c(0.0000001, 1),
                breaks = c(0.0000001, 0.00001, 0.0001, 0.001, 0.01),
                labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(x = bquote("Specific Daily Discharge (mm)"), y = NULL) +
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

ggsave(paste0(here, "/output/plots/fs3_DepVarsVsQ_mean.png"),
       width = 8, height = 6)

##2.2  Mean dependent variables vs LULC evenness ----
stats <- lm_mods_mean_watershed2 %>% 
  filter(expl_names == "streamorde")

label_coord <- dat5 %>% 
  group_by(dep_names, expl_names) %>% 
  mutate(x_pos = 4,# max(expl_values, na.rm = T)*0.01,
         y_pos = max(dep_mean, na.rm = T)*1.45,
         x_pos2 = 4, #max(expl_values, na.rm = T)*0.8,
         y_pos2 = max(dep_mean, na.rm = T)*1.25) %>% 
  ungroup() %>% 
  distinct(Watershed, dep_names, expl_names, .keep_all = TRUE) %>% 
  filter(expl_names == "streamorde") %>% 
  select(Watershed, dep_names, x_pos, y_pos, x_pos2, y_pos2)

stats <- stats %>% 
  left_join(., label_coord, by = c("Watershed", "dep_names"))

dat5 %>%
  pivot_wider(names_from = expl_names, values_from = expl_values) %>% 
  ggplot() +
  geom_jitter(data = dat4, mapping= aes(x= streamorde, y = dep_values),
              width = 0.02, shape = 1, color = "grey") + 
  geom_point(mapping= aes(x= streamorde, y = dep_mean)) + 
  geom_errorbar(mapping= aes(x= streamorde, ymin = dep_mean-dep_sd, ymax= dep_mean+dep_sd)) +
  geom_smooth(mapping= aes(x= streamorde, y = dep_mean), method = "lm", se=TRUE, fill="light blue") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7)) + #limits = c(0, 0.7)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(x = "Stream Order", y = NULL) +
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

ggsave(paste0(here, "/output/plots/fs4_DepVarsVsStreamOrd_mean.png"),
       width = 8, height = 6)


#3.0 Ordination of watershed characteristics and explanatory variables ----

#clean environment
to_remove <- ls() %>% as_tibble() %>%
  filter(str_detect(value, "here", negate = TRUE)) %>%
  pull()
rm(list = to_remove)
##3.1 Setup for PCA ----
#import all data
dat <- readRDS(paste0(here, "/output/dat_wide.Rds")) %>% 
  convert_as_factor(vars= c("Watershed", "season_tb", "Site", "streamorde"))
glimpse(dat)

#Select Variables
dat2 <- dat %>% 
  select(Site, Date, Watershed, WsAreaSqKm, 
         ElevWs, #mean elevation in meters
         TmeanSite, #mean 30 year temperature in Celsius for selected years
         PrecipSite, #mean 30 year precipitation (mm)
         tt_hr, streamorde, Da,
         q_daily_mm,
         PctDecid2019Ws, PctConif2019Ws,
         lulc_evenness,
         DOC_mgL)

#Select variable names to average per site
mean_vars <- dat2 %>% 
  select(tt_hr, Da, q_daily_mm, DOC_mgL) %>% 
  colnames()

#Average values per site
dat3 <- dat2 %>% 
  pivot_longer(cols = all_of(mean_vars),
             names_to= "mean_names", values_to = "mean_values") %>% 
  group_by(Site, mean_names) %>% 
  mutate(mean = mean(mean_values, na.rm = TRUE)) %>% 
  ungroup() %>% 
  distinct(Site, mean_names, .keep_all = TRUE) %>% 
  select(-c(mean_values, Date)) %>% 
  pivot_wider(names_from = mean_names, values_from = mean,
              names_prefix = "mean_") %>% 
  rowid_to_column(var = "rowid") %>%
  column_to_rownames(var = "Site") %>% 
  mutate(across(streamorde, as.numeric))
  

gg_miss_var(dat3) #two sites missing Da

#Define input variable types for PCA
vars_active <- names(dat3[,3:length(dat3)])
vars_supl <- names(dat3[,1:2])

#plot QQ plots to visually inspect for normality
dat3 %>% 
  pivot_longer(all_of(vars_active)) %>% 
  group_by(name) %>% 
  ggqqplot("value", facet.by = "name", scale = "free")

#Assess overall correlations
dat4 <- dat3 %>% select(all_of(vars_active)) %>% 
  filter(!is.na(mean_Da))
corr <- cor(dat4)
png(height=6, width=6, units = "in", res = 500,
    file=paste0(here, "/output/plots/pca/fs5_site_vars_corrplot.png"),
    type = "cairo")
corrplot(corr, type = "upper", diag = FALSE,
         mar=c(0,0,1,0), tl.col = "black")
dev.off()

#get column indices for PCA input
pca_vars_active <- match(vars_active, names(dat3))
pca_vars_supl <- match(vars_supl, names(dat3))

##3.2 Run PCA----
rownames(dat3)
res.pca_active <- PCA(X = dat3, scale.unit = TRUE,
                      quali.sup = pca_vars_supl)
res.pca_active2 <- PCA(X = dat3 %>% filter(Watershed != "Connecticut"),
                       scale.unit = TRUE,
                      quali.sup = pca_vars_supl)

#Visualize the eigenvalues, scree plots
fviz_eig(res.pca_active, ncp = 5,
         title = "")
ggsave(paste0(here, "/output/plots/pca/pca_active_scree.png"))
fviz_eig(res.pca_active2, ncp = 5,
         title = "")
ggsave(paste0(here, "/output/plots/pca/pca_active_scree2.png"))

#Extract the results for individuals and variables, respectively
#Visualize the results individuals and variables, respectively.
## Principal Component Analysis Results for variables
##   Name       Description                                    
## 1 "$coord"   "Coordinates for the variables"                
## 2 "$cor"     "Correlations between variables and dimensions"
## 3 "$cos2"    "Cos2 for the variables"                       
## 4 "$contrib" "contributions of the variables"
pca_var_active <- get_pca_var(res.pca_active) 
pca_var_active2 <- get_pca_var(res.pca_active2) 
#Visualize Variables
fviz_pca_var(res.pca_active,
             arrowsize = 1,
             labelsize = 3,
             col.var = "contrib",
             col.quanti.sup = "blue",
             repel = TRUE,
             title = "A")
ggsave(paste0(here, "/output/plots/pca/pca_vars_active.png"))

fviz_pca_var(res.pca_active2,
             arrowsize = 1,
             labelsize = 3,
             col.var = "contrib",
             col.quanti.sup = "blue",
             repel = TRUE,
             title = "C")
ggsave(paste0(here, "/output/plots/pca/pca_vars_active2.png"))

#Visualize Individuals
pca_ind_active <- get_pca_ind(res.pca_active)$coord
pca_ind_active2 <- get_pca_ind(res.pca_active2)$coord
fviz_pca_ind(res.pca_active, repel = TRUE, 
             habillage = 2, addEllipses = TRUE, 
             col.ind.sup = "black", label = "none",
             title = "B")
ggsave(paste0(here, "/output/plots/pca/pca_active_ind.png"))

fviz_pca_ind(res.pca_active2, repel = TRUE, 
             habillage = 2, addEllipses = TRUE, 
             col.ind.sup = "black", label = "none",
             title = "D")
ggsave(paste0(here, "/output/plots/pca/pca_active_ind2.png"))


#4.0 Additional plots ----
dat %>% 
  ggplot() +
  geom_jitter(mapping=aes(x=streamorde, y=Da), width = 0.2) +
  scale_y_log10() +
  labs(x= "Stream Order")
ggsave(paste0(here, "/output/plots/fs_Da_vs_streamorder.png"))

glimpse(dat)
dat %>% 
  ggplot(aes(x=Date, y=DOC_mgL*0)) +
  geom_point(alpha = 0) +
  geom_vline(mapping = aes(xintercept=Date, color = Watershed))+
  scale_x_date(date_breaks = "3 month", date_labels = "%Y") +
  labs(x = "Sample Date") +
  theme(axis.title.y = element_blank(),  # Remove y-axis title
        axis.text.y = element_blank(),   # Remove y-axis text
        axis.ticks.y = element_blank())
ggsave(paste0(here, "/output/plots/fs_SampleDate.png"),
       width = 8, height = 5, units = "in")

#Check DOM chemistry difference with CT subwatersheds
dat %>% filter(Watershed == "Connecticut") %>% droplevels() %>% 
  mutate(CT_basin = case_when(str_detect(Site, "BUNN|FARM|NEPA|PHEL|STIL|UNIO") ~ "Farmington",
                              Site == "CT-THOM" ~ "Thompsonville",
                              TRUE ~ "Passumpsic")) %>% 
  select(CT_basin, DOC_mgL, peaks.with.formula, AI.Mod.mean, CHON_norm,
         normalized.transformations, everything()) %>% 
  pivot_longer(cols = DOC_mgL:normalized.transformations) %>%
  ggplot() +
  geom_jitter(mapping=aes(x=CT_basin, y= value), width = 0.2)+
  facet_wrap(~name, scales = "free") +
  labs(x = NULL)

dat %>% filter(Watershed == "Yakima") %>% 
  ggplot()

#clean environment
to_remove <- ls() %>% as_tibble() %>%
  filter(str_detect(value, "here", negate = TRUE)) %>%
  pull()
rm(list = to_remove)