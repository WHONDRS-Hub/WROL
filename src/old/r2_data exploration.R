#0.0 Setup----
library(here)
here <- here()
library(tidyverse)
library(ggpubr)
library(naniar)
library(ggpmisc)

#1.0 Input data----
# list2env(x=readRDS(paste0(here, "/output/dat_compiled_ls.Rds")),
#          envir = globalenv())

# dat <- readRDS(paste0(here, "/output/dat_wide.Rds"))


#2.0 Send merged data to visplore for exploration----
# install.packages(paste0(here, "/src/visplorer_1.4.0.zip"), repos = NULL, type = "win.binary") 
library(visplorer)
v <- visplore() #Start new instance of visplore
# once connected, call send_data(v, dataframe) to send the R dataframe to Visplore.
dat <- readRDS(paste0(here, "/output/dat_wide.Rds"))

send_data(v, dat)
# See help(visplorer) for details.
t <- get_selection(v)







#3.0 Preliminary Plots ----
dat <- readRDS(paste0(here, "/output/dat_wide.Rds"))
glimpse(dat)

ggplot(data = dat) +
  geom_point(mapping= aes(x= normalized.transformations, 
                          # y= NtoC.mean,
                          y=CHON_norm,
                          color = Watershed)) +
  # geom_smooth(mapping= aes(x= tt_hr, y= normalized.transformations), 
  #             method = "lm", se=TRUE, fill = "light blue") +
  # stat_poly_eq(use_label(c("eq", "p")),
  #              mapping= aes(x= tt_hr, y= normalized.transformations)) +
  scale_x_log10() +
  theme_bw() +
  facet_wrap(~Watershed, scales = "free")
glimpse(dat)

ggplot(data = dat, mapping= aes(x= tt_hr, y= normalized.transformations)) +
  geom_point() +
  geom_smooth(method = "lm", se=TRUE, fill = "light blue") +
  stat_poly_eq(use_label(c("eq", "p", "R2"))) +
  scale_x_log10() +
  theme_bw()

#check missing data by variable
dat %>% gg_miss_var()
glimpse(dat)

#Pivot longer for plotting
dat2 <- dat %>% 
  select(Site, Date, Watershed, WsAreaSqKm, tt_hr, season_tb,
         q_daily_mm,
         PctDecid2019Ws, PctConif2019Ws,
         ClayWs, SandWs,
         lulc_pca1, lulc_pca2, lulc_evenness,
         decid_conif, clay_sand, claysand_decidconif,
         DOC_mgL, 
         # TN_mgL,
         # number.of.peaks, 
         total.transformations, normalized.transformations,
         AI_Mod.mean,
         # DBE.mean, NOSC.mean, HtoC.mean, 
         OtoC.mean) %>% 
  pivot_longer(cols = DOC_mgL:OtoC.mean) %>% 
  group_by(Site, name) %>% 
  mutate(mean = mean(value, na.rm = TRUE),
         sd = sd(value, na.rm=TRUE)) %>% 
  ungroup() %>% 
  distinct(Site, name, .keep_all = TRUE) %>% 
  select(-value) %>% 
  mutate(across(where(is.character), as.factor))  
  # reorder(Watershed, c("Connecticut", "Gunnison",
  #                         "Deschutes", "Willamette",
  #                         "Yakima"))

ggplot(dat, aes(x=q_daily_m3s, y=DOC_mgL)) +
  geom_miss_point(size = 3) +
  theme_bw()+
  facet_wrap(~Watershed, scales = "free")

#plot vars vs. specific discharge
dat2 %>% ggplot() +
  geom_errorbar(mapping=aes(x=q_daily_mm, ymin= mean-sd, ymax= mean+sd))+
  geom_point(mapping= aes(x= q_daily_mm, y= mean)) + 
  geom_smooth(mapping= aes(x= q_daily_mm, y= mean), method = "lm", se=FALSE) +
  stat_cor(mapping= aes(x= q_daily_mm, y= mean), method= "pearson",
           label.x.npc = "middle", label.y.npc = "bottom",
           size = 2) +
  facet_wrap(~Watershed+name, scales = "free") +
  theme_bw()
ggsave(paste0(here, "/figs/xplr_q_daily_mm2.png"),
       width = 11, height = 8)

#plot vars vs. watershed area
dat2 %>% ggplot() +
  geom_errorbar(mapping=aes(x=WsAreaSqKm, ymin= mean-sd, ymax= mean+sd))+
  geom_point(mapping= aes(x= WsAreaSqKm, y= mean)) + 
  geom_smooth(mapping= aes(x= WsAreaSqKm, y= mean), method = "lm", se=FALSE) +
  stat_cor(mapping= aes(x= WsAreaSqKm, y= mean), method= "pearson",
           label.x.npc = "middle", label.y.npc = "bottom",
           size = 2) +
  facet_wrap(~Watershed+name, scales = "free") +
  theme_bw()
ggsave(paste0(here, "/figs/xplr_WSarea2.png"),
       width = 11, height = 8)

#plot vars vs. travel time
dat2 %>% ggplot() +
  geom_errorbar(mapping=aes(x=tt_hr, ymin= mean-sd, ymax= mean+sd))+
  geom_point(mapping= aes(x= tt_hr, y= mean)) + 
  geom_smooth(mapping= aes(x= tt_hr, y= mean), method = "lm", se=FALSE) +
  stat_cor(mapping= aes(x= tt_hr, y= mean), method= "pearson",
           label.x.npc = "middle", label.y.npc = "bottom",
           size = 2) +
  facet_wrap(~Watershed+name, scales = "free") +
  theme_bw()
ggsave(paste0(here, "/figs/xplr_tt_hr2.png"),
       width = 11, height = 8)

#plot vars vs. %deciduous
dat2 %>% ggplot() +
  geom_errorbar(mapping=aes(x=PctDecid2019Ws, ymin= mean-sd, ymax= mean+sd))+
  geom_point(mapping= aes(x= PctDecid2019Ws, y= mean)) + 
  geom_smooth(mapping= aes(x= PctDecid2019Ws, y= mean), method = "lm", se=FALSE) +
  stat_cor(mapping= aes(x= PctDecid2019Ws, y= mean), method= "pearson",
           label.x.npc = "middle", label.y.npc = "bottom",
           size = 2) +
  facet_wrap(~Watershed+name, scales = "free") +
  theme_bw()
ggsave(paste0(here, "/figs/xplr_pcDecid2.png"),
       width = 11, height = 8)

#plot vars vs. %conifer
dat2 %>% ggplot() +
  geom_errorbar(mapping=aes(x=PctConif2019Ws, ymin= mean-sd, ymax= mean+sd))+
  geom_point(mapping= aes(x= PctConif2019Ws, y= mean)) + 
  geom_smooth(mapping= aes(x= PctConif2019Ws, y= mean), method = "lm", se=FALSE) +
  stat_cor(mapping= aes(x= PctConif2019Ws, y= mean), method= "pearson",
           label.x.npc = "middle", label.y.npc = "bottom",
           size=2) +
  facet_wrap(~Watershed+name, scales = "free") +
  theme_bw()
ggsave(paste0(here, "/figs/xplr_pcConifer2.png"),
       width = 11, height = 8)

#plot vars vs. lulc evenness
dat2 %>% ggplot() +
  geom_errorbar(mapping=aes(x=lulc_evenness, ymin= mean-sd, ymax= mean+sd))+
  geom_point(mapping= aes(x= lulc_evenness, y= mean)) + 
  geom_smooth(mapping= aes(x= lulc_evenness, y= mean), method = "lm", se=FALSE) +
  stat_cor(mapping= aes(x= lulc_evenness, y= mean), method= "pearson",
           label.x.npc = "middle", label.y.npc = "bottom",
           size=2) +
  facet_wrap(~Watershed+name, scales = "free") +
  theme_bw()
ggsave(paste0(here, "/figs/xplr_lulc_evenness.png"),
       width = 11, height = 8)

dat2 %>% ggplot() +
  geom_errorbar(mapping=aes(x=decid_conif, ymin= mean-sd, ymax= mean+sd))+
  geom_point(mapping= aes(x= decid_conif, y= mean)) + 
  geom_smooth(mapping= aes(x= decid_conif, y= mean), method = "lm", se=FALSE) +
  stat_cor(mapping= aes(x= decid_conif, y= mean), method= "pearson",
           label.x.npc = "middle", label.y.npc = "bottom",
           size=2) +
  facet_wrap(~Watershed+name, scales = "free") +
  theme_bw()
ggsave(paste0(here, "/figs/xplr_lulc_evenness.png"),
       width = 11, height = 8)

#boxplot vars vs. season
dat %>% ggplot() +
  geom_boxplot(mapping= aes(x= season_tb, y= normalized.transformations)) + 
  facet_wrap(~Watershed, scales = "free") +
  theme_bw()
ggsave(paste0(here, "/figs/xplr_season.png"),
       width = 11, height = 8)


#boxplot DOC per watershed
dat %>% ggplot() +
  geom_boxplot(mapping= aes(x= Watershed, y= DOC_mgL),
               alpha = 0.1, size=1, show.legend = FALSE) + 
  geom_jitter(mapping= aes(x= Watershed, y= DOC_mgL, color=Site),
              width = 0.2, size= 3, show.legend = FALSE) + 
  theme_bw()

ggsave(paste0(here, "/figs/xplr_DOC_v_watershed.png"),
       width = 11, height = 8)

#biplot DOC per watershed
dat %>% #filter(Watershed != "Yakima") %>% 
  ggplot() +
  geom_point(mapping= aes(x= DOC_mgL, y= normalized.transformations,
                          color=Watershed), size=3) + 
  theme_bw()

ggsave(paste0(here, "/figs/xplr_trans_v_DOC.png"),
       width = 11, height = 8)

#scatter plot transformations v Q
dat %>% #filter(Watershed != "Yakima") %>% 
  ggplot() +
  geom_point(mapping= aes(x= log(q_daily_mm), y= normalized.transformations,
                          color=Site), size=3) + 
  theme_bw()+
  facet_wrap(~Watershed, scales = "free")

ggsave(paste0(here, "/figs/xplr_trans_v_Q.png"),
       width = 11, height = 8)




#clean environment
to_remove <- ls() %>% as_tibble() %>%
  filter(str_detect(value, "here", negate = TRUE)) %>%
  pull()
rm(list = to_remove)

