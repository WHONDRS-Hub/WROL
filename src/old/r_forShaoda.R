#0.0 Setup----
library(here)
here <- here()
library(tidyverse)
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
  select(sample, Site, Date, Watershed, WsAreaSqKm, tt_hr, 
         streamorde,
         Da,
         # season,
         COMID,
         q_daily_cms, q_daily_mm,
         Twat_degC,
         DOC_mgL,
         number.of.peaks,
         total.transformations, normalized.transformations,
         AI_Mod.mean
         )

t <- dat2

t <- dat2 %>% 
  mutate(Damk_draft_index = (tt_hr/24)/(0.038*(2^((Twat_degC-15)/10))))

ggplot(t) +
  geom_point(mapping = aes(x= log(Da), y = normalized.transformations,
                           color = Watershed), size = 5, alpha = 0.5)+
  facet_wrap(~Watershed, scales = "free")

ggplot(t) +
  geom_point(mapping = aes(y= Da, x = streamorde,
                           color = streamorde))+
  labs(title = "Yakima")

ggplot(t) +
  geom_point(mapping = aes(x= Da, y = WsAreaSqKm,
                           color = streamorde))+
  labs(title = "Yakima")

#SAVE
write_csv(dat2, paste0(here, "/data/waterTemp/forDamkohler.csv"))

