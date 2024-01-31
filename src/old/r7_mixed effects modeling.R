

# setup ----
library(tidyverse)
library(lme4)
library(merTools)
library(ggeffects)
# library(vegan)
library(here)
here <- here()
here


dat <- readRDS(paste0(here, "/output/dat_wide.Rds"))
glimpse(dat)

dat2 <- dat %>% 
  convert_as_factor(vars= c("Watershed", "season_tb", "Site"))

#Watershed as random effect with Sites nested in Watersheds
m1 <- lmer(normalized.transformations ~ WsAreaSqKm + tt_hr + season_n + lulc_evenness + (1 | Watershed/Site), data = dat2)

summary(m1)
ranef(m1)
plot(m1)
qqnorm(resid(m1))
qqline(resid(m1))

m1_pred <- ggpredict(m1, terms= c("tt_hr"))

ggplot(m1_pred) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_point(data = dat2,                      # adding the raw data (scaled values)
               aes(x = tt_hr, y = normalized.transformations, colour = Watershed)) +
  labs(x="tt_hr", y= "trans.") +
    theme_bw()


dat2 <- dat %>% 
  convert_as_factor(vars= c("Watershed", "season_tb", "Site", "streamorde"))

m2 <- lmer(normalized.transformations ~ Watershed + tt_hr + season_n + lulc_evenness + (1 | streamorde), data = dat2)

summary(m2)
ranef(m2)
plot(m2)

plotREsim(REsim(m2))


#clean environment
to_remove <- ls() %>% as_tibble() %>%
  filter(str_detect(value, "here", negate = TRUE)) %>%
  pull()
rm(list = to_remove)
