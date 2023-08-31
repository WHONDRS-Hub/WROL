library(Hmisc)
# library(psych)
library(beepr)
beep(2)
## takes a few minutes for ~6500 x 250 matrix ##
ptm <- proc.time() # Start the clock!
cor <- Hmisc::rcorr(for_cor, type = "spearman")
min <- (proc.time() - ptm)/60 # Stop the clock
beep(2) #run with lines above to signal end

p <- cor$P
#adjust p-values for multiple comparisons
#this takes a long time, so only do this at the end
# cor2 <- psych::corr.p(cor$r, cor$n, adjust = "fdr")
# p <- cor2$p #adjusted values in upper diagonal 
view(p)
cor_rnames <- rownames(cor$r)
cor_rnames
cor_sig <- cor$P < 0.1 #to filter out insignificant relations
cor_sig
cor_r <- cor$r*cor_sig #upper diagonal reflects adjusted p-values
cor_sig2 <- 0.1 < cor$r < -0.1

# tibble of spearman rho values for each mass across environmental variables
cor3 <- cor_r %>% as_tibble(rownames = "m_z") %>% 
  filter(!m_z %in% c(predictors, indices)) %>% 
  select(m_z, all_of(c(predictors, indices))) %>% 
  mutate(across(m_z, as.numeric)) %>% 
  rename_with(.fn = ~paste0(.x, "_r"), -m_z) %>% 
  mutate(across(contains("_r"), ~case_when(. < -0.1 ~ .,
                                           . > 0.1 ~ .,
                                           TRUE ~ NA_real_)))

#join to expr_meta for ploting
rho <- inner_join(expr_meta %>% select(m_z, formula, contains("_C")),
                  cor3,
                  by = "m_z" )
var <- lat_r
ggplot()+
  geom_point(rho %>% drop_na(pc_developed_r),
             mapping =  aes(x= O_C, y= H_C, color = pc_developed_r),
             alpha = 0.8)+
  scale_color_gradient2(low = "blue", high = "red", breaks=c(-1, 0, 1),
                        limit=c(-1, 1))+
  theme_bw()

ggsave(paste0(here, "/output/spearman_pc_devel.png"),
       height=5, width= 7)


corplot
ggsave(paste0(here, "/output/indices_corrplot.png"),
       width = 7, height = 8, bg = "white")


#END


# Clean objects ----
to_remove <- ls() %>% as_tibble() %>% 
  filter(str_detect(value, "here", negate = TRUE)) %>% 
  pull()
rm(list = to_remove)