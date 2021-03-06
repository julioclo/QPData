rm(list = ls(all = TRUE))

setwd("~/Desktop/DATA QP")

library(tidyverse)
library(readxl)
library(lme4)
library(lmtest)

df_p_ro <- read_excel("./P_RO.xlsx")

glimpse(df_p_ro)

# Overal averages
df_p_ro %>% 
  filter(., do != 'AGC') %>%
  group_by(., group, do, animacy) %>% 
  summarize(., accuracy = mean(response), sd = sd(response)) 

# # Groups:   group, do [?]
# group    do animacy  accuracy        sd
# <chr> <chr>   <chr>     <dbl>     <dbl>
#   1     b    DP      An 0.7555556 0.4321649
#   2     b    DP    Inan 1.0000000 0.0000000
#   3     b  Pron      An 0.8777778 0.3293773
#   4     b  Pron    Inan 0.7888889 0.4103833

df_p_ro %>%
  filter(., do != 'AGC') %>% 
  mutate(., group = recode(group, `b` = "Bilinguals", `m` = "Monolinguals")) %>%
  ggplot(., aes(x = animacy, y = response, shape = do, dodge = do)) + 
  facet_grid(. ~ group) +
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  stat_summary(fun.y = mean, geom = 'point', color = 'white', size = 3, 
               position = position_dodge(width = 0.5)) + 
  scale_shape(name = "") +
  labs(x = 'Group', y = 'Response accuracy', caption = 'Mean +/- 95% CI', 
       title = 'Response accuracy as a function of group, do, and animacy.') + 
  theme_grey(base_size = 16, base_family = 'Times')

df_dp_only <- df_p_ro %>%
  filter(., do != "AGC") %>%
  mutate(., groupRC = recode(group, `m` = -0.5, `b` = 0.5), 
         doRC    = recode(do, `DP` = -0.5, `Pron` = 0.5),
         animacyRC = recode(animacy, `An` = -0.5, `Inan` = 0.5),
         mintSc = (mintro - mean(mintro)) / 2 * sd(mintro))

# Fit null, group and full models


mod_p_ro_mint <- glmer(response ~ group * do * animacy + mintSc +
                         (1 + animacyRC * doRC | participant) + 
                         (1 + animacyRC * doRC | item), 
                       data = df_dp_only, family = 'binomial', 
                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

anova(mod_p_ro_full, mod_p_ro_mint)

summary(mod_p_ro_full)
summary(mod_p_ro_mint)

# same t.test as used in the Spanish production

df_p_ro %>%
  filter(., animacy == "Inan", group == "b", do == "Pron") %>%
  group_by(., group, participant) %>%
  summarize(., resp = mean(response)) %>%
  do(tidy(t.test(.$resp, alternative = "greater", mu = 0.0, conf.level = 0.95)))

library('rmarkdown')
rmarkdown::render('/Users/Julio/Desktop/DATA QP/QP_P_RO_Jan15.R')
