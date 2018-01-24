rm(list = ls(all = TRUE))

setwd("~/Desktop/DATA QP")

library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(broom)

df_p_sp <- read_excel("./P_SP.xlsx")

glimpse(df_p_sp)

# Overal averages
df_p_sp %>% 
  filter(., do != 'AGC') %>%
  group_by(., group, do, animacy) %>% 
  summarize(., accuracy = mean(response), sd = sd(response)) 

unique(df_p_sp$response)

# group    do animacy   accuracy        sd
#    b    DP      An 0.83333333 0.3747658
#    b    DP    Inan 0.02222222 0.1482314
#    b  Pron      An 0.83333333 0.3747658
#    b  Pron    Inan 0.23333333 0.4253221
#    m    DP      An 1.00000000 0.0000000
#    m    DP    Inan 0.00000000 0.0000000
#    m  Pron      An 0.98823529 0.1084652
#    m  Pron    Inan 0.00000000 0.0000000    

prod_p1 <- df_p_sp %>%
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
  labs(x = 'Group', y = '% DOM', caption = 'Mean +/- 95% CI', 
       title = 'Proportion of DOM as a function of group, do, and animacy.') + 
  theme_grey(base_size = 14, base_family = 'Times')

ggsave("prod_p1.png", plot = prod_p1, dpi = 600, 
       device = "png", path = "./figs", 
       height = 6, width = 9, units = "in")



df_dp_b_only <- df_p_sp %>%
  filter(., do != "AGC", group == "b") %>%
  mutate(., groupRC = recode(group, `m` = -0.5, `b` = 0.5), 
            doRC    = recode(do, `DP` = -0.5, `Pron` = 0.5),
            animacyRC = recode(animacy, `An` = -0.5, `Inan` = 0.5),
            mintSc = (mintsp - mean(mintsp)) / 2 * sd(mintsp))

mod_p_sp_mint <- readRDS("./final_mod.rds")

# Fit null, group and full models
mod_p_sp_full <- glmer(response ~ doRC + animacyRC +
                         (1 + doRC + animacyRC | participant) + 
                         (1 + doRC + animacyRC | item), 
                       data = df_dp_b_only, family = 'binomial', 
                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

mod_p_sp_mint <- glmer(response ~ doRC + animacyRC + mintSc +
                      (1 + doRC + animacyRC | participant) + 
                      (1 + doRC + animacyRC | item), 
                      data = df_dp_b_only, family = 'binomial', 
                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

if(F){
mod_p_sp_int <- glmer(response ~ doRC + animacyRC + mintSc + doRC:animacyRC +
                      (1 | participant) + 
                      (1 | item), 
                      data = df_dp_b_only, family = 'binomial', 
                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
saveRDS(mod_p_sp_mint, file = "./final_mod.rds", compress = "xz")
}

anova(mod_p_sp_full, mod_p_sp_mint, mod_p_sp_int)

summary(mod_p_sp_full)
summary(mod_p_sp_mint)
summary(mod_p_sp_int)


# Do bilinguals differ from monolinguals in their 
# responses for animate objects
# - we know there is no difference in DO so we collapse
#   over this factor

df_dp_animate <- df_p_sp %>%
  filter(., do != "AGC", animacy == "An") %>%
  mutate(., groupRC = recode(group, `m` = -0.5, `b` = 0.5), 
         doRC    = recode(do, `DP` = -0.5, `Pron` = 0.5),
         animacyRC = recode(animacy, `An` = -0.5, `Inan` = 0.5),
         mintSc = (mintsp - mean(mintsp)) / 2 * sd(mintsp))

animacy_mod <- glmer(response ~ groupRC + (1 | participant) + (1 | item), 
      data = df_dp_animate, family = "binomial", 
      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

summary(animacy_mod)
# YES


# Do bilinguals differ from monolinguals in their 
# responses for inanimate pronoun objects

df_dp_inanimate_pro <- df_p_sp %>%
  filter(., do == "Pron", animacy == "Inan") %>%
  mutate(., groupRC = recode(group, `m` = -0.5, `b` = 0.5), 
         mintSc = (mintsp - mean(mintsp)) / 2 * sd(mintsp)) %>% as.data.frame

inanimacy_mod <- glmer(response ~ groupRC + (1 | participant) + (1 | item), 
                     data = df_dp_inanimate_pro, family = "binomial", 
                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

summary(inanimacy_mod)
# Model cannot run because "group" is perfectly correlated with response (all monos avoid use of DOM)
# Options
#  1. ttest agaist 0 (do bi use DOM significanly above 0)
#  2. add some 1´s for mono group to make model run
# How to decide? Talk to liliana about what questions are most important to answer

df_p_sp %>%
  filter(., animacy == "Inan", group == "b", do != "AGC") %>%
  group_by(., group, do, participant) %>%
  summarize(., resp = mean(response)) %>%
  do(tidy(t.test(.$resp, alternative = "greater", mu = 0.0, conf.level = 0.95)))

# Yes, the bilinguals use "a" significantly greater than 
# 0 for inanimate objects pronouns

# group    do   estimate statistic     p.value parameter    conf.low
#     b    DP 0.02222222  1.000000 0.165666381        17 -0.01643571
#     b  Pron 0.23333333  3.378011 0.001786916        17  0.11317143

# Do bilinguals differ from monolinguals in their 
# responses for animate DPs
# - we know there is no difference in DO so we collapse
#   over this factor

df_dp_animateDP <- df_p_sp %>%
  filter(., do == "DP", animacy == "An") %>%
  mutate(., groupRC = recode(group, `m` = -0.5, `b` = 0.5), 
         doRC    = recode(do, `DP` = -0.5, `Pron` = 0.5),
         animacyRC = recode(animacy, `An` = -0.5, `Inan` = 0.5),
         mintSc = (mintsp - mean(mintsp)) / 2 * sd(mintsp))

summary(df_dp_animateDP)

animacyDP_mod <- glmer(response ~ groupRC + (1 | participant) + (1 | item), 
                     data = df_dp_animateDP, family = "binomial", 
                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

library(lm.beta)

st.animacyDP_mod <- standardize(animacyDP_mod, standardize.y=FALSE)

summary(animacy_modDP)
# YES


df_p_sp %>%
  filter(., animacy == "An", group == "b", do == "DP") %>%
  group_by(., group, participant) %>%
  summarize(., resp = mean(response)) %>%
  do(tidy(t.test(.$resp, alternative = "greater", mu = 0.0, conf.level = 0.95)))
