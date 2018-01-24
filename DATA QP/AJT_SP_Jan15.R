rm(list = ls(all = TRUE))

setwd("~/Desktop/DATA QP")

library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)

df_ajt_sp <- read_excel("./AJT_SP.xlsx")

glimpse(df_ajt_sp)

# Overal averages
df_ajt_sp %>% 
  filter(., do != 'AGC') %>%
  group_by(., group, do, animacy, gram) %>% 
  summarize(., accuracy = mean(response), sd = sd(response)) 

# group    do animacy  gram accuracy        sd
# <chr> <chr>   <chr> <chr>    <dbl>     <dbl>
#  1     b    DP      An     g 4.355556 0.8021436
#  2     b    DP      An     u 3.244444 1.3340907
#  3     b    DP    Inan     g 4.422222 0.7534433
#  4     b    DP    Inan     u 2.444444 1.2532785
#  5     b  Pron      An     g 4.333333 0.7977240
#  6     b  Pron      An     u 2.888889 1.4177803
#  7     b  Pron    Inan     g 4.311111 0.7330578
#  8     b  Pron    Inan     u 3.088889 1.4113537
#  9     m    DP      An     g 4.600000 0.7442084
# 10     m    DP      An     u 1.577778 0.7534433
# 11     m    DP    Inan     g 4.725000 0.5057363
# 12     m    DP    Inan     u 1.311111 0.4681794
# 13     m  Pron      An     g 4.200000 0.8420754
# 14     m  Pron      An     u 2.000000 1.1766968
# 15     m  Pron    Inan     g 4.377778 0.6838690
# 16     m  Pron    Inan     u 1.525000 0.5057363

ajt_p2 <- df_ajt_sp %>%
  filter(., do != 'AGC') %>% 
  mutate(., group = recode(group, `b` = "Bilinguals", `m` = "Monolinguals")) %>%
  ggplot(., aes(x = animacy, y = response, shape = do, color = gram, dodge = gram)) + 
  facet_grid(. ~ group) +
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  stat_summary(fun.y = mean, geom = 'point', color = 'white', size = 3, 
               position = position_dodge(width = 0.5)) + 
  scale_shape(name = "") +
  scale_color_brewer(palette = "Set1", name = "", labels = c("Gram.", "Ungram.")) +
  labs(x = 'Group', y = '% Response', caption = 'Mean +/- 95% CI', 
       title = 'Acceptability of DOM as a function of group, do, animacy,\nand grammaticality.') + 
  theme_grey(base_size = 16, base_family = 'Times')

ggsave("ajt_p2.png", plot = ajt_p2, dpi = 600, 
       device = "png", path = "./figs", 
       height = 6, width = 9, units = "in")


mintSc <- (mintsp - mean(mintsp) / 2 * sd(mintsp))

df_ajt_sp <- df_ajt_sp %>%
  filter(., do != "AGC") %>%
  mutate(., groupRC = recode(group, `m` = -0.5, `b` = 0.5), 
            doRC    = recode(do, `DP` = -0.5, `Pron` = 0.5),
            animacyRC = recode(animacy, `An` = -0.5, `Inan` = 0.5),
            gramRC = recode(gram, `g` = -0.5, `u` = 0.5), 
            mintSc = (mintsp - mean(mintsp)) / 2 * sd(mintsp))

mod_full <- lmer(response ~ groupRC + doRC + animacyRC + gramRC + mintSc + 
     (1 + doRC + animacyRC + gramRC | participant) + 
     (1 + doRC + animacyRC + gramRC | item), 
     data = df_ajt_sp, 
     control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

summary(mod_full)

# Main effect of group and gram, we will separate gram (we are interested in ungram)
df_ajt_sp_ungram <- filter(df_ajt_sp, gram == "u")

mod_full_ungram <- lmer(response ~ groupRC + doRC + animacyRC + mintSc + 
                   (1 + doRC + animacyRC | participant) + 
                   (1 + doRC + animacyRC | item), 
                 data = df_ajt_sp_ungram, 
                 control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

mod_full_int <- lmer(response ~ groupRC * doRC * animacyRC + mintSc + 
                    (1 + doRC + animacyRC | participant) + 
                    (1 + doRC + animacyRC | item), 
                    data = df_ajt_sp_ungram, 
                    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))


summary(mod_full_ungram)
summary(mod_full_int) # 3 way interaction: group by do by animacy

# Separate by groups
df_ajt_sp_ungram_bi <- filter(df_ajt_sp_ungram, group == "b")

mod_bi_full_int <- lmer(response ~ doRC * animacyRC + mintSc + 
                       (1 + doRC + animacyRC | participant) + 
                       (1 + doRC + animacyRC | item), 
                     data = df_ajt_sp_ungram_bi, 
                     control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

summary(mod_bi_full_int)

df_ajt_sp_ungram_bi_inan <- filter(df_ajt_sp_ungram_bi, animacy == "Inan")

mod_bi_only_inan <- lmer(response ~ do + 
                          (1 | participant) + 
                          (1 + do | item), 
                        data = df_ajt_sp_ungram_bi_inan, 
                        control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

summary(mod_bi_only_inan)

## Probando

df_ajt_sp_ungram_bi_inanPron <- filter(df_ajt_sp_ungram_bi, do == "Pron")

mod_bi_only_inanPron <- lmer(response ~ group + 
                           (1 | participant) + 
                           (1 | item), 
                         data = df_ajt_sp_ungram_bi_inanPron, 
                         control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

summary(mod_bi_only_inanDP)

## Probando

df_ajt_sp_ungram <- filter(df_ajt_sp, gram == "u")
df_ajt_sp_ungram_DP <- filter(df_ajt_sp_ungram, do == "DP")
df_ajt_sp_ungram_DP_An <- filter(df_ajt_sp_ungram_DP, animacy == "An")

mod_full_ungram_DP_An <- lmer(response ~ groupRC + mintSc + 
                          (1 | participant) + 
                          (1 | item), 
                        data = df_ajt_sp_ungram_DP_An, 
                        control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

summary(mod_full_ungram_DP_An)

df_ajt_sp_ungram <- filter(df_ajt_sp, gram == "u")
df_ajt_sp_ungram_Pron <- filter(df_ajt_sp_ungram, do == "Pron")
df_ajt_sp_ungram_Pron_Inan <- filter(df_ajt_sp_ungram_Pron, animacy == "Inan")

mod_full_ungram_Pron_Inan <- lmer(response ~ groupRC + mintSc + 
                                (1 | participant) + 
                                (1 | item), 
                              data = df_ajt_sp_ungram_DP_An, 
                              control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

summary(mod_full_ungram_Pron_Inan)