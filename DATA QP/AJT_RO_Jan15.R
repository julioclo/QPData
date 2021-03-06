rm(list = ls(all = TRUE))

setwd("~/Desktop/DATA QP")

library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)

df_ajt_ro <- read_excel("./AJT_RO.xlsx")

glimpse(df_ajt_ro)

# Overal averages
df_ajt_ro %>% 
  filter(., do != 'AGC') %>%
  group_by(., group, do, animacy, gram) %>% 
  summarize(., accuracy = mean(response), sd = sd(response)) 

# group    do animacy  gram accuracy        sd
# <chr> <chr>   <chr> <chr>    <dbl>     <dbl>
# 1     b    DP      An     g 3.977778 1.2337837
# 2     b    DP      An     u 3.288889 1.3249738
# 3     b    DP    Inan     g 4.288889 1.0362130
# 4     b    DP    Inan     u 1.577778 0.8657338
# 5     b  Pron      An     g 4.688889 0.8208151
# 6     b  Pron      An     u 2.333333 1.4459976
# 7     b  Pron    Inan     g 4.088889 1.1836427
# 8     b  Pron    Inan     u 2.355556 1.1511961


ajt_p2 <- df_ajt_ro %>%
  filter(., do != 'AGC') %>% 
  ggplot(., aes(x = animacy, y = response, shape = do, color = gram, dodge = gram)) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  stat_summary(fun.y = mean, geom = 'point', color = 'white', size = 3, 
               position = position_dodge(width = 0.5)) +
  scale_color_brewer(palette = "Set1", name = "", labels = c("Gram.", "Ungram.")) +
  labs(x = 'Animacy', y = '% Response', caption = 'Mean +/- 95% CI', 
       title = 'Acceptability of DOM as a function of do, animacy,\nand grammaticality.') + 
  theme_grey(base_size = 16, base_family = 'Times')
