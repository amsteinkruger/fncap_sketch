# Visualize the distribution of production over firms. Ideally represent price and fire responses, too.

#  Level distribution? (Nope.)
#  Log distribution? (Maybe.)
#  Lorenz curves?
#  Zipf curves? (Seems a little difficult.)

#  Maybe log distribution and lorenz curves

#  Palette

library(RColorBrewer)

pal_fire = brewer.pal(9, "Reds")[c(2, 4, 6, 8)] %>% rev

#  Data

dat = 
  "03_intermediate/dat_firms_implicit_3_1.csv" %>% 
  read_csv %>% 
  mutate(MBF_Both = MBF_DouglasFir + MBF_WesternHemlock,
         MBF_Both = MBF_Both %>% ceiling) %>% 
  group_by(Landowner) %>% 
  summarize(MBF_Mean = sum(MBF_Both) / n_distinct(Year),
            Fire = weighted.mean(Fire_30_Lag_1, MBF_Both)) %>% 
  ungroup %>% 
  mutate(Fire_Factor = 
           case_when(Fire == 0 ~ "0",
                     Fire > 0 & Fire <= 1 ~ "0-1",
                     Fire > 1 & Fire <= 2 ~ "1-2",
                     Fire > 2 ~ "3+") %>% 
           factor)

#  Visualizations

#   Log Distribution

vis_log = 
  dat %>% 
  mutate(MBF_Mean_Log = MBF_Mean %>% log %>% round(0),
         MBF_Mean_Log_Bins = MBF_Mean_Log %>% cut(breaks = 12),
         MBF_Mean_Log_Bins_Neat = 
           MBF_Mean_Log_Bins %>% 
           fct_recode("(0,1]" = "(-0.012,1]") %>% 
           str_replace_all(",", ", ") %>% 
           factor %>% 
           fct_reorder(MBF_Mean_Log_Bins %>% as.numeric),
         Fire_Factor = Fire_Factor %>% fct_rev) %>% 
  group_by(MBF_Mean_Log_Bins_Neat, Fire_Factor) %>% 
  summarize(Firms = n()) %>% 
  ungroup %>% 
  ggplot() + 
  geom_col(aes(x = MBF_Mean_Log_Bins_Neat,
               y = Firms,
               fill = Fire_Factor)) +
  scale_fill_manual(values = pal_fire) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr()

#   Lorenz Curve

vis_lor = 
  dat %>% 
  mutate(MBF_Mean_Percentile = ntile(MBF_Mean, 100) %>% factor,
         MBF_Mean_Percent = MBF_Mean / sum(MBF_Mean)) %>% 
  arrange(MBF_Mean_Percentile) %>% 
  group_by(MBF_Mean_Percentile, Fire_Factor) %>% 
  summarize(MBF_Mean_Percent = sum(MBF_Mean_Percent)) %>% 
  ungroup %>% 
  pivot_wider(values_from = MBF_Mean_Percent,
              names_from = Fire_Factor,
              names_prefix = "Fire_") %>% 
  mutate(across(starts_with("Fire"), ~ replace_na(.x, 0)),
         across(starts_with("Fire"), ~ cumsum(.x))) %>% 
  pivot_longer(cols = starts_with("Fire"),
               values_to = "MBF_Mean_Percent",
               names_to = "Fire",
               names_prefix = "Fire_") %>% 
  mutate(Fire_Factor = Fire %>% factor %>% fct_rev,
         MBF_Mean_Percent = MBF_Mean_Percent * 100) %>% 
  ggplot() + 
  geom_col(aes(x = MBF_Mean_Percentile,
               y = MBF_Mean_Percent,
               fill = Fire_Factor),
           width = 1) +
  geom_abline(intercept = 0, 
              slope = 1, 
              linetype = "dashed") +
  scale_fill_manual(values = pal_fire) +
  scale_x_discrete(breaks = c(1, 25, 50, 75, 100)) +
  scale_y_continuous(expand = c(0, 0),
                     position = "right") +
  theme_pubr()

vis_both = vis_log + vis_lor

ggsave("04_out/Presentation/vis_2_curves.png",
       vis_both,
       dpi = 300,
       height = 4.5,
       width = 6.5)
