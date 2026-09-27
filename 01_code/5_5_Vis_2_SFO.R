# Visualize the distribution of production over firms. Ideally represent price and fire responses, too.

# Count ~ x, with x in acres owned, acres harvested, MBF harvested; split bars by SFO/non-SFO. 

#  Palette

# pal_fire = brewer.pal(9, "Reds")[c(2, 4, 6, 8)] %>% rev

pal = brewer.pal(3, "Set1")[2:3]

#  Data

dat = 
  "03_intermediate/dat_firms_implicit_3_1.csv" %>% 
  read_csv %>% 
  mutate(MBF_Both = ceiling(MBF_DouglasFir + MBF_WesternHemlock)) %>% 
  group_by(Landowner, Landowner_ID) %>% 
  summarize(MBF_Mean = sum(MBF_Both) / n_distinct(QuarterCompletion),
            Acres_Mean = sum(Acres) / n_distinct(QuarterCompletion),
            Owner_Acres_Mean = sum(Owner_Acres) / n_distinct(QuarterCompletion),
            Fire = weighted.mean(Fire_30, MBF_Both)) %>% 
  ungroup %>% 
  mutate(
    SFO = 
      ifelse(
        Landowner_ID %in% vec_small & MBF_Mean < 2000, 
        "Small Landowners", 
        "Large Landowners"), 
    Fire_Factor = 
      case_when(Fire == 0 ~ "0",
                Fire > 0 & Fire <= 1 ~ "0-1",
                Fire > 1 & Fire <= 2 ~ "1-2",
                Fire > 2 ~ "3+") %>% 
      factor
  )

#  Visualizations

#   Log Distribution

vis_log = 
  dat %>% 
  mutate(
    MBF_Mean_Log = MBF_Mean %>% log %>% round(0),
    MBF_Mean_Log_Bins = MBF_Mean_Log %>% cut(breaks = 12),
    MBF_Mean_Log_Bins_Neat = 
      MBF_Mean_Log_Bins %>% 
      fct_recode("(0,1]" = "(-0.012,1]") %>% 
      str_replace_all(",", ", ") %>% 
      factor %>% 
      fct_reorder(MBF_Mean_Log_Bins %>% as.numeric),
    Fire_Factor = Fire_Factor %>% fct_rev,
    SFO = SFO %>% factor %>% fct_rev
  ) %>% 
  group_by(MBF_Mean_Log_Bins_Neat, SFO) %>% 
  summarize(Firms = n()) %>% 
  ungroup %>% 
  ggplot() + 
  geom_col(aes(x = MBF_Mean_Log_Bins_Neat,
               y = Firms,
               fill = SFO),
           position = position_dodge2(preserve = "single")) +
  labs(x = "Log(Production)",
       y = "Firms",
       fill = NULL) + # "Landowner SFO Status"
  scale_fill_manual(values = pal) +
  scale_y_continuous(expand = c(0, 0)) +
  # guides(fill = guide_legend(reverse = TRUE)) +
  theme_pubr() +
  theme(axis.text.x = 
          element_text(angle = 45, 
                       hjust = 1,
                       vjust = 1))

#   Lorenz Curve

vis_lor = 
  dat %>% 
  mutate(MBF_Mean_Percentile = 
           ntile(MBF_Mean, 20) %>% 
           `*` (5) %>% 
           factor %>% 
           fct_expand("0", after = 0),
         MBF_Mean_Percent = MBF_Mean / sum(MBF_Mean)) %>% 
  arrange(MBF_Mean_Percentile) %>% 
  group_by(MBF_Mean_Percentile, SFO) %>% 
  summarize(MBF_Mean_Percent = sum(MBF_Mean_Percent)) %>% 
  ungroup %>% 
  pivot_wider(values_from = MBF_Mean_Percent,
              names_from = SFO,
              names_prefix = "SFO_") %>% 
  mutate(across(starts_with("SFO"), ~ replace_na(.x, 0)),
         across(starts_with("SFO"), ~ cumsum(.x))) %>% 
  pivot_longer(cols = starts_with("SFO"),
               values_to = "MBF_Mean_Percent",
               names_to = "SFO",
               names_prefix = "SFO_") %>% 
  mutate(SFO_Factor = SFO %>% factor,
         MBF_Mean_Percent = MBF_Mean_Percent * 100) %>% 
  ggplot() + 
  geom_col(aes(x = MBF_Mean_Percentile,
               y = MBF_Mean_Percent,
               fill = SFO_Factor),
           width = 1) +
  geom_abline(intercept = -5, 
              slope = 5, 
              linetype = "dashed") +
  labs(x = "Firm Percentile by Mean Production",
       y = "Percent Cumulative Production",
       fill = NULL) +
  scale_fill_manual(values = pal %>% rev) +
  scale_x_discrete(limits = seq(0, 100, by = 5) %>% as.character,
                   breaks = c(0, 25, 50, 75, 100)) +
  scale_y_continuous(expand = c(0, 0),
                     position = "right") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_pubr()

vis_both = 
  vis_log + 
  vis_lor + 
  plot_layout(guides = "collect") &
  theme(legend.direction = "horizontal",
        legend.position = "bottom")

ggsave("04_out/Paper_FirmSupply/vis_2_curves.png",
       vis_both,
       dpi = 300,
       height = 4.5,
       width = 7.0)
