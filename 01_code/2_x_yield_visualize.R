# Estimate simple linear models of MBF/Acre on a kitchen sink of variables.

dat = 
  "03_intermediate/dat_notifications_1_9.csv" %>% 
  read_csv %>% 
  # Get a species-agnostic yield variable.
  mutate(MBF_Acre_Both = MBF_Acre_2_DouglasFir + MBF_Acre_2_WesternHemlock) %>% 
  # Get landowner percentiles by total production of both species.
  # group_by(Landowner) %>% 
  # mutate(MBF_Both_Total = sum(MBF_2_DouglasFir) + sum(MBF_2_WesternHemlock)) %>% 
  # ungroup %>% 
  # mutate(Landowner_MBF_Percentile = ntile(MBF_Both_Total, 100)) %>% 
  # Get factors for some categorical variables.
  mutate(Activity = Activity %>% factor,
         Pyrome = Pyrome %>% factor,
         County = County %>% factor,
         District = District %>% factor) %>% 
  # Pick variables.
  select(YearCompletion,
         Activity,
         Acres_1,
         MBF_1,
         MBF_Acre_1,
         Pyrome)

# Visualize

vis =
  dat %>% 
  mutate(YearCompletion = YearCompletion %>% factor) %>% 
  group_by(YearCompletion,
           Activity,
           Pyrome) %>% 
  summarize(Acres_Sum = sum(Acres_1),
            MBF_Sum = sum(MBF_1),
            MBF_Acre_Mean = mean(MBF_Acre_1)) %>% 
  ungroup %>% 
  pivot_longer(cols = c(Acres_Sum, MBF_Sum, MBF_Acre_Mean)) %>% 
  mutate(name = name %>% factor %>% fct_relevel(c("Acres_Sum", "MBF_Sum", "MBF_Acre_Mean"))) %>% 
  ggplot() +
  geom_line(aes(x = YearCompletion,
               y = value,
               color = Activity,
               group = Activity)) +
  facet_grid(name ~ Pyrome,
             scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Export

ggsave("04_out/Smorgasbord/vis_aggregate_yield_year.png",
       vis,
       dpi = 300,
       width = 8.5,
       height = 6)
