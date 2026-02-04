# Quick plots.

dat_notifications %>% 
  as_tibble %>% 
  mutate(MBF_Tile = MBF %>% ntile(100),
         Acres_Tile = Acres %>% ntile(100)) %>% 
  filter(MBF_Tile %in% 2:99 & Acres_Tile %in% 2:99) %>% 
  group_by(Year_Start) %>% 
  summarize(Count = n(),
            MBF = sum(MBF),
            Acres = sum(Acres)) %>% 
  pivot_longer(cols = c(Count, MBF, Acres)) %>% 
  ggplot() +
  geom_col(aes(x = Year_Start %>% factor,
               y = value,
               fill = name %>% factor %>% fct_relevel("Count", "Acres", "MBF"))) +
  labs(x = "Year",
       y = NULL) +
  scale_x_discrete(breaks = c("2015", "2024")) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ name %>% factor %>% fct_relevel("Count", "Acres", "MBF"),
             nrow = 1,
             scales = "free") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("output/quick_1.png",
       dpi = 300,
       width = 6.5)
