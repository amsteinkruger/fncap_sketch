# sketch waterways and notifications for a subset of the McKenzie

# get fpa waterways in and set up

dat_fpa_1_test %>%
  filter(HUC8Name == "Mckenzie") %>% 
  ggplot() +
  geom_spatvector(color = "grey50") +
  geom_spatvector(data = dat_notifications_less_1,
                  color = NA,
                  fill = "red",
                  alpha = 0.50) +
  coord_sf(xlim = c(-123.0, -122.40), ylim = c(44.0, 44.3), expand = FALSE) +
  theme_minimal()

ggsave("output/mckenzie_20260217.png",
       dpi = 300,
       width = 9,
       height = 6.5)