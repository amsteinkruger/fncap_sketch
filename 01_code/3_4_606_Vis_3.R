# Visualize parameter estimates with standard errors. 
#  This requires outputs of 3_3_606. 

#  Maybe normalize variable ranges and reestimate models for better comparisons. 

vis_coef = 
  tibble(Stage = rep(1:2, each = 4),
         Specification = rep(1:4, 2),
         Models = 
           list(mod_hurdle_first_1, 
                mod_hurdle_first_2, 
                mod_hurdle_first_3, 
                mod_hurdle_first_4, 
                mod_hurdle_second_1, 
                mod_hurdle_second_2, 
                mod_hurdle_second_3, 
                mod_hurdle_second_4)) %>% 
  mutate(Tidy = Models %>% map(tidy, conf.int = TRUE)) %>% 
  select(Stage, Specification, Tidy) %>% 
  unnest(Tidy) %>% 
  # Names
  # mutate(ID_Term = paste0(ID, "_", term),
  #        Term_ID = paste0(term, "_", ID)) %>% 
  # Formats
  mutate(# ID = ID %>% factor,
         term = term %>% factor,
         # ID_Term = ID_Term %>% factor,
         # Term_ID = Term_ID %>% factor,
         Stage = Stage %>% factor,
         Specification = Specification %>% factor) %>% 
  # filter(term %in% c("SiteClassMode", "Elevation")) %>% # Easier for demo. 
  ggplot() + 
  geom_vline(xintercept = 0, 
             linetype = "dashed") +
  geom_pointrange(aes(x = estimate,
                      y = term,
                      xmin = conf.low,
                      xmax = conf.high,
                      color = Specification),
                  position = position_dodge(width = 0.75)) +
  facet_wrap(~ Stage,
             scales = "free_x")

ggsave("04_out/Presentation/vis_2_coefficients.png",
       vis_coef,
       dpi = 300,
       height = 4.5,
       width = 6.5)
