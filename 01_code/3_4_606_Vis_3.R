# Visualize parameter estimates with standard errors. 
#  This requires outputs of 3_3_606. 

mod_marginal_bind = 
  mod_marginal %>% 
  rename(term = Variable) %>% 
  pivot_longer(cols = starts_with("Model"),
               values_to = "estimate",
               names_to = "Specification",
               names_prefix = "Model") %>% 
  mutate(Specification = Specification %>% as.numeric,
         Stage = 3)

#  Maybe normalize variable ranges and reestimate models for better comparisons. 
#  Maybe split on variable groups to get a digestible plot. 

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
  bind_rows(mod_marginal_bind) %>% 
  # filter(term %>% str_sub(1, 4) != "Rate") %>% 
  mutate(Variable = term %>% factor %>% fct_rev,
         Stage = Stage %>% factor,
         Specification = Specification %>% factor) %>% 
  # filter(term %in% c("SiteClassMode", "Elevation")) %>% # Easier for demo. 
  ggplot() + 
  geom_vline(xintercept = 0, 
             linetype = "dashed") +
  geom_pointrange(aes(x = estimate,
                      y = Variable,
                      xmin = conf.low,
                      xmax = conf.high,
                      color = Specification),
                  position = position_dodge(width = 0.75)) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~ Stage,
             scales = "free_x") +
  theme_pubr()

ggsave("04_out/Presentation/vis_2_coefficients.png",
       vis_coef,
       dpi = 300,
       height = 4.5,
       width = 6.5)
