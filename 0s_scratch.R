#  Check the distribution of pre-to-post-notification changes in TCC.

# library(ggpubr)
# library(RColorBrewer)
# 
# pal_tcc_lower = brewer.pal('Reds', n = 8) %>% rev
# pal_tcc_upper = brewer.pal('Greys', n = 7)
# pal_tcc = c(pal_tcc_lower, pal_tcc_upper)

# vis_tcc = 
#   dat_join_tcc %>% 
#   mutate(bin = cut_interval(TCC_Change, length = 10)) %>% 
#   group_by(bin) %>% 
#   summarize(count = n()) %>% 
#   ungroup %>% 
#   ggplot() +
#   geom_col(aes(x = bin,
#                y = count,
#                fill = bin),
#            color = "#000000") +
#   labs(x = "Change in TCC (Pre- to Post-Notification)",
#        y = "Notification Count") +
#   scale_fill_manual(values = pal_tcc) +
#   scale_y_continuous(expand = c(0, 0)) +
#   theme_pubr() +
#   theme(legend.position = "none",
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
#   
# ggsave("output/vis_tcc_20251210.png",
#        dpi = 300,
#        width = 6.5)