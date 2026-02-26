# Estimate . . . stuff for 2YP.

# Need: 
#  - real quarterly harvest detection
#  - PAD for ownership type intersection/restriction
#  - owner company string
#  - owner scale (percentile in acres of clearcuts?)
#  - short-term VPD, fires
#  - something for land rents
#  - something for discount rates
#  - something for yield increase in time
#  - maybe treatment intersections (but not really without more data)

# Out:
#  - maps of notifications, MBF, acre by county and HUC8
#  - plots of notifications, MBF, acre in time by firm
#  - plots of notification-MBF-acre relationships (appendix)
#  - table of sample restriction impacts
#  - table of summary statistics by model variable

library(tidyverse)
library(magrittr)
library(plm)
library(stargazer)
library(sandwich)

dat = 
  "output/dat_notifications_more_annual.csv" %>% 
  read_csv %>% 
  mutate(Restriction_Overlaps = (Intersects == 0 & Equals == 0),
         Restriction_DouglasFir = (ProportionDouglasFir > 0.50),
         Restriction_NDVI = !is.na(NDVI_Change),
         Restriction_PAD = (PAD == 0),
         Restriction_None = TRUE,
         Restriction_Any = (Restriction_Overlaps == TRUE | Restriction_DouglasFir == TRUE | Restriction_NDVI == TRUE | Restriction_PAD == TRUE),
         Restriction_All = (Restriction_Overlaps == TRUE & Restriction_DouglasFir == TRUE & Restriction_NDVI == TRUE & Restriction_PAD == TRUE)) %>% 
  filter(Restriction_All == TRUE) %>% 
  select(UID, 
         Year_Start, 
         Month_Start, 
         MBF, 
         Acres, 
         Elevation, 
         Slope, 
         Roughness, 
         Pyrome, 
         VPD, 
         starts_with("Fire"),
         ProportionDouglasFir,
         starts_with("SiteClass"),
         starts_with("Distance"),
         starts_with("FPA"),
         Watershed,
         starts_with("Stumpage"),
         starts_with("Delivered")) %>% 
  mutate(MBF_Acre = MBF / Acres) %>% 
  filter(MBF < quantile(MBF, 0.99) & MBF > quantile(MBF, 0.01),
         Acres < quantile(Acres, 0.99) & Acres > quantile(Acres, 0.01),
         MBF_Acre < quantile(MBF_Acre, 0.99) & MBF_Acre > quantile(MBF_Acre, 0.01))

mod_less = dat %>% lm(MBF_Acre ~ Elevation + Slope + VPD + ProportionDouglasFir + SiteClass_Med + Distance_Place + FPA_1 + Stumpage_Real_PPI_Timber, data = .)

mod_more = 
  dat %>% 
  plm(MBF_Acre ~ 
        MBF +
        Acres +
        Elevation +
        Slope + 
        Roughness + 
        VPD + 
        Fire_0 +
        Fire_15_Doughnut +
        Fire_30_Doughnut +
        ProportionDouglasFir +
        SiteClass_Min +
        SiteClass_Max +
        SiteClass_Med +
        Distance_Road +
        Distance_Mill + 
        Distance_Place +
        FPA_1 +
        FPA_2 +
        FPA_4 + 
        Stumpage_Real_PPI +
        Stumpage_Real_PPI_Timber +
        Delivered_Real_PPI +
        Delivered_Real_PPI_Timber,
      index = c("Pyrome", "Year_Start"),
      data = .)

mod_less_iterate = 
  dat %>% 
  plm(MBF_Acre ~ 
        # MBF +
        # Acres +
        Elevation + 
        Slope + 
        VPD + 
        Fire_0 +
        Fire_15_Doughnut +
        ProportionDouglasFir + 
        SiteClass_Med + 
        Distance_Road + 
        Distance_Mill +
        FPA_1 + 
        Stumpage_Real_PPI_Timber,
      index = c("Pyrome", "Year_Start"),
      data = .)

# Begin tabulation shenanigans.

m1 = mod_less
m2 = mod_less_iterate
m3 = mod_more

se_list <- list(
  sqrt(diag(vcovHC(m1, type = "HC1"))),
  sqrt(diag(vcovHC(m2, type = "HC1", cluster = "group"))),
  sqrt(diag(vcovHC(m3, type = "HC1", cluster = "group")))
)

stargazer(m1, m2, m3,
          type = "html",
          se = se_list,
          column.labels = c("Naive", "Less Naive", "Kitchen SInk"),
          keep.stat = c("n","rsq","adj.rsq","f"),
          out = "output/estimates_20260225.html")

# Correlogram

dat %>%
  select(where(is.numeric)) %>%
  select(-UID) %>% 
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "Correlation") %>%
  ggplot(aes(var1, var2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(limits = c(-1, 1), low = "orange", mid = "white", high = "#2166AC") +
  coord_equal() +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = NULL, y = NULL, fill = "Correlation")

ggsave("output/corr_20260225.png",
       dpi = 300,
       width = 9,
       height = 9)

# PCA Biplot

X <- 
  dat %>% 
  select(where(is.numeric)) %>% 
  select(-UID) %>% 
  tidyr::drop_na()

# PCA
pca <- prcomp(X, scale. = TRUE)
ve  <- (pca$sdev^2) / sum(pca$sdev^2) 

# Scores (observations) and loadings (variables)
scores <- 
  as_tibble(pca$x[, 1:2], .name_repair = "minimal") %>%
  rename(PC1 = 1, PC2 = 2)

loadings <- 
  as_tibble(pca$rotation[, 1:2], rownames = "var") %>%
  rename(PC1 = 2, PC2 = 3)

# Scale loadings to the score range for a readable biplot
sf <- 0.8 * min(
  (max(scores$PC1) - min(scores$PC1)) / max(abs(loadings$PC1)),
  (max(scores$PC2) - min(scores$PC2)) / max(abs(loadings$PC2))
)

loadings_sc <- loadings %>% mutate(PC1 = PC1 * sf, PC2 = PC2 * sf)

# Biplot

ggplot() +
  geom_point(data = scores, aes(PC1, PC2), alpha = 0.1, shape = 21, fill = NA, color = "black") +
  geom_segment(data = loadings_sc,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.02, "npc")), color = "#444444") +
  geom_text(data = loadings_sc, aes(PC1, PC2, label = var),
            hjust = 0.5, vjust = -0.7, size = 3.2) +
  coord_equal() +
  theme_minimal(base_size = 12) +
  labs(
    x = sprintf("PC1 (%.1f%%)", 100 * ve[1]),
    y = sprintf("PC2 (%.1f%%)", 100 * ve[2]))

ggsave("output/pca_20260225.png",
       dpi = 300,
       width = 9,
       height = 9)
