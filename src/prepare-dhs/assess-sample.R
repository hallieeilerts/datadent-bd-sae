################################################################################
#' @description Assess number of households and clusters in each district
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(sf)
library(dplyr)
library(tidyr)
library(wesanderson)
library(kableExtra)
#' Inputs
source("./src/util.R")
# household variables
hr <- readRDS("./gen/prepare-dhs/temp/variables-hr.rds")
# Bangladesh district boundaries
bangladesh_1 <- st_read("./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm1_bbs_20201113")
bangladesh_2 <- st_read("./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm2_bbs_20201113")
# cluster locations
ea <- st_read("./data/BD_2022_DHS_03042025_2045_120781/BDGE81FL", layer = "BDGE81FL")
################################################################################

# total number of households
hr %>%
  summarise(n_hhd = n()) 
  
# number of households in a district
df_hhd <- hr %>%
  group_by(ADM2_EN) %>%
  summarise(n_hhd = n()) 
# number of households in a district double check
hr %>%
  group_by(ADM2_EN, hv001) %>%
  summarise(n_hhd = n()) %>%
  group_by(ADM2_EN) %>%
  summarise(sum(n_hhd))

# number of clusters in a district
df_cluster <- hr %>%
  group_by(ADM2_EN) %>%
  summarise(n_cluster = n_distinct(hv001))

# combine
dat <- merge(df_hhd, df_cluster, by = "ADM2_EN")


# Plot households ---------------------------------------------------------

# Join spatial data
plotdat <- bangladesh_2 %>% 
  left_join(dat, by = "ADM2_EN")

# plot for households
p1 <- ggplot() +
  geom_sf(data = plotdat, aes(fill = n_hhd), color = "black") +
  geom_sf_text(data = plotdat, aes(label = n_hhd), size = 2) +
  scale_fill_gradientn(
    colors = wes_palette("Zissou1", 100, type = "continuous"),
    limits = c(0, max(plotdat$n_hhd, na.rm = TRUE)),  # adjust as needed
    na.value = "grey80", name = ""
  ) +
  labs(title = "Number of households") +
  theme_void() +
  theme(text = element_text(size = 10))
# Plot for datadent presentation May 13 2025 on wide slides
ggsave(paste0("./gen/prepare-dhs/output/n-hhd.png"), p1, width = 5, height = 5)


# table for households ----------------------------------------------------

# Break into 3 columns side by side
n_cols <- 3
n_rows_per_col <- ceiling(nrow(df_hhd) / n_cols)

# Create column index
df <- df_hhd %>%
  mutate(col_grp = rep(1:n_cols, each = n_rows_per_col, length.out = n()))

# Split and reshape wide
wide_df <- df %>%
  group_by(col_grp) %>%
  mutate(row = row_number()) %>%
  ungroup() %>%
  pivot_wider(names_from = col_grp,
              values_from = c(ADM2_EN, n_hhd),
              names_sep = "")

# reorder
wide_df <- wide_df[,c("ADM2_EN1","n_hhd1","ADM2_EN2","n_hhd2","ADM2_EN3","n_hhd3")]

# replace NA with blank
wide_df_clean <- wide_df %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), ~replace_na(.x, "")))
rownames(wide_df_clean) <- NULL

# table
kable(wide_df_clean, "latex", booktabs = TRUE, 
      align = "ll ll ll", row.names = FALSE, 
      col.names = c("District","N","District","N","District","N")) %>%
  kable_styling(latex_options = c("hold_position"), font_size = 8)


# Table for clusters ------------------------------------------------------

# Break into 3 columns side by side
n_cols <- 3
n_rows_per_col <- ceiling(nrow(df_cluster) / n_cols)

# Create column index
df <- df_cluster %>%
  mutate(col_grp = rep(1:n_cols, each = n_rows_per_col, length.out = n()))

# Split and reshape wide
wide_df <- df %>%
  group_by(col_grp) %>%
  mutate(row = row_number()) %>%
  ungroup() %>%
  pivot_wider(names_from = col_grp,
              values_from = c(ADM2_EN, n_cluster),
              names_sep = "")

# reorder
wide_df <- wide_df[,c("ADM2_EN1","n_cluster1","ADM2_EN2","n_cluster2","ADM2_EN3","n_cluster3")]

# replace NA with blank
wide_df_clean <- wide_df %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), ~replace_na(.x, "")))
rownames(wide_df_clean) <- NULL

# table
kable(wide_df_clean, "latex", booktabs = TRUE, 
      align = "ll ll ll", row.names = FALSE, 
      col.names = c("District","N","District","N","District","N")) %>%
  kable_styling(latex_options = c("hold_position"), font_size = 8)


# plot clusters -----------------------------------------------------------

ea <- st_as_sf(ea, coords = c("longitude", "latitude"), crs = st_crs(bangladesh_2))

# Join spatial data
plotdat <- bangladesh_2 %>% 
  left_join(df_cluster, by = "ADM2_EN")

p2 <- ggplot() +
  geom_sf(data = plotdat, aes(fill = n_cluster), color = "black") +
  geom_sf(data = ea, color = "red", size = 1) + 
  labs(title = "Number of clusters") +
  scale_fill_gradient(low = "grey90", high = "grey20", na.value = "grey80", name = "") +
  theme_void() +
  theme(text = element_text(size = 10))
# Plot for datadent presentation May 13 2025 on wide slides
ggsave(paste0("./gen/prepare-dhs/output/n-clusters.png"), p2, width = 5, height = 5)
