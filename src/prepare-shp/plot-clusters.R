################################################################################
#' @description Plot location of geomasked survey clusters in adm2 regions
#' @return plot
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(sf)
library(ggplot2)
#' Inputs
source("./src/util.R")
# Location masked of DHS clusters
ea <- st_read("./data/BD_2022_DHS_03042025_2045_120781/BDGE81FL", layer = "BDGE81FL")
# Bangladesh district boundaries
bangladesh_2 <- st_read("./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm2_bbs_20201113")
################################################################################

# convert to sf object with same CRS as shapefile
ea <- st_as_sf(ea, coords = c("longitude", "latitude"), crs = st_crs(bangladesh_2))

# plot AD2 boundaries and ea locations
p1 <-ggplot() +
  geom_sf(data = bangladesh_2, fill = NA, color = "black", lwd = 1) +
  geom_sf(data = ea, color = "red", size = 1) + 
  theme_bw() +
  labs(title = "Adm2 boundaries with geomasked survey clusters", color = "Legend") +
  scale_color_manual(values = c("ea" = "red")) 
ggsave("./gen/prepare-shp/audit/clusters.png", p1, width = 10, height = 7, dpi = 300)

