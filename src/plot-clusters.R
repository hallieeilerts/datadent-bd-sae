################################################################################
#' @description 
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(rgdal)
library(sf)
library(ggplot2)
library(RColorBrewer)
#' Inputs
source("./src/util.R")
# Bangladesh district boundaries
AD2 <- readOGR(dsn="./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm2_bbs_20201113")
# Location masked of DHS clusters
EAPoints <- readOGR(dsn="./data/BD_2022_DHS_03042025_2045_120781/BDGE81FL",layer="BDGE81FL")
################################################################################

# Make Projections consistent
#AD2 <- spTransform(AD2, CRS("+proj=utm +zone=36 +datum=WGS84"))
#EAPoints <- spTransform(EAPoints, CRS("+proj=utm +zone=36 +datum=WGS84"))
# If I do this, the map is slanted.

# Convert AD2 to an sf object
AD2 <- st_as_sf(AD2)

# Convert EAPoints to sf object
EAPoints <- st_as_sf(EAPoints)

# Plot AD2 boundaries and EAPoints locations
p1 <- ggplot() +
  # Plot AD2 boundaries
  geom_sf(data = AD2, fill = NA, color = "black", lwd = 1) +
  # Plot EAPoints using LATNUM and LONGNUM
  geom_sf(data = EAPoints, color = "red", size = 1) + 
  theme_bw() +
  labs(title = "Adm2 boundaries with geomasked survey clusters", color = "Legend") +
  scale_color_manual(values = c("EAPoints" = "red"))  # Customize point color
ggsave("./gen/clusters.png", p1, width = 10, height = 7, dpi = 300)

