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
################################################################################

# Convert AD2 to an sf object
AD2 <- st_as_sf(AD2, region="id")

# Create an ID column (sf objects store attributes directly, so no need for @data)
AD2$id <- as.character(seq_len(nrow(AD2)))

# Extract point geometries from polygons if needed (e.g., centroids)
AD2.points <- st_centroid(AD2)  # Or use st_coordinates() for true point extraction
AD2.points <- AD2.points[,c("id","geometry")]

# To join by ID specifically
# Perform an attribute join (ID-based join)
# Drops geometry before joining
AD2.df <- AD2.points %>%
  left_join(st_drop_geometry(AD2), by = "id") 
# Or could...
# Perform a spatial join (attaching polygon attributes to points)
# AD2.df <- st_join(AD2.points, AD2, left = TRUE)

ggplot(AD2) +
  geom_sf(aes(fill = ADM2_EN)) +
  theme_minimal()

ggplot(AD2.df) +
  geom_sf(aes(fill = ADM2_EN)) +
  theme_minimal()

