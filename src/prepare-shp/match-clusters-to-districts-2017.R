################################################################################
#' @description Match GPS coordinates of clusters to BD adm2 districts
#' @return spreadsheet with columns for cluster number and district name
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(sf)
#' Inputs
source("./src/util.R")
# Location masked of DHS clusters
ea <- st_read("./data/BD_2017-18_DHS_08202025_153_120781/BDGE7SFL", layer = "BDGE7SFL")
# Bangladesh district boundaries
bangladesh_2 <- st_read("./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm2_bbs_20201113")
################################################################################

# convert to sf object with same CRS as shapefile
ea_crs <- st_as_sf(ea, coords = c("longitude", "latitude"), crs = st_crs(bangladesh_2))

# join each point to the district polygon it falls within
points_with_district <- st_join(ea_crs, bangladesh_2)

# There is one that was NA
nrow(subset(points_with_district, is.na(ADM2_EN))) # 1
subset(points_with_district, is.na(ADM2_EN))
subset(ea_crs, DHSCLUST %in% c(80))
# Fill in these two manually based on nearby clusters
subset(points_with_district, DHSCLUST %in% c(80))
subset(points_with_district, DHSCLUST %in% c(78,79,80,81,82))
points_with_district$ADM1_EN[is.na(points_with_district$ADM2_EN) & 
                               points_with_district$DHSCLUST %in% c(80)] <- "Chittagong"
points_with_district$ADM2_EN[is.na(points_with_district$ADM2_EN) & 
                               points_with_district$DHSCLUST %in% c(80)] <- "Chandpur"

df <- data.frame(DHSCLUSTER = as.numeric(as.character(points_with_district$DHSCLUST)),
                 ADM2_EN = as.character(points_with_district$ADM2_EN),
                 ADM1_EN = as.character(points_with_district$ADM1_EN))
nrow(subset(df, is.na(ADM2_EN))) # 0 
nrow(subset(df, is.na(ADM1_EN))) # 0

# Save --------------------------------------------------------------------

write.csv(df, "./gen/prepare-shp/output/cluster-locations-2017.csv", row.names = FALSE)
