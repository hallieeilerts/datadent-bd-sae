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
ea <- st_read("./data/BD_2022_DHS_03042025_2045_120781/BDGE81FL", layer = "BDGE81FL")
# Bangladesh district boundaries
bangladesh_2 <- st_read("./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm2_bbs_20201113")
################################################################################

# convert to sf object with same CRS as shapefile
ea_crs <- st_as_sf(ea, coords = c("longitude", "latitude"), crs = st_crs(bangladesh_2))

# join each point to the district polygon it falls within
points_with_district <- st_join(ea_crs, bangladesh_2)

df <- data.frame(DHSCLUSTER = as.numeric(as.character(points_with_district$DHSCLUST)),
                 ADM2_EN = as.character(points_with_district$ADM2_EN),
                 ADM1_EN = as.character(points_with_district$ADM1_EN))

# Save --------------------------------------------------------------------

write.csv(df, "./gen/prepare-shp/output/cluster-locations.csv", row.names = FALSE)


