################################################################################
#' @description Rename shape file adm1 regions to match DHS
#' @return new shape files with regions matching DHS
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(sf)
#' Inputs
source("./src/util.R")
# Bangladesh region boundaries
DP <- readOGR(dsn="./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm1_bbs_20201113")

bangladesh_2 <- st_read("./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm1_bbs_20201113")
################################################################################

DP@data$ADM1_EN[DP@data$ADM1_EN == "Barisal"] <- "Barishal"
DP@data$ADM1_EN[DP@data$ADM1_EN == "Chittagong"] <- "Chattogram"

# Save --------------------------------------------------------------------

writeOGR(DP, dsn = "./gen/prepare-shp/output/modified_bgd_adm1", layer = "bgd_admbnda_adm1_bbs_20201113_modified",
         driver = "ESRI Shapefile", overwrite_layer = TRUE)
