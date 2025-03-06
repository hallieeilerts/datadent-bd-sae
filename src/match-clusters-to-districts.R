################################################################################
#' @description Match GPS coordinates of clusters to BD adm2 districts
#' @return spreadsheet with columns for cluster number and district name
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(rgdal)
#' Inputs
source("./src/util.R")
# Location masked of DHS clusters
EAPoints <- readOGR(dsn="./data/BD_2022_DHS_03042025_2045_120781/BDGE81FL",layer="BDGE81FL")
# Bangladesh district boundaries
DP <- readOGR(dsn="./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm2_bbs_20201113")
################################################################################

proj4string(EAPoints)

# Make Projections consistent
EAPoints <- spTransform(EAPoints, CRS("+proj=utm +zone=36 +datum=WGS84"))
DP <- spTransform(DP, CRS("+proj=utm +zone=36 +datum=WGS84"))

# Number of clusters/EAs
nEA <- dim(EAPoints)[[1]]

#Determining Polygon of Observed Point
#for(i in 1:nEA) {
ov_point <- as.data.frame(over(EAPoints, DP))
dim(ov_point)
ov_point
given_dis <- as.character(ov_point$ADM2_EN)
given_dis

head(EAPoints)
EAOut <- as.data.frame(EAPoints)
EAOut$Mapped_district <- given_dis
as.character(EAOut$Mapped_district)

EAOut <- EAOut[,c("DHSCLUST","Mapped_district")]
temp <- data.frame(  DHSCLUSTER = as.numeric(as.character(EAOut$DHSCLUST)),
                     district = as.character(EAOut$Mapped_district))

#write.csv(temp, "./gen/BDGE81FL_C.csv", row.names = FALSE)
write.csv(temp, "./gen/cluster-districts.csv", row.names = FALSE)


