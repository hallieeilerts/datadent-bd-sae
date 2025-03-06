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
vas <- read.csv("./gen/ch_vas_data.csv")
################################################################################

# Convert AD2 to an sf object
AD2 <- st_as_sf(AD2)

vas.val <- subset(vas, admin_level == "adm2")[,c("district_name", "value")]
names(vas.val)[which(names(vas.val) == "district_name")] <- "ADM2_EN"

# Merge with AD2 based on ADM2_EN
AD2_merged <- left_join(AD2, vas.val, by = "ADM2_EN")

p1 <- ggplot() +
  geom_sf(data = AD2_merged, aes(fill = value), color = "black") +
  scale_fill_viridis_c(option = "magma", na.value = "gray80") +  # Optional color scale
  theme_bw() +
  labs(title = "Child vitamin A supplementation", fill = "Value")
ggsave("./gen/adm2_chvas.png", p1, width = 10, height = 7, dpi = 300)

