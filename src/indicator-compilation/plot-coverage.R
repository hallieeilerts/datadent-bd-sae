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
library(viridis)
#' Inputs
source("./src/util.R")
# Bangladesh district boundaries
AD1 <- readOGR(dsn="./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm1_bbs_20201113")
AD2 <- readOGR(dsn="./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm2_bbs_20201113")
# Manually calculated indicators
dat_filename <- list.files("./gen/indicator-compilation/output")
dat_filename <- dat_filename[grepl("indicators_data", dat_filename, ignore.case = TRUE)]
dat_filename <- tail(sort(dat_filename), 1)
dat <- read.csv(paste0("./gen/indicator-compilation/output/", dat_filename, sep = ""))
################################################################################

# Convert AD to an sf object
AD1 <- st_as_sf(AD1)
AD2 <- st_as_sf(AD2)
v_ind <- unique(dat$indicator)

# admin 2
for(i in 1:length(v_ind)){
  
  dat_ind <- subset(dat, indicator == v_ind[i] & admin_level == "adm2")[,c("district_name", "value")]
  names(dat_ind)[which(names(dat_ind) == "district_name")] <- "ADM2_EN"
  
  # Merge with AD2 based on ADM2_EN
  AD2_merged <- left_join(AD2, dat_ind, by = "ADM2_EN")
  
  p1 <- ggplot() +
    geom_sf(data = AD2_merged, aes(fill = value), color = "black") +
    scale_fill_viridis_c(option = "magma", na.value = "gray80") + 
    theme_bw() +
    labs(title = v_ind[i])
  
  ggsave(paste0("./gen/indicator-compilation/output/adm2-",v_ind[i],".png"), p1, width = 10, height = 7, dpi = 300)
  
}

# admin 1
for(i in 1:length(v_ind)){
  
  dat_ind <- subset(dat, indicator == v_ind[i] & admin_level == "adm1")[,c("region_name", "value")]
  names(dat_ind)[which(names(dat_ind) == "region_name")] <- "ADM1_EN"
  
  # Merge with AD1 based on ADM1_EN
  AD1_merged <- left_join(AD1, dat_ind, by = "ADM1_EN")
  
  p1 <- ggplot() +
    geom_sf(data = AD1_merged, aes(fill = value), color = "black") +
    scale_fill_viridis_c(option = "magma", na.value = "gray80") + 
    theme_bw() +
    labs(title = v_ind[i])
  
  ggsave(paste0("./gen/indicator-compilation/output/adm1-",v_ind[i],".png"), p1, width = 10, height = 7, dpi = 300)
  
}


