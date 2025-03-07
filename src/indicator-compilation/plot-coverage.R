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
library(gridExtra)
library(viridis)
#' Inputs
source("./src/util.R")
# Bangladesh district boundaries
AD1 <- readOGR(dsn="./gen/prepare-shp/output/modified_bgd_adm1", layer = "bgd_admbnda_adm1_bbs_20201113_modified")
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

# adm1
# open a PDF file to save multiple pages
pdf("./gen/indicator-compilation/output/adm1-plots.pdf", width = 10, height = 8)
for(i in 1:length(v_ind)){
  
  # subset indicator of interest
  dat_ind <- subset(dat, indicator == v_ind[i] & admin_level == "adm1")
  names(dat_ind)[which(names(dat_ind) == "region_name")] <- "ADM1_EN"
  # create confidence interval
  dat_ind$value_lb <- dat_ind$value - 1.96 * dat_ind$se
  dat_ind$value_ub <- dat_ind$value + 1.96 * dat_ind$se
  
  # plot 1: map
  # merge with AD2 based on ADM2_EN
  AD1_merged <- left_join(AD1, dat_ind, by = "ADM1_EN")
  # create map
  p1 <- ggplot() +
    geom_sf(data = AD1_merged, aes(fill = value), color = "black") +
    scale_fill_viridis_c(option = "magma", na.value = "gray80") + 
    theme_bw() +
    labs(title = v_ind[i])
  
  # plot2: estimates with ci
  p2 <- ggplot(dat_ind, aes(x = ADM1_EN, y = value)) +
    geom_errorbar(aes(ymin = value_lb, ymax = value_ub), width = 0.2) +  # Error bars
    geom_point(aes(color=value), size = 3) +  # Plot estimate points
    labs(title = "", x = "", y = "") +
    scale_color_viridis_c(option = "magma", na.value = "gray80",guide = "none") + 
    theme_bw() +
    coord_flip()
  
  # arrange the two plots side by side
  grid.arrange(p1, p2, ncol = 2)
}
dev.off()


# adm2
# open a PDF file to save multiple pages
pdf("./gen/indicator-compilation/output/adm2-plots.pdf", width = 10, height = 8)
for(i in 1:length(v_ind)){
  
  # subset indicator of interest
  dat_ind <- subset(dat, indicator == v_ind[i] & admin_level == "adm2")
  names(dat_ind)[which(names(dat_ind) == "district_name")] <- "ADM2_EN"
  # create confidence interval
  dat_ind$value_lb <- dat_ind$value - 1.96 * dat_ind$se
  dat_ind$value_ub <- dat_ind$value + 1.96 * dat_ind$se
  
  # plot 1: map
  # merge with AD2 based on ADM2_EN
  AD2_merged <- left_join(AD2, dat_ind, by = "ADM2_EN")
  # create map
  p1 <- ggplot() +
    geom_sf(data = AD2_merged, aes(fill = value), color = "black") +
    scale_fill_viridis_c(option = "magma", na.value = "gray80") + 
    theme_bw() +
    labs(title = v_ind[i])
  
  # plot2: estimates with ci
  p2 <- ggplot(dat_ind, aes(x = ADM2_EN, y = value)) +
    geom_errorbar(aes(ymin = value_lb, ymax = value_ub), width = 0.2) +  # Error bars
    geom_point(aes(color=value), size = 3) +  # Plot estimate points
    labs(title = "", x = "", y = "") +
    scale_color_viridis_c(option = "magma", na.value = "gray80",guide = "none") + 
    theme_bw() +
    coord_flip()
  
  # arrange the two plots side by side
  grid.arrange(p1, p2, ncol = 2)
}
dev.off()


