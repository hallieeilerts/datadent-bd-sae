################################################################################
#' @description Creates fancy tm map for one model/indicator at a time
#' @return One map per model/indicator
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(sf)
library(dplyr)
library(tidyr)
library(wesanderson)
library(ggplot2)
library(tmap)
#' Inputs
source("./src/util.R")
# Bangladesh district boundaries
bangladesh_2 <- st_read("./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm2_bbs_20201113")
# direct estiamtes
est <- read.csv("./gen/calculate-direct/output/direct-estimates.csv")
################################################################################

# choose indicator
unique(est$variable)
# "nt_ch_micro_vas"  "nt_ch_micro_dwm"  "nt_ebf" "nt_wm_micro_iron" "rh_anc_4vs" "rh_anc_1tri" 
outcome <- "nt_wm_micro_iron"

# posterior predictions for all models for this indicator
filenames <- list.files("./gen/model/output/")
filenames <- filenames[grepl("postpred", filenames, ignore.case = TRUE)]
filenames <- filenames[grepl(outcome, filenames)]

# load most recent
filename <- tail(sort(filenames), 1)
model <- str_match(filename, "-([0-9]+-[^-.]+)-[0-9]{8}\\.csv$")[,2]
postpred <- read.csv(paste0("./gen/model/output/",filename))

# join spatial data
postpred_sf <- bangladesh_2 %>% 
  left_join(postpred, by = "ADM2_EN")


# Prevalence --------------------------------------------------------------

plot_map <- function(data, col, title, metric) {
  
  # Create a map: 
  map <- tm_shape(data) + 
    tm_fill(col = col,
            title = metric, 
            style = "cont",
            breaks = seq(0, 90, by = 10),
            textNA = "Missing Data",
            legend.is.portrait = F,
            palette = wesanderson::wes_palette("Zissou1Continuous")) + 
    tm_layout(main.title = title, frame = F, main.title.size = 0.8, 
              main.title.position = "center", legend.outside.position = "bottom",
              legend.outside.size = 0.35) +
    tm_borders(lwd = 0) + 
    tm_legend(show = T) +
    tm_shape(bangladesh_2) +
    tm_borders(col = "black", lwd = 0.8)
  
  return(map)
}

# direct_map

direct_map <- plot_map(data = postpred_sf , 
                       col = "dir", 
                       title = paste0("Direct estimates - ", outcome), 
                       metric = "%")
tmap_save(direct_map, paste0("./gen/visualizations/tmap/", outcome, "-direct.png"), width = 8, height = 8,
          units = "in", dpi = 600)

# FB_map

FB_map <- plot_map(data = postpred_sf ,
                   col = "post_mean", 
                   title = paste0("Full-Bayes BYM2 smoothed estimates - ", outcome,", ", model), 
                   metric = "(%)")

tmap_save(FB_map, paste0("./gen/visualizations/tmap/", outcome,"-", model, "-FB.png"), width = 8, height = 8,
          units = "in", dpi = 600)


# Variance ----------------------------------------------------------------

plot_map <- function(data, col, title, metric, level) {
  
  # Create a map: 
  map <- tm_shape(data) + 
    tm_fill(col = col,
            title = metric, 
            style = "cont",
            breaks = seq(0, 0.035, by = 0.01),
            textNA = "Missing Data",
            legend.is.portrait = F,
            palette = viridis::viridis(10)) + 
    tm_layout(main.title = title, frame = F, main.title.size = 0.8, 
              main.title.position = "center", legend.outside.position = "bottom",
              legend.outside.size = 0.35) +
    tm_borders(lwd = 0) + 
    tm_legend(show = T) +
    tm_shape(bangladesh_2) +
    tm_borders(col = "black", lwd = 0.8)
  
  return(map)
}

# variance of direct estimates

var_direct <- plot_map(data = postpred_sf , 
                       col = "dir_var", 
                       title = paste0("Variance of direct estimates - ", outcome), 
                       metric = "(%)")
tmap_save(var_direct, paste0("./gen/visualizations/tmap/", outcome, "-direct-var.png"), width = 8, height = 8,
          units = "in", dpi = 600)

# variance of smoothed estimates

var_smoothed_FB <- plot_map(data = postpred_sf, 
                            col = "post_var", 
                            title = paste0("Variance of smoothed estimates - ", outcome, ", ", model), 
                            metric = "Variance")

tmap_save(var_smoothed_FB, paste0("./gen/visualizations/tmap/", outcome, "-FB-BYM2-var.png"), width = 8, height = 8,
          units = "in", dpi = 600)
