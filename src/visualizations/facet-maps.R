################################################################################
#' @description Creates faceted maps for multiple models for same indicator
#' @return One faceted map for prevalence, one for variance
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
outcome <- "rh_anc_1tri"

# posterior predictions for all models for this indicator
filenames <- list.files("./gen/model/output/")
filenames <- filenames[grepl("postpred", filenames, ignore.case = TRUE)]
filenames <- filenames[grepl(outcome, filenames)]

# save posterior prediction for first model in df
filename <- filenames[1]
model <- str_match(filename, "-([0-9]+-[^-.]+)-[0-9]{8}\\.csv$")[,2]
postpred <- read.csv(paste0("./gen/model/output/",filename))
names(postpred)[which(names(postpred) == "post_mean")] <- paste0(model, "_mean")
names(postpred)[which(names(postpred) == "post_var")] <- paste0(model, "_var")

# merge on posterior predictions for subsequent models
if(length(filenames) > 1){
  # if more than one version of this model, read in and merge on
  for(i in 2:length(filenames)){
    
    filename <- filenames[i]
    # Extract the part between the second-to-last and third-to-last hyphen
    model <- str_match(filename, "-([0-9]+-[^-.]+)-[0-9]{8}\\.csv$")[,2]
    postpred_mult <- read.csv(paste0("./gen/model/output/",filename))
    postpred_mult <- postpred_mult[,c("ADM2_EN", "post_mean", "post_var")]
    names(postpred_mult)[which(names(postpred_mult) == "post_mean")] <- paste0(model, "_mean")
    names(postpred_mult)[which(names(postpred_mult) == "post_var")] <- paste0(model, "_var")
    postpred <- merge(postpred, postpred_mult, by = "ADM2_EN")
  }
}

# Faceted plots -----------------------------------------------------------

# Join spatial data
postpred_sf <- bangladesh_2 %>% 
  left_join(postpred, by = "ADM2_EN")

# plot prevalence

postpred_long <- postpred_sf %>%
  pivot_longer(
    cols = c("dir" | starts_with("00") & ends_with("mean")),
    names_to = "plotlabel",
    values_to = "value"
  ) %>%
  mutate(plotlabel = ifelse(plotlabel == "dir", "direct", plotlabel)) %>%
  mutate(plotlabel = gsub("_mean", "", plotlabel))

v_plots <- unique(postpred_long$plotlabel)
postpred_long$plotlabel <- factor(postpred_long$plotlabel, levels = v_plots)

p1 <- ggplot() +
  geom_sf(data = postpred_long, aes(fill = value), color = "black") +
  scale_fill_gradientn(
    colors = wes_palette("Zissou1", 100, type = "continuous"),
    limits = c(0, max(postpred_long$value, na.rm = TRUE)),  # adjust as needed
    na.value = "grey80"
  ) +
  labs(title = outcome, subtitle = "prevalence") +
  facet_wrap(~plotlabel) +
  theme_bw()
ggsave(paste0("./gen/visualizations/facet-maps/",outcome,"-prev.png"), p1, width = 8, height = 4)

# variance

postpred_long <- postpred_sf %>%
  pivot_longer(
    cols = c("dir_var" | starts_with("00") & ends_with("var")),
    names_to = "plotlabel",
    values_to = "value"
  ) %>%
  mutate(plotlabel = ifelse(plotlabel == "dir", "direct", plotlabel)) %>%
  mutate(plotlabel = gsub("_var", "", plotlabel))
v_plots <- unique(postpred_long$plotlabel)
postpred_long$plotlabel <- factor(postpred_long$plotlabel, levels = v_plots)
p2 <- ggplot() +
  geom_sf(data = postpred_long, aes(fill = value), color = "black") +
  scale_fill_gradientn(
    colors = wes_palette("Zissou1", 100, type = "continuous"),
    limits = c(0, max(postpred_long$value, na.rm = TRUE)),  # adjust as needed
    na.value = "grey80"
  ) +
  labs(title = outcome, subtitle = "variance") +
  facet_wrap(~plotlabel) +
  theme_bw()
ggsave(paste0("./gen/visualizations/facet-maps/", outcome,"-var.png"), p2, width = 8, height = 4)


