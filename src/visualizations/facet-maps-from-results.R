################################################################################
#' @description Creates faceted map of prevalence and variance for an indicator
#' loads from results file (model with minimum error already chosen)
#' has grey for high CV
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
library(ggpattern)
#' Inputs
source("./src/util.R")
# Bangladesh division boundaries
bangladesh_1 <- st_read("./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm1_bbs_20201113")
# Bangladesh district boundaries
bangladesh_2 <- st_read("./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm2_bbs_20201113")
ind <- read_excel("./data/ind-info.xlsx", sheet = "indicators")
info <- read.csv("./gen/model/audit/model-info.csv")
# results
filenames <- list.files("./gen/results/output/")
filenames <- filenames[grepl("mod103", filenames)]
filenames <- filenames[!grepl("wide", filenames)]
filename <- tail(sort(filenames), 1)
res <- read.csv(paste0("./gen/results/output/", filename))
################################################################################

# subset to included indicators
df_ind <- subset(ind, status == "include")

# vector of indicators
v_var <- unique(df_ind$variable)
v_varlong <- df_ind$description

v_var[v_var %in% unique(subset(res, cv > 30)$variable)]
# [1] "rh_anc_bldsamp"   "rh_anc_toxinj"    "nt_ch_micro_dwm"  "nt_ch_micro_mp"   "nt_ch_gwmt_any"   "ch_diar_ors"      "rh_del_inst"     
# [8] "ph_wtr_trt_appr"  "nt_wm_ppvita"     "nt_ch_micro_iron"

for(i in 1:length(v_var)){

  myoutcome <- v_var[i]
  myoutlab <- subset(df_ind, variable == myoutcome)$description
  
  # reshape direct and modeled estimates long
  dat_dir <- res %>%
    filter(variable %in% myoutcome & admin_level == "adm2") %>%
    select(variable, description, ADM2_EN, dir, dir_se) %>%
    mutate(var = dir_se^2) %>%
    select(-dir_se) %>%
    rename(value = dir) %>%
    mutate(name = "Direct",
           cv = NA) # no need to plot a cv for direct estimate
  dat_mod <- res %>%
    filter(variable %in% myoutcome & admin_level == "adm2") %>%
    select(variable, description, ADM2_EN, r, se, cv) %>%
    mutate(var = se^2) %>%
    select(-se) %>%
    rename(value = r) %>%
    mutate(name = "Modeled")
  dat_plot <- rbind(dat_dir, dat_mod)
  
  # Join spatial data
  postpred_sf <- bangladesh_2 %>% 
    left_join(dat_plot, by = "ADM2_EN") %>%
    mutate(name = factor(name, levels = c("Direct", "Modeled")))
  
  p1 <- ggplot() +
    geom_sf(data = postpred_sf, aes(fill = value), color = NA) +
    geom_sf_pattern(data = subset(postpred_sf, cv > 30), aes(geometry = geometry), 
                    fill = NA, pattern = "stripe", pattern_color = "grey50",
                    pattern_density = 0.02, pattern_spacing = 0.01, pattern_size = 0.2) +
    geom_sf(data = bangladesh_2, color = "black", fill = NA) +
    scale_fill_viridis_c(
      option = "D",
      limits = c(0, max(postpred_sf$value, na.rm = TRUE)),
      direction = -1, # reverse scale
      na.value = "grey80",
      name = ""
    ) +
    labs(title = myoutlab, subtitle = "Prevalence") +
    facet_wrap(~name) +
    theme_void() +
    theme(
      text = element_text(size = 10),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  ggsave(paste0("./gen/visualizations/facet-maps-cv/",myoutcome,"-prev.png"), p1, width = 10, height = 5.5)
  
  p2 <- ggplot() +
    geom_sf(data = postpred_sf, aes(fill = var), color = NA) +
    geom_sf(data = bangladesh_2, color = "black", fill = NA) +
    geom_sf_pattern(data = subset(postpred_sf, cv > 30), aes(geometry = geometry), 
                    fill = NA, pattern = "stripe", pattern_color = "grey50",
                    pattern_density = 0.01, pattern_spacing = 0.01, pattern_size = 0.2) +
    scale_fill_viridis_c(
      option = "D", 
      limits = c(0, max(postpred_sf$var, na.rm = TRUE)),
      direction = -1, # reverse scale
      na.value = "grey80",
      name = ""
    ) +
    labs(title = myoutlab, subtitle = "Variance") +
    facet_wrap(~name) +
    theme_void() +
    theme(
      text = element_text(size = 10),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  ggsave(paste0("./gen/visualizations/facet-maps-cv/",myoutcome,"-var.png"), p2, width = 10, height = 5.5)

}

# guidance note -----------------------------------------------------------

# subset to included indicators
df_ind <- subset(ind, status == "include")

# vector of indicators
v_var <- c("ch_diar_ors","nt_ch_micro_vas")
v_varlong <- c("Children under 24m with diarrhea who received ORS","Children 6-23m given vitamin A supplements")

v_var[v_var %in% unique(subset(res, cv > 30)$variable)]
# [1] "rh_anc_bldsamp"   "rh_anc_toxinj"    "nt_ch_micro_dwm"  "nt_ch_micro_mp"   "nt_ch_gwmt_any"   "ch_diar_ors"      "rh_del_inst"     
# [8] "ph_wtr_trt_appr"  "nt_wm_ppvita"     "nt_ch_micro_iron"

for(i in 1:length(v_var)){
  
  myoutcome <- v_var[i]
  myoutlab <- v_varlong[i]
  
  # reshape direct and modeled estimates long
  dat_dir <- res %>%
    filter(variable %in% myoutcome & admin_level == "adm2") %>%
    select(variable, description, ADM2_EN, dir, dir_se) %>%
    mutate(var = dir_se^2) %>%
    select(-dir_se) %>%
    rename(value = dir) %>%
    mutate(name = "Direct",
           cv = NA) # no need to plot a cv for direct estimate
  dat_mod <- res %>%
    filter(variable %in% myoutcome & admin_level == "adm2") %>%
    select(variable, description, ADM2_EN, r, se, cv) %>%
    mutate(var = se^2) %>%
    select(-se) %>%
    rename(value = r) %>%
    mutate(name = "Modeled")
  dat_plot <- rbind(dat_dir, dat_mod)
  
  # Join spatial data
  postpred_sf <- bangladesh_2 %>% 
    left_join(dat_plot, by = "ADM2_EN") %>%
    mutate(name = factor(name, levels = c("Direct", "Modeled")))
  
  p1 <- ggplot() +
    geom_sf(data = postpred_sf, aes(fill = value), color = NA) +
    geom_sf_pattern(data = subset(postpred_sf, cv > 30), aes(geometry = geometry), 
                    fill = NA, pattern = "stripe", pattern_color = "grey70",
                    pattern_density = 0.02, pattern_spacing = 0.01, pattern_size = 0.2) +
    geom_sf(data = bangladesh_2, color = "black", fill = NA) +
    scale_fill_viridis_c(
      option = "D",
      limits = c(0, max(postpred_sf$value, na.rm = TRUE)),
      direction = -1, # reverse scale
      na.value = "grey80",
      name = ""
    ) +
    labs(title = myoutlab, subtitle = "Prevalence") +
    facet_wrap(~name) +
    theme_void() +
    theme(
      text = element_text(size = 10),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  ggsave(paste0("./gen/visualizations/facet-maps-cv-guidance-note/",myoutcome,"-prev.png"), p1, width = 8, height = 4.5)
  
  p2 <- ggplot() +
    geom_sf(data = postpred_sf, aes(fill = var), color = NA) +
    geom_sf(data = bangladesh_2, color = "black", fill = NA) +
    geom_sf_pattern(data = subset(postpred_sf, cv > 30), aes(geometry = geometry), 
                    fill = NA, pattern = "stripe", pattern_color = "grey80",
                    pattern_density = 0.01, pattern_spacing = 0.01, pattern_size = 0.2) +
    scale_fill_viridis_c(
      option = "D", 
      limits = c(0, max(postpred_sf$var, na.rm = TRUE)),
      direction = -1, # reverse scale
      na.value = "grey80",
      name = ""
    ) +
    labs(title = myoutlab, subtitle = "Variance") +
    facet_wrap(~name) +
    theme_void() +
    theme(
      text = element_text(size = 10),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  ggsave(paste0("./gen/visualizations/facet-maps-cv-guidance-note/",myoutcome,"-var.png"), p2, width = 8, height = 4.5)
  
}

