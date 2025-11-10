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


# facet maps for guidance note --------------------------------------------------------------

# subset to included indicators
df_ind <- subset(ind, status == "include")

# vector of indicators
v_var <- unique(df_ind$variable)
v_varlong <- df_ind$description

v_var[v_var %in% unique(subset(res, cv > 30)$variable)]
# [1] "rh_anc_bldsamp"   "rh_anc_toxinj"    "nt_ch_micro_dwm"  "nt_ch_micro_mp"   "nt_ch_gwmt_any"   "ch_diar_ors"      "rh_del_inst"     
# [8] "ph_wtr_trt_appr"  "nt_wm_ppvita"     "nt_ch_micro_iron"

# example indicators for guidance note
which(v_var == "ch_diar_ors") # 18
which(v_var == "nt_ch_micro_vas") # 11
#v_var <- c("ch_diar_ors","nt_ch_micro_vas")
#v_varlong <- c("Children under 24m with diarrhea who received ORS","Children 6-23m given vitamin A supplements")
v_varlong[18] <- "Children under 24m with diarrhea who received ORS"
v_varlong[11] <- "Children 6-23m given vitamin A supplements"

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
  ggsave(paste0("./gen/visualizations/guidance-note/maps/",myoutcome,"-prev.png"), p1, width = 8, height = 4.5)
  
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
  ggsave(paste0("./gen/visualizations/guidance-note/maps/",myoutcome,"-var.png"), p2, width = 8, height = 4.5)
  
}


# table with unreliability of estimates -----------------------------------

# number of indicators with any unreliable estimates
res %>%
  filter(admin_level == "adm2") %>%
  filter(cv > 30) %>%
  select(variable) %>% pull() %>% unique() %>% length()
# 19 have some unreliable

# percentage of unreliable districts by indicator
res %>%
  filter(admin_level == "adm2") %>%
  mutate(unreliable = ifelse(cv > 30, 1, 0)) %>%
  group_by(variable, model, description, unreliable) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total) %>%
  filter(unreliable == 1) %>%
  arrange(-per)

# average per among those with any unreliable
res %>%
  filter(admin_level == "adm2") %>%
  mutate(unreliable = ifelse(cv > 30, 1, 0)) %>%
  group_by(variable, model, description, unreliable) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n/total) %>%
  filter(unreliable == 1) %>%
  ungroup() %>%
  summarise(med = median(n),
            medper = median(per),
            avgper = mean(per))
# median 5
# median unreliable districts is 7.8%
# average is 20.6%

# average unreliable districts among all indicators
res %>%
  filter(admin_level == "adm2") %>%
  mutate(unreliable = ifelse(cv > 30, 1, 0)) %>%
  group_by(variable, model, description) %>%
  summarise(n = sum(unreliable),
            per = sum(unreliable)/n()) %>%
  ungroup() %>%
  summarise(med = median(per),
            avg = mean(per))
# median 1.56%
# average 12.6%


# uncertainty interval figures for guidance note ---------------------------------------------------

# subset to included indicators
df_ind <- subset(ind, status == "include")

# vector of indicators
v_var <- unique(df_ind$variable)
v_varlong <- df_ind$description

v_var[v_var %in% unique(subset(res, cv > 30)$variable)]
# [1] "rh_anc_bldsamp"   "rh_anc_toxinj"    "nt_ch_micro_dwm"  "nt_ch_micro_mp"   "nt_ch_gwmt_any"   "ch_diar_ors"      "rh_del_inst"     
# [8] "ph_wtr_trt_appr"  "nt_wm_ppvita"     "nt_ch_micro_iron"

# vector of indicators for guidance note
#v_var <- c("ch_diar_ors","nt_ch_micro_vas")
#v_varlong <- c("Children under 24m with diarrhea who received ORS","Children 6-23m given vitamin A supplements")
v_varlong[18] <- "Children under 24m with diarrhea who received ORS"
v_varlong[11] <- "Children 6-23m given vitamin A supplements"


for(i in 1:length(v_var)){
  
  myoutcome <- v_var[i]
  myoutlab <- v_varlong[i]

  # select indicator
  plotDat <- res %>%
    filter(variable == myoutcome)
  
  # reshape direct and modeled long
  
  # direct
  # if dir_se is NA and un is > 0, fill in dir_se as 0
  # this way the lb and ub will also be 0 instead of NA. and the zero prevalence will still be plotted.
  # otherwise, districts with zero observations and zero prevalence both are not plotted.
  plotDir <- plotDat %>%
    filter(admin_level == "adm2") %>%
    select(variable, description, ADM2_EN, dir, dir_se, un) %>%
    mutate(dir_se = ifelse(un > 0 & is.na(dir_se), 0 , dir_se)) %>%
    rename(value = dir) %>%
    mutate(lb = value - 1.96*dir_se,
           ub = value + 1.96*dir_se) %>%
    mutate(lb = ifelse(lb < 0, 0, lb),
           ub = ifelse(ub > 100, 100, ub),
           name = "Direct")
  # modeled
  plotMod <- plotDat %>%
    filter(admin_level == "adm2") %>%
    select(variable, description, ADM2_EN, r, ll, ul, un) %>%
    rename(value = r,
           lb = ll,
           ub = ul) %>%
    mutate(name = "Modeled")
  plotDatLong <- plotDir %>%
    bind_rows(plotMod) %>%
    mutate(name = factor(name, levels = c( "Modeled", "Direct")))
  
  # split into two panels
  df_panels <- plotDatLong %>%
    dplyr::select(ADM2_EN) %>%
    distinct() %>% arrange(ADM2_EN) %>%
    mutate(n = 1:n(),
           total = n(),
           panel = ifelse(n/total<0.5, 1, 2))

  plot1 <- plotDatLong %>% 
    filter(ADM2_EN %in% subset(df_panels, panel == 1)$ADM2_EN) 
  plot2 <- plotDatLong %>% 
    filter(ADM2_EN %in% subset(df_panels, panel == 2)$ADM2_EN) 

  p1 <- plot1 %>%
    ggplot(aes(x = ADM2_EN, y = value, color = name, group = name)) +
    geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.5), width = 0.2) +
    geom_point(position = position_dodge(width = 0.5)) +
    scale_color_manual(values = c( "#00BFC4", "grey38"), guide = guide_legend(reverse = TRUE)) +
    labs(x = "Adm2 district", y = "Prevalence") +
    theme_bw() +
    coord_flip() +
    theme(text = element_text(size = 10), legend.title=element_blank()) 
  p2 <- plot2 %>%
    ggplot(aes(x = ADM2_EN, y = value, color = name, group = name)) +
    geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.5), width = 0.2) +
    geom_point(position = position_dodge(width = 0.5)) +
    scale_color_manual(values = c( "#00BFC4", "grey38"), guide = guide_legend(reverse = TRUE)) +
    labs(x = "", y = "Prevalence") +
    theme_bw() +
    coord_flip() +
    theme(text = element_text(size = 10), legend.title=element_blank()) +
    scale_shape_manual(values = c(16, 8)) 
  # Combine p1 and p2
  combined_plot <- (p1 + p2) + 
    plot_layout(guides = "collect") &   # collect shared legend
    theme(legend.position = "bottom",
          legend.margin = margin(t = -5, b = 0),   # tighten legend spacing
          legend.box.margin = margin(t = -5)       # reduce gap between plot and legend
    )
  # Add single title on top
  combined_plot  <- combined_plot + plot_annotation(title = paste0(myoutlab))
  
  ggsave(paste0("./gen/visualizations/guidance-note/uncert-int/", myoutcome,".png"), 
         combined_plot, width = 8, height = 4, units = "in", dpi = 500)

}

# ch_diar_ors
# 13 districts with zero observations
res %>%
  filter(variable %in% "ch_diar_ors" & admin_level == "adm2" & un == 0) %>%
  select(ADM2_EN, dir, dir_se, un) %>%
  arrange(ADM2_EN)
# 28 districts with no variance
res %>%
  filter(variable %in% "ch_diar_ors" & admin_level == "adm2" & 
           un != 0 & (is.na(dir_se) | dir_se == 0)) %>%
  select(ADM2_EN, dir, dir_se, un) %>%
  arrange(ADM2_EN)
# range of uncertainty
res %>%
  filter(variable %in% "ch_diar_ors" & admin_level == "adm2" &
           dir_se != 0 & !is.na(dir_se)) %>%
  mutate(ll = dir - 1.96*dir_se,
         ul = dir + 1.96*dir_se) %>%
  mutate(ll = ifelse(ll < 0, 0, ll),
         ul = ifelse(ul > 100, 100, ul)) %>%
  mutate(range = ul-ll) %>%
  arrange(-range) %>%
  mutate(rangethresh = ifelse(range > 50, 1, 0)) %>%
  summarise(sum(rangethresh)/n())

# nt_ch_micro_vas
# 0 districts with no observations
res %>%
  filter(variable %in% "nt_ch_micro_vas" & admin_level == "adm2" & un == 0) %>%
  select(ADM2_EN, dir, dir_se, un) %>%
  arrange(ADM2_EN)
# one district with no uncertainty
res %>%
  filter(variable %in% "nt_ch_micro_vas" & admin_level == "adm2" & 
           un != 0 & (is.na(dir_se) | dir_se == 0)) %>%
  select(ADM2_EN, dir, dir_se, un) %>%
  arrange(ADM2_EN)
# cases where uncertainty is narrower for model
res %>%
  filter(variable %in% "nt_ch_micro_vas" & admin_level == "adm2" & 
           !(un != 0 & (is.na(dir_se) | dir_se == 0))) %>%
  mutate(lb_dir = dir - 1.96*dir_se,
         ub_dir = dir + 1.96*dir_se) %>%
  mutate(lb_dir = ifelse(lb_dir < 0, 0, lb_dir),
         ub_dir = ifelse(ub_dir > 100, 100, ub_dir)) %>%
  mutate(range_dir = ub_dir-lb_dir,
         range_mod = ul-ll) %>%
  mutate(modnarrower = ifelse(range_mod < range_dir, 1, 0)) %>%
  group_by(modnarrower) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         per = n / total)
# interquartile range of Direct estimates
res %>%
  filter(variable %in% "nt_ch_micro_vas" & admin_level == "adm2") %>%
  summarise(IQR(dir))
res %>%
  filter(variable %in% "nt_ch_micro_vas" & admin_level == "adm2") %>%
  summarise(IQR(r))
# highest Direct estimates
res %>%
  filter(variable %in% "nt_ch_micro_vas" & admin_level == "adm2") %>%
  arrange(-dir)
# lowest Direct estimates
res %>%
  filter(variable %in% "nt_ch_micro_vas" & admin_level == "adm2") %>%
  arrange(dir)

