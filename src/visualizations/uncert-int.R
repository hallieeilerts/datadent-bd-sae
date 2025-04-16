################################################################################
#' @description Creates uncertainty interval plots for all models for one indicator
#' @return One plot with uncertainty intervals for all models for one indicator
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

postpred_value <- postpred %>%
  pivot_longer(
    cols = c("dir" | starts_with("00") & ends_with("mean")),
    names_to = "name",
    values_to = "value"
  ) %>%
  mutate(name = ifelse(name == "dir", "direct", name)) %>%
  mutate(name = gsub("_mean", "", name)) %>%
  select(ADM2_EN, name, value)

postpred_var <- postpred %>%
  pivot_longer(
    cols = c("dir_var" | starts_with("00") & ends_with("var")),
    names_to = "name",
    values_to = "var"
  ) %>%
  mutate(name = ifelse(name == "dir_var", "direct", name)) %>%
  mutate(name = gsub("_var", "", name)) %>%
  select(ADM2_EN, name, var)

postpred_plot <- postpred_value %>%
  inner_join(postpred_var, by = c("ADM2_EN", "name")) %>%
  mutate(se = sqrt(var) * 100,
         lb = value - 1.96*se,
         ub = value + 1.96*se)

v_plots <- unique(postpred_plot$name)
v_plots <- rev(v_plots)
postpred_plot$name <- factor(postpred_plot$name, levels = v_plots)

p <- postpred_plot %>%
  ggplot(aes(x = ADM2_EN, y = value, color = name, group = name)) +
  geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.8), width = 0.2) +
  geom_point(position = position_dodge(width = 0.8)) +
  labs(x = "", y = "") +
  theme_bw() +
  coord_flip() +
  labs(title = outcome) +
  theme(text = element_text(size = 10), legend.title=element_blank()) +
  scale_color_discrete(guide = guide_legend(reverse = TRUE))
ggsave(paste0("./gen/visualizations/uncert-int/", outcome,".png"), p, width = 8, height = 10)

