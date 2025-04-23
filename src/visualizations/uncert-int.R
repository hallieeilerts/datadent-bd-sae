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
library(stringr)
#' Inputs
source("./src/util.R")
# direct estiamtes
est <- read.csv("./gen/calculate-direct/output/direct-estimates.csv")
################################################################################

# load posterior predictions

# choose indicator
unique(est$variable)
# "nt_ch_micro_vas"  "nt_ch_micro_dwm"  "nt_ebf" "nt_wm_micro_iron" "rh_anc_4vs" "rh_anc_1tri" 
outcome <- "rh_anc_1tri" 

# 001 no intercept
# 002 has intercept
# 003 has residence covariate
# 004 has mother_edu covariate
# 005 has wealth_index covariate
# 006 has hhd_under5 covariate

# posterior predictions for all models for this indicator
filenames <- list.files("./gen/model/pred/")
filenames <- filenames[grepl("pred", filenames, ignore.case = TRUE)]
filenames <- filenames[grepl(outcome, filenames)]

# load posterior prediction for first model in df
filename <- filenames[1]
# for brevity, extract vers and test
model_abbrev <- substr(filename, nchar(filename) - 12, nchar(filename) - 4)
postpred <- read.csv(paste0("./gen/model/pred/",filename))
names(postpred)[which(names(postpred) == "post_mean")] <- paste0(model_abbrev, "_mean")
names(postpred)[which(names(postpred) == "post_var")] <- paste0(model_abbrev, "_var")

# merge on posterior predictions for subsequent models
if(length(filenames) > 1){
  # if more than one version of this model, read in and merge on
  for(i in 2:length(filenames)){
    
    filename <- filenames[i]
    # Extract the part between the second-to-last and third-to-last hyphen
    #model <- str_match(filename, "-([0-9]+-[^-.]+)-[0-9]{8}\\.csv$")[,2]
    model_abbrev <- substr(filename, nchar(filename) - 12, nchar(filename) - 4)
    postpred_mult <- read.csv(paste0("./gen/model/pred/",filename))
    postpred_mult <- postpred_mult[,c("ADM2_EN", "post_mean", "post_var")]
    names(postpred_mult)[which(names(postpred_mult) == "post_mean")] <- paste0(model_abbrev, "_mean")
    names(postpred_mult)[which(names(postpred_mult) == "post_var")] <- paste0(model_abbrev, "_var")
    postpred <- merge(postpred, postpred_mult, by = "ADM2_EN")
  }
}

# reshape -----------------------------------------------------------------

postpred_value <- postpred %>%
  pivot_longer(
    cols = c("dir" | starts_with("00") & ends_with("mean")),
    names_to = "name",
    values_to = "value"
  ) %>%
  mutate(name = ifelse(name == "dir", "direct", name)) %>%
  mutate(name = gsub("_mean", "", name)) %>%
  dplyr::select(ADM2_EN, name, value)

postpred_var <- postpred %>%
  pivot_longer(
    cols = c("dir_var" | starts_with("00") & ends_with("var")),
    names_to = "name",
    values_to = "var"
  ) %>%
  mutate(name = ifelse(name == "dir_var", "direct", name)) %>%
  mutate(name = gsub("_var", "", name)) %>%
  dplyr::select(ADM2_EN, name, var)

postpred_qt <- postpred %>%
  dplyr::select(ADM2_EN, qt_lb, qt_ub) %>%
  mutate(name = model_abbrev)

postpred_plot <- postpred_value %>%
  inner_join(postpred_var, by = c("ADM2_EN", "name")) %>%
  full_join(postpred_qt, by = c("ADM2_EN", "name")) %>%
  mutate(se = sqrt(var) * 100)

# calculate standard uncertainty intervals for direct estimates
# credible intervals for model predictions
postpred_plot$lb <- ifelse(!is.na(postpred_plot$qt_lb), postpred_plot$qt_lb*100, postpred_plot$value - 1.96*postpred_plot$se)
postpred_plot$ub <- ifelse(!is.na(postpred_plot$qt_ub), postpred_plot$qt_ub*100, postpred_plot$value + 1.96*postpred_plot$se)

v_plots <- unique(postpred_plot$name)
v_plots <- rev(v_plots)
postpred_plot$name <- factor(postpred_plot$name, levels = v_plots)

p <- postpred_plot %>%
  filter(name %in% c("direct", "002-Test1", "003-Test1", "004-Test1", "005-Test1", "006-Test1")) %>%
  mutate(name = case_when(
    name == "direct" ~ "1 - direct",
    name == "002-Test1" ~ "2 - baseline",
    name == "003-Test1" ~ "3 - residence",
    name == "004-Test1" ~ "4 - mother_edu",
    name == "005-Test1" ~ "5 - wealth_index",
    name == "006-Test1" ~ "6 - hhd_under5",
    TRUE ~ name)
  ) %>% 
  ggplot(aes(x = ADM2_EN, y = value, color = name, group = name)) +
  geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.8), width = 0.2) +
  geom_point(position = position_dodge(width = 0.8)) +
  labs(x = "", y = "") +
  theme_bw() +
  coord_flip() +
  labs(title = outcome) +
  theme(text = element_text(size = 14), legend.title=element_blank()) +
  #scale_color_manual(values = wes_palette("Zissou1", n = length(unique(postpred_plot$name)), type = "discrete"))
  scale_color_discrete(guide = guide_legend(reverse = TRUE)) 

ggsave(paste0("./gen/visualizations/uncert-int/", outcome,".png"), p, width = 8, height = 20, limitsize = F)

