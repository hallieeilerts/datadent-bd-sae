################################################################################
#' @description Creates uncertainty interval plots comparing my model and summer
#' @return One plot with uncertainty intervals for one model for one indicator
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
library(patchwork)
#' Inputs
source("./src/util.R")
info <- read.csv("./gen/model/audit/model-info.csv")
info_sum <- read.csv("./gen/model/audit/model-info-summer.csv")
################################################################################

# set inputs

# choose indicator
mynum <- 2
v_all_outcomes <- c("nt_ch_micro_vas", "nt_ch_micro_dwm", "nt_ebf", "nt_wm_micro_iron", "rh_anc_4vs", "rh_anc_1tri") 
v_all_outlab <- c("Children 6-59m given Vit. A supplements", 
                  "Children 6-59m given deworming medication",
                  "Children under 6 months exclusively breastfed",
                  "Women took iron supp. 90+ days during last pregnancy (previous 5 years)",
                  "Attended 4+ ANC visits during last pregnancy (previous 5 years)",
                  "First ANC visit during first trimester during last pregnancy (previous 5 years)")
myoutcome <- v_all_outcomes[mynum]
outlab <- v_all_outlab[mynum]

# Load var smoothing model ------------------------------------------------

# posterior predictions for all models for this indicator
filenames <- list.files("./gen/model/pred/")
filenames <- filenames[grepl("pred", filenames, ignore.case = TRUE)]
filenames <- filenames[grepl(myoutcome, filenames)]

# load posterior prediction for first model in df
filename <- filenames[1]
# for brevity, extract vers and test
model_abbrev <- substr(filename, nchar(filename) - 12, nchar(filename) - 4)
postpred <- read.csv(paste0("./gen/model/pred/",filename))
names(postpred)[which(names(postpred) == "post_mean")] <- paste0(model_abbrev, "_mean")
names(postpred)[which(names(postpred) == "post_var")] <- paste0(model_abbrev, "_var")
names(postpred)[which(names(postpred) == "qt_lb")] <- paste0(model_abbrev, "_qt_lb")
names(postpred)[which(names(postpred) == "qt_ub")] <- paste0(model_abbrev, "_qt_ub")

# merge on posterior predictions for subsequent models
if(length(filenames) > 1){
  # if more than one version of this model, read in and merge on
  for(i in 2:length(filenames)){
    
    filename <- filenames[i]
    # Extract the part between the second-to-last and third-to-last hyphen
    #model <- str_match(filename, "-([0-9]+-[^-.]+)-[0-9]{8}\\.csv$")[,2]
    model_abbrev <- substr(filename, nchar(filename) - 12, nchar(filename) - 4)
    postpred_mult <- read.csv(paste0("./gen/model/pred/",filename))
    postpred_mult <- postpred_mult[,c("ADM2_EN", "post_mean", "post_var", "qt_lb", "qt_ub")]
    names(postpred_mult)[which(names(postpred_mult) == "post_mean")] <- paste0(model_abbrev, "_mean")
    names(postpred_mult)[which(names(postpred_mult) == "post_var")] <- paste0(model_abbrev, "_var")
    names(postpred_mult)[which(names(postpred_mult) == "qt_lb")] <- paste0(model_abbrev, "_qt_lb")
    names(postpred_mult)[which(names(postpred_mult) == "qt_ub")] <- paste0(model_abbrev, "_qt_ub")
    postpred <- merge(postpred, postpred_mult, by = "ADM2_EN")
  }
}


postpred_value <- postpred %>%
  pivot_longer(
    cols = c("dir" | starts_with("0") & ends_with("mean")),
    names_to = "name",
    values_to = "value"
  ) %>%
  mutate(name = ifelse(name == "dir", "direct", name)) %>%
  mutate(name = gsub("_mean", "", name)) %>%
  dplyr::select(ADM2_EN, name, value)

postpred_var <- postpred %>%
  pivot_longer(
    cols = c("dir_var" | starts_with("0") & ends_with("var")),
    names_to = "name",
    values_to = "var"
  ) %>%
  mutate(name = ifelse(name == "dir_var", "direct", name)) %>%
  mutate(name = gsub("_var", "", name)) %>%
  dplyr::select(ADM2_EN, name, var)

postpred_qt_lb <- postpred %>%
  pivot_longer(
    cols = c(starts_with("0") & ends_with("_qt_lb")),
    names_to = "name",
    values_to = "qt_lb"
  ) %>%
  mutate(name = gsub("_qt_lb", "", name)) %>%
  dplyr::select(ADM2_EN, name, qt_lb)

postpred_qt_ub <- postpred %>%
  pivot_longer(
    cols = c(starts_with("0") & ends_with("_qt_ub")),
    names_to = "name",
    values_to = "qt_ub"
  ) %>%
  mutate(name = gsub("_qt_ub", "", name)) %>%
  dplyr::select(ADM2_EN, name, qt_ub)

postpred_plot <- postpred_value %>%
  inner_join(postpred_var, by = c("ADM2_EN", "name")) %>%
  full_join(postpred_qt_lb, by = c("ADM2_EN", "name")) %>%
  full_join(postpred_qt_ub, by = c("ADM2_EN", "name")) %>%
  mutate(se = sqrt(var) * 100)

# calculate standard uncertainty intervals for direct estimates
# credible intervals for model predictions
postpred_plot$lb <- ifelse(!is.na(postpred_plot$qt_lb), postpred_plot$qt_lb*100, postpred_plot$value - 1.96*postpred_plot$se)
postpred_plot$ub <- ifelse(!is.na(postpred_plot$qt_ub), postpred_plot$qt_ub*100, postpred_plot$value + 1.96*postpred_plot$se)

v_plots <- unique(postpred_plot$name)
v_plots <- rev(v_plots)
postpred_plot$name <- factor(postpred_plot$name, levels = v_plots)
postpred_plot$model <- "OurModel"
postpred_plot$model[postpred_plot$name == "direct"] <- "Direct"

pred_mymodel <- postpred_plot
pred_mymodel <- pred_mymodel[,c("ADM2_EN", "name", "model", "value", "lb", "ub")]

# Load SUMMER -------------------------------------------------------------

# posterior predictions for all models for this indicator
filenames <- list.files("./gen/model/pred-summer/")
filenames <- filenames[grepl("pred", filenames, ignore.case = TRUE)]
filenames <- filenames[grepl(myoutcome, filenames)]

# required columns
# ADM2_EN, name (model name), value, lb, ub, model

postpred <- data.frame()
for(i in 1:length(filenames)){
  
  filename <- filenames[i]
  # Extract the part between the second-to-last and third-to-last hyphen
  #model <- str_match(filename, "-([0-9]+-[^-.]+)-[0-9]{8}\\.csv$")[,2]
  model_abbrev <- substr(filename, nchar(filename) - 4, nchar(filename) - 4)
  postpred_mult <- read.csv(paste0("./gen/model/pred-summer/",filename))
  postpred_mult <- postpred_mult[,c("domain", "mean", "var", "lower", "upper")]
  names(postpred_mult)[which(names(postpred_mult) == "domain")] <- "ADM2_EN"
  names(postpred_mult)[which(names(postpred_mult) == "mean")] <- "value"
  #names(postpred_mult)[which(names(postpred_mult) == "var")] <- paste0(model_abbrev, "_var")
  names(postpred_mult)[which(names(postpred_mult) == "lower")] <- "lb"
  names(postpred_mult)[which(names(postpred_mult) == "upper")] <- "ub"
  postpred_mult$name <- model_abbrev
  postpred <- rbind(postpred, postpred_mult)
}

postpred$value <- postpred$value*100
postpred$lb <- postpred$lb*100
postpred$ub <- postpred$ub*100
postpred$model <- "SUMMER"

pred_summer <- postpred
pred_summer <- pred_summer[,c("ADM2_EN", "name", "model", "value", "lb", "ub")]

# Create merge key for my model and summer --------------------------------

# Subset to outcome of interest
df_info <- subset(info, outcome == myoutcome)
df_info_sum <- subset(info_sum, outcome == myoutcome)

# if cov is NA in my model info, assign as 1
df_info$cov[is.na(df_info$cov)] <- 1
# create name column that matches with model predictions
# keep only that and covariates
df_info <- df_info %>%
  mutate(vers_padded = str_pad(as.character(vers), width = 3, side = "left", pad = "0")) %>%
  mutate(name_mymodel = paste0(vers_padded, "-", test)) %>%
  select(name_mymodel, cov)

# keep only name and covariates for summer
df_info_sum <- df_info_sum %>%
  mutate(name_summer = vers) %>%
  select(name_summer, cov)

# merge
mod_merge_key <- merge(df_info, df_info_sum, by = "cov")

# Drop 001-Test1 from name_mymodel, which was model without intercept
mod_merge_key <- subset(mod_merge_key, name_mymodel != "001-Test1")


# Combine data ------------------------------------------------------------

dat <- rbind(pred_mymodel, pred_summer)
dat <- merge(dat, mod_merge_key, by.x = "name", by.y = "name_mymodel", all.x = TRUE)
dat <- merge(dat, mod_merge_key, by.x = "name", by.y = "name_summer", all.x = TRUE)

# keep where ourmodel has been matched with name_summer
# (or drop where they have not been matched, and vice versa)
dat <- subset(dat, !(model == "OurModel" & is.na(name_summer)))
dat <- subset(dat, !(model == "SUMMER" & is.na(name_mymodel)))
# create one column for covariates
dat <- dat %>%
  mutate(cov = coalesce(cov.x, cov.y))
dat$covgrp <- dat$cov
dat$covgrp[dat$model == "Direct"] <- "Direct"
dat$covgrp[dat$cov == 1] <- "Intercept"

v_cov <- unique(dat$cov)
v_cov <- v_cov[!is.na(v_cov)]

for(i in 1:length(v_cov)){
  p <- dat %>%
    filter(model == "Direct" | cov %in% v_cov[i]) %>%
    ggplot(aes(x = ADM2_EN, y = value, color = model)) +
    geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.5), width = 0.2) +
    geom_point(position = position_dodge(width = 0.5)) +
    labs(x = "", y = "") +
    theme_bw() +
    coord_flip() +
    labs(title = paste0(myoutcome, "~", v_cov[i])) +
    theme(text = element_text(size = 14), legend.title=element_blank()) +
    scale_color_discrete(guide = guide_legend(reverse = TRUE))
  ggsave(paste0("./gen/visualizations/compare-summer/", myoutcome,"-", v_cov[i], "-", format(Sys.Date(), "%Y%m%d"), ".png"), p, width = 8, height = 20, limitsize = F)
}




