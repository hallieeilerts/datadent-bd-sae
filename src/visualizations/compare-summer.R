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
info_summer <- read.csv("./gen/model/audit/model-info-summer.csv")
################################################################################

# set inputs


# choose indicator
mynum <- 15
# v_all_outcomes <- c("nt_ch_micro_vas", "nt_ch_micro_dwm", "nt_ebf", "nt_wm_micro_iron", "rh_anc_4vs", "rh_anc_1tri") 
# v_all_outlab <- c("Children 6-59m given Vit. A supplements", 
#                   "Children 6-59m given deworming medication",
#                   "Children under 6 months exclusively breastfed",
#                   "Women took iron supp. 90+ days during last pregnancy (previous 5 years)",
#                   "Attended 4+ ANC visits during last pregnancy (previous 5 years)",
#                   "First ANC visit during first trimester during last pregnancy (previous 5 years)")
# indicator list
v_all_outcomes <- c("ch_diar_ors", 
                    "ch_diar_zinc",
                    "nt_ch_micro_dwm",
                    "nt_ch_micro_mp",
                    "nt_ch_micro_vas",
                    "nt_ebf",
                    "nt_wm_micro_iron",
                    "nt_wm_micro_iron_any",
                    "ph_hndwsh_basic",
                    "ph_sani_improve",
                    "ph_wtr_improve",
                    "ph_wtr_trt_appr",
                    "rh_anc_1tri",
                    "rh_anc_4vs",
                    "rh_anc_1vs",
                    "rh_anc_bldpres",
                    "rh_anc_bldsamp",
                    "rh_anc_iron",
                    "rh_anc_urine",
                    "rh_anc_wgt",
                    "rh_del_inst",
                    "rh_del_pvskill",
                    "rh_pnc_nb_2days",
                    "rh_pnc_wm_2days",
                    "rh_pnc_wm_bfcounsel"
) 
v_ch_outcomes <- c("ch_diar_ors",
                   "ch_diar_zinc",
                   "nt_ch_micro_dwm", 
                   "nt_ch_micro_mp",
                   "nt_ch_micro_vas", 
                   "nt_ebf")
v_all_outlab <- c("Children under 5y with diarrhea in the preceding 2 weeks who received ORS",
                  "Children under 5y with diarrhea in the preceding 2 weeks who received zinc supplements",
                  "Children 6-59m given deworming medication",
                  "Children 6-23m given multiple micronutrient powder",
                  "Children 6-59m given Vit. A supplements", 
                  "Children under 6 months exclusively breastfed",
                  "Women took iron supp. 90+ days during last pregnancy (previous 5 years)",
                  "Women took any iron supp. during last pregnancy (previous 5 years)",
                  "Households with basic handwashing facility",
                  "Households with access to improved sanitation facility",
                  "Households with access to improved water source",
                  "Households using an appropriate water treatment method",
                  "First ANC visit during first trimester during last pregnancy (previous 5 years)",
                  "Attended 4+ ANC visits during last pregnancy (previous 5 years)",
                  "Attended 1 ANC visit during last pregnancy (previous 5 years)",
                  "Blood pressure taken at ANC visit during last pregnancy (previous 5 years)", # among those with any anc
                  "Blood sample taken at ANC visit during last pregnancy (previous 5 years)", # among those with any anc
                  "Received iron tablets or syrup at ANC visit", 
                  "Urine sample taken at ANC visit during last pregnancy (previous 5 years)", # among those with any anc
                  "Weighed at ANC visit during last pregnancy (previous 5 years)", # among those with any anc
                  "Live births delivered in health facility (previous 5 years)",
                  "Live births with skilled provider providing assistance at delivery (previous 5 years)",
                  "PNC check within two days for newborn (last birth in previous 24 months)",
                  "PNC check within two days for mother (last birth in previous 24 months)",
                  "Breastfeeding counseling within two days of birth (last birth in previous 24 months)")

myoutcome <- v_all_outcomes[mynum]
outlab <- v_all_outlab[mynum]

# Load var smoothing model ------------------------------------------------

# posterior predictions for all models for this indicator
filenames <- list.files("./gen/model/pred/")
filenames <- filenames[grepl("pred", filenames, ignore.case = TRUE)]
filenames <- filenames[grepl(myoutcome, filenames)]
if(myoutcome == "nt_wm_micro_iron"){
  filenames <- filenames[!grepl("nt_wm_micro_iron_any", filenames)]
}

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

if(myoutcome %in% v_ch_outcomes){
  
  postpred_value <- postpred %>%
    pivot_longer(
      cols = c("dirplot" | matches("^[0-9]") & ends_with("mean")),
      names_to = "name",
      values_to = "value"
    ) %>%
    mutate(name = ifelse(name == "dirplot", "direct", name)) %>%
    mutate(name = gsub("_mean", "", name)) %>%
    dplyr::select(ADM2_EN, name, value)
  postpred_var <- postpred %>%
    pivot_longer(
      cols = c("dirplot_var" | matches("^[0-9]") & ends_with("var")),
      names_to = "name",
      values_to = "var"
    ) %>%
    mutate(name = ifelse(name == "dirplot_var", "direct", name)) %>%
    mutate(name = gsub("_var", "", name)) %>%
    dplyr::select(ADM2_EN, name, var)
  
}else{
  postpred_value <- postpred %>%
    pivot_longer(
      cols = c("dir" | matches("^[0-9]") & ends_with("mean")),
      names_to = "name",
      values_to = "value"
    ) %>%
    mutate(name = ifelse(name == "dir", "direct", name)) %>%
    mutate(name = gsub("_mean", "", name)) %>%
    dplyr::select(ADM2_EN, name, value)
  
  postpred_var <- postpred %>%
    pivot_longer(
      cols = c("dir_var" | matches("^[0-9]") & ends_with("var")),
      names_to = "name",
      values_to = "var"
    ) %>%
    mutate(name = ifelse(name == "dir_var", "direct", name)) %>%
    mutate(name = gsub("_var", "", name)) %>%
    dplyr::select(ADM2_EN, name, var)
  
}

postpred_qt_lb <- postpred %>%
  pivot_longer(
    cols = c(matches("^[0-9]") & ends_with("_qt_lb")),
    names_to = "name",
    values_to = "qt_lb"
  ) %>%
  mutate(name = gsub("_qt_lb", "", name)) %>%
  dplyr::select(ADM2_EN, name, qt_lb)

postpred_qt_ub <- postpred %>%
  pivot_longer(
    cols = c(matches("^[0-9]") & ends_with("_qt_ub")),
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
postpred_plot$value <- postpred_plot$value*100
#postpred_plot$value[postpred_plot$name == "direct"] <- postpred_plot$value[postpred_plot$name == "direct"]*100
postpred_plot$lb <- ifelse(!is.na(postpred_plot$qt_lb), postpred_plot$qt_lb*100, postpred_plot$value - 1.96*postpred_plot$se)
postpred_plot$ub <- ifelse(!is.na(postpred_plot$qt_ub), postpred_plot$qt_ub*100, postpred_plot$value + 1.96*postpred_plot$se)
# sometimes the direct confidence intervals will exceed 0 or 100
postpred_plot$lb[postpred_plot$lb < 0] <- 0
postpred_plot$ub[postpred_plot$ub > 100] <- 100

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
if(myoutcome == "nt_wm_micro_iron"){
  filenames <- filenames[!grepl("nt_wm_micro_iron_any", filenames)]
}
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
df_info_summer <- subset(info_summer, outcome == myoutcome)

# if cov is NA in my model info, assign as 1
df_info$cov[is.na(df_info$cov)] <- 1
# create name column that matches with model predictions
df_info <- df_info %>%
  mutate(vers_padded = str_pad(as.character(vers), width = 3, side = "left", pad = "0")) %>%
  mutate(name_mymodel = paste0(vers_padded, "-", test)) 

# CHOOSE WHICH TO COMPARE FROM MY MODELS

df_info <- df_info %>%
  filter(name_mymodel != "001-Test1") %>% # drop 001-Test1, which was model without intercept
  filter(vers >= 100) %>% # only keep >=100 to compare models with penalized complexity prior
  filter(test == "Test1") %>% # if vers>=100, keep test1 or test2 (the latter with less spatial smoothing due to pcp prior values)
  dplyr::select(name_mymodel, cov)

# keep only name and covariates for summer
df_info_summer <- df_info_summer %>%
  mutate(name_summer = vers) %>%
  dplyr::select(name_summer, cov)

# merge
mod_merge_key <- merge(df_info, df_info_summer, by = "cov")

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

# plot one covariate combination
# foo <- 1
# dat %>%
#   filter(model == "Direct" | cov %in% v_cov[foo]) %>%
#   ggplot(aes(x = ADM2_EN, y = value, color = model)) +
#   geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.8), width = 0.2) +
#   geom_point(position = position_dodge(width = 0.8)) +
#   labs(x = "", y = "") +
#   theme_bw() +
#   coord_flip() +
#   labs(title = paste0(myoutcome, "~", v_cov[foo])) +
#   theme(text = element_text(size = 14), legend.title=element_blank()) +
#   scale_color_discrete(guide = guide_legend(reverse = TRUE))


# # Plot multiple covariate combos and save
# for(i in 1:nrow(mod_merge_key)){
#   
#   myrow <- mod_merge_key[i,]
#   mycov <- myrow$cov
#   mymodelname <- myrow$name_mymodel
#   
#   p <- dat %>%
#     filter(model == "Direct" | cov %in% mycov) %>%
#     filter(!(model == "OurModel" & name != mymodelname)) %>% 
#     ggplot(aes(x = ADM2_EN, y = value, color = model)) +
#     geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.5), width = 0.2) +
#     geom_point(position = position_dodge(width = 0.5)) +
#     labs(x = "", y = "") +
#     theme_bw() +
#     coord_flip() +
#     labs(title = paste0(myoutcome, "~", mycov)) +
#     theme(text = element_text(size = 14), legend.title=element_blank()) +
#     scale_color_discrete(guide = guide_legend(reverse = TRUE))
#   ggsave(paste0("./gen/visualizations/compare-summer/", myoutcome,"-", mycov, "-", mymodelname, "-", format(Sys.Date(), "%Y%m%d"), ".png"), p, width = 8, height = 20, limitsize = F)
# }


# two panels --------------------------------------------------------------

# choose one model
myrow <- subset(mod_merge_key, name_mymodel == "100-Test1")
mycov <- myrow$cov
mymodelname <- myrow$name_mymodel

df_dist <- dat %>%
  dplyr::select(ADM2_EN) %>%
  distinct() %>% arrange(ADM2_EN) %>%
  mutate(n = 1:n(),
         total = n(),
         panel = ifelse(n/total<0.5, 1, 2))
plot1 <- dat %>% filter(ADM2_EN %in% subset(df_dist, panel == 1)$ADM2_EN)
plot2 <- dat %>% filter(ADM2_EN %in% subset(df_dist, panel == 2)$ADM2_EN)

# p <- dat %>%
#   filter(model == "Direct" | cov %in% mycov) %>%
#   filter(!(model == "OurModel" & name != mymodelname)) %>% 
#   ggplot(aes(x = ADM2_EN, y = value, color = model)) +
#   geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.5), width = 0.2) +
#   geom_point(position = position_dodge(width = 0.5)) +
#   labs(x = "", y = "") +
#   theme_bw() +
#   coord_flip() +
#   labs(title = paste0(myoutcome, "~", mycov)) +
#   theme(text = element_text(size = 14), legend.title=element_blank()) +
#   scale_color_discrete(guide = guide_legend(reverse = TRUE))
# ggsave(paste0("./gen/visualizations/compare-summer/intercept-model/", myoutcome,"_", format(Sys.Date(), "%Y%m%d"), ".png"), p, width = 8, height = 10, limitsize = F)

p1 <- plot1 %>%
  filter(model == "Direct" | cov %in% mycov) %>%
  filter(!(model == "OurModel" & name != mymodelname)) %>% 
  ggplot(aes(x = ADM2_EN, y = value, color = model)) +
  geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.5), width = 0.2) +
  geom_point(position = position_dodge(width = 0.5)) +
  labs(x = "", y = "") +
  theme_bw() +
  coord_flip() +
  labs(title = paste0(myoutcome, "~", mycov)) +
  theme(text = element_text(size = 10), legend.title=element_blank()) +
  scale_color_discrete(guide = guide_legend(reverse = TRUE))
p2 <- plot2 %>%
  filter(model == "Direct" | cov %in% mycov) %>%
  filter(!(model == "OurModel" & name != mymodelname)) %>% 
  ggplot(aes(x = ADM2_EN, y = value, color = model)) +
  geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.5), width = 0.2) +
  geom_point(position = position_dodge(width = 0.5)) +
  labs(x = "", y = "") +
  theme_bw() +
  coord_flip() +
  labs(title = paste0(myoutcome, "~", mycov)) +
  theme(text = element_text(size = 10), legend.title=element_blank()) +
  scale_color_discrete(guide = guide_legend(reverse = TRUE))

# Combine p1 and p2
combined_plot <- (p1 + p2) + 
  plot_layout(guides = "collect") &   # collect shared legend
  theme(legend.position = "bottom")   # move legend bottom
# Add single title on top
combined_plot  <- combined_plot + plot_annotation(title = paste0(outlab, " (", myoutcome, ")"))

ggsave(paste0("./gen/visualizations/compare-summer/intercept-model/", myoutcome,"_", format(Sys.Date(), "%Y%m%d"), ".png"), combined_plot, width = 10, height = 7.5, units = "in", dpi = 300)


