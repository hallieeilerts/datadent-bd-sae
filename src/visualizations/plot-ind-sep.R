################################################################################
#' @description Plot indicators separately
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(ggplot2)
library(rgdal)
library(spdep)
library(dplyr)
library(patchwork)
#' Inputs
source("./src/util.R")
# predicted prevalence
pred <- read.csv("./gen/model/output/predictions.csv")
# direct
direct <- read.csv("./gen/calculate-direct/output/direct-estimates.csv")
# district shape files
AD2 <- readOGR(dsn="./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm2_bbs_20201113")
AD2 <- st_as_sf(AD2)
# model names for plots
modinfo <- read.csv("./gen/model/audit/model-info.csv")
modinfoWide <- read.csv("./gen/model/audit/model-info-wide.csv")
################################################################################

# create confidence intervals for direct estimates
direct <- subset(direct, admin_level == "adm2")
names(direct)[which(names(direct) == "district_name")] <- "ADM2_EN"
direct <- direct[,c("ADM2_EN", "value", "lower", "upper", "outcome")]
direct$plotlabel <- "Direct"
direct$weighted <- FALSE

# predictions
df_pred <- pred
names(df_pred)[which(names(df_pred) == "district")] <- "ADM2_EN"
# merge on model info
df_pred <- merge(df_pred, modinfoWide, by = "file")


# Uncertainty intervals ---------------------------------------------------

df_pred1 <- df_pred
df_pred1$plotlabel <- paste0(df_pred1$vers, "-", df_pred1$test)
df_pred1 <- df_pred1[,c("ADM2_EN", "value", "lower", "upper", "outcome", "plotlabel", "weighted")]

dat_ind <- rbind(direct, df_pred1)
dat_ind <- dat_ind %>% filter(outcome == "nt_wm_micro_iron")
dat_ind$weighted[dat_ind$weighted == TRUE] <- "weighted"
dat_ind$weighted[dat_ind$weighted == FALSE] <- "unweighted"

# remove intercept models
dat_ind <- subset(dat_ind, plotlabel != "1-Test1")
dat_ind <- subset(dat_ind, plotlabel != "2-Test1")
# remove others that don't have major differences
dat_ind <- subset(dat_ind, plotlabel != "8-Test2")
dat_ind <- subset(dat_ind, plotlabel != "9-Test2")
dat_ind <- subset(dat_ind, plotlabel != "11-Test3")


dodge_width <- 0.8
v_dist <- unique(dat_ind$ADM2_EN)[5:9]
dat_ind$plotlabel <- factor(dat_ind$plotlabel, levels = unique(dat_ind$plotlabel))

p0 <- dat_ind %>%
  filter(ADM2_EN %in% v_dist) %>%
  ggplot(aes(x = ADM2_EN, y = value, color = plotlabel, group = plotlabel)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = dodge_width), width = 0.2) +
  geom_point(aes(shape = weighted), position = position_dodge(width = dodge_width)) +
  labs(x = "", y = "") +
  theme_bw() +
  coord_flip() +
  theme(text = element_text(size = 10), legend.title=element_blank())

ggsave(paste0("./gen/visualizations/temp/ui-nt_wm_micro_iron2.png"), p0, width = 10, height = 8)

# Loop for map + intervals ------------------------------------------------

# subset model fits
df_pred_sub$plotlabel <- NA
# if missing all betas this is a baseline model
df_pred_sub$baseline <- apply(df_pred_sub[, grepl("^b_", names(df_pred_sub))], 1, function(x) all(is.na(x)))
df_pred_sub$full <- apply(df_pred_sub[, grepl("^b_", names(df_pred_sub))], 1, function(x) all(!is.na(x)))
df_pred_sub$plotlabel[df_pred_sub$baseline == TRUE] <- "Baseline"
df_pred_sub$plotlabel[df_pred_sub$full == TRUE] <- "Full"
# some baselines have t-test prior. I added a cauchy prior for others.
df_pred_sub$ssRE <- str_replace_all( string = df_pred_sub$ssRE , pattern = "[^A-Za-z0-9 ]", replacement = "")
df_pred_sub$plotlabel[df_pred_sub$baseline == TRUE & grepl("cauchy0 2", df_pred_sub$ssRE)] <- "Baseline - cauchy 2"
df_pred_sub$plotlabel[df_pred_sub$baseline == TRUE & grepl("cauchy0 5", df_pred_sub$ssRE)] <- "Baseline - cauchy 5"
df_pred_sub <- subset(df_pred_sub, !is.na(plotlabel))
df_pred_sub <- df_pred_sub[,c("ADM2_EN", "value", "lower", "upper", "outcome", "plotlabel")]

# combine
dat <- rbind(direct, df_pred_sub)
dat$plotlabel <- factor(dat$plotlabel, levels = c("Direct", "Baseline", "Baseline - cauchy 2", "Baseline - cauchy 5", "Full"))


# merge with boundaries
AD2_merged <- left_join(AD2, dat, by = "ADM2_EN")

# Get unique outcomes
outcomes <- unique(dat$outcome)
#outcomes <- "nt_ch_micro_dwm"

# Loop over indicators
for (i in 1:length(outcomes)) {
  
  # Filter for outcome
  dat_ind <- dat %>% filter(outcome == outcomes[i])
  AD2_merged_ind <- AD2_merged %>% filter(outcome == outcomes[i])
  
  # Map plot
  p1 <- ggplot() +
    geom_sf(data = AD2_merged_ind, aes(fill = value), color = "black") +
    scale_fill_viridis_c(option = "magma", na.value = "gray80") + 
    theme_bw() +
    facet_wrap(~plotlabel, ncol = 1) +
    labs(title = paste(outcomes[i]))
  
  # Uncertainty plot
  dodge_width <- 0.8
  p2 <- ggplot(dat_ind, aes(x = ADM2_EN, y = value, color = plotlabel, group = plotlabel)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = dodge_width), width = 0.2) +
    geom_point(aes(shape = plotlabel), size = 1, position = position_dodge(width = dodge_width)) +
    labs(x = "", y = "") +
    theme_bw() +
    coord_flip() +
    theme(text = element_text(size = 10), legend.title=element_blank())
  
  # Combine plots
  combined <- p1 + p2 + plot_layout(widths = c(1, 1))
  
  # Save to PDF
  ggsave(filename = paste0("./gen/visualizations/output/", outcomes[i], ".png"),
         plot = combined,  width = 10, height = 8)
}
