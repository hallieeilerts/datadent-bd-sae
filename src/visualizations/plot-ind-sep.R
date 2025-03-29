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
################################################################################

# create column for name in ggplot
modinfo$plotlabel <- NA
#modinfo[grepl("001-Test1", modinfo$file, ignore.case = TRUE), "plotlabel"] <- "Baseline" 
#modinfo[grepl("002-Test1", modinfo$file, ignore.case = TRUE), "plotlabel"] <- "Full" 

# create confidence intervals for direct estimates
direct <- subset(direct, admin_level == "adm2")
names(direct)[which(names(direct) == "district_name")] <- "ADM2_EN"
direct <- direct[,c("ADM2_EN", "value", "lower", "upper", "indicator")]
direct$model <- "Direct"

# merge on model name
names(pred)[which(names(pred) == "district")] <- "ADM2_EN"
df_modinfo <- modinfo[,c("file", "plotlabel")]
df_modinfo <- df_modinfo[!duplicated(df_modinfo),]
df_modinfo$model <- str_extract(df_modinfo$file, "(?<=_)[^_]+(?=_)")
df_modinfo$file <- NULL
df_pred <- merge(pred, df_modinfo, by = "model")
df_pred$model <- df_pred$plotlabel
df_pred$plotlabel <- NULL

# combine
dat <- rbind(direct, df_pred)
dat$model <- factor(dat$model, levels = c("Direct", "Baseline", "Full"))

# merge with boundaries
AD2_merged <- left_join(AD2, dat, by = "ADM2_EN")

# Get unique indicators
indicators <- unique(dat$indicator)

# Loop over indicators
for (ind in indicators) {
  
  # Filter for indicator
  dat_ind <- dat %>% filter(indicator == ind)
  AD2_merged_ind <- AD2_merged %>% filter(indicator == ind)
  
  # Map plot
  p1 <- ggplot() +
    geom_sf(data = AD2_merged_ind, aes(fill = value), color = "black") +
    scale_fill_viridis_c(option = "magma", na.value = "gray80") + 
    theme_bw() +
    facet_wrap(~model, ncol = 1) +
    labs(title = paste(ind))
  
  # Uncertainty plot
  dodge_width <- 0.8
  p2 <- ggplot(dat_ind, aes(x = ADM2_EN, y = value, color = model, group = model)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = dodge_width), width = 0.2) +
    geom_point(aes(shape = model), size = 1, position = position_dodge(width = dodge_width)) +
    labs(x = "", y = "") +
    theme_bw() +
    coord_flip() +
    theme(text = element_text(size = 10), legend.title=element_blank())
  
  # Combine plots
  combined <- p1 + p2 + plot_layout(widths = c(1, 1))
  
  # Save to PDF
  ggsave(filename = paste0("./gen/visualizations/output/", ind, ".png"),
         plot = combined,  width = 10, height = 8)
}
