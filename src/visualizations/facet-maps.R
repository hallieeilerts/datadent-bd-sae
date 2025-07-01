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
# Bangladesh division boundaries
bangladesh_1 <- st_read("./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm1_bbs_20201113")
# Bangladesh district boundaries
bangladesh_2 <- st_read("./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm2_bbs_20201113")
# direct estimates
est <- read.csv("./gen/calculate-direct/output/direct-estimates.csv")
################################################################################

# choose indicator
unique(est$variable)
mynum <- 6
v_all_outcomes <- c("nt_ch_micro_vas", "nt_ch_micro_dwm", "nt_ebf", "nt_wm_micro_iron", "rh_anc_4vs", "rh_anc_1tri") 
v_all_outlab <- c("Children 6-59m given Vit. A supplements", 
                  "Children 6-59m given deworming medication",
                  "Children under 6 months exclusively breastfed",
                  "Women took iron supp. 90+ days during last pregnancy (previous 5 years)",
                  "Attended 4+ ANC visits during last pregnancy (previous 5 years)",
                  "First ANC visit during first trimester during last pregnancy (previous 5 years)")
outcome <- v_all_outcomes[mynum]
outlab <- v_all_outlab[mynum]

# posterior predictions for all models for this indicator
filenames <- list.files("./gen/model/pred")
filenames <- filenames[grepl("pred", filenames, ignore.case = TRUE)]
filenames <- filenames[grepl(outcome, filenames)]

# save posterior prediction for first model in df
filename <- filenames[1]
# Extract the part between the second-to-last and third-to-last hyphen
#model <- str_match(filename, "-([0-9]+-[^-.]+)-[0-9]{8}\\.csv$")[,2]
model <- substr(filename, nchar(filename)-12, nchar(filename)-4)
postpred <- read.csv(paste0("./gen/model/pred/",filename))
names(postpred)[which(names(postpred) == "post_mean")] <- paste0(model, "_mean")
names(postpred)[which(names(postpred) == "post_var")] <- paste0(model, "_var")

# merge on posterior predictions for subsequent models
if(length(filenames) > 1){
  # if more than one version of this model, read in and merge on
  for(i in 2:length(filenames)){
    
    filename <- filenames[i]
    # Extract the part between the second-to-last and third-to-last hyphen
    #model <- str_match(filename, "-([0-9]+-[^-.]+)-[0-9]{8}\\.csv$")[,2]
    model <- substr(filename, nchar(filename)-12, nchar(filename)-4)
    postpred_mult <- read.csv(paste0("./gen/model/pred/",filename))
    postpred_mult <- postpred_mult[,c("ADM2_EN", "post_mean", "post_var")]
    names(postpred_mult)[which(names(postpred_mult) == "post_mean")] <- paste0(model, "_mean")
    names(postpred_mult)[which(names(postpred_mult) == "post_var")] <- paste0(model, "_var")
    postpred <- merge(postpred, postpred_mult, by = "ADM2_EN")
  }
}

# Join spatial data
postpred_sf <- bangladesh_2 %>% 
  left_join(postpred, by = "ADM2_EN")

# Set which models to plot
if(outcome %in% c("nt_wm_micro_iron", "nt_ch_micro_vas", "nt_ch_micro_dwm", "rh_anc_1tri", "rh_anc_4vs", "nt_ebf")){
  v_plots <- c("direct", "050-Test1")
}

# Prevalence --------------------------------------------------------------

postpred_long <- postpred_sf %>%
  pivot_longer(
    cols = c("dir" | starts_with("0") & ends_with("mean")),
    names_to = "plotlabel",
    values_to = "value"
  ) %>%
  mutate(plotlabel = ifelse(plotlabel == "dir", "direct", plotlabel)) %>%
  mutate(plotlabel = gsub("_mean", "", plotlabel))

#v_plots <- unique(postpred_long$plotlabel)
#postpred_long$plotlabel <- factor(postpred_long$plotlabel, levels = v_plots)

plotdat <- subset(postpred_long, plotlabel %in% v_plots)
plotdat$plotlabel <- ifelse(plotdat$plotlabel == "direct", "Direct", "Modeled")


p1 <- ggplot() +
  geom_sf(data = plotdat, aes(fill = value), color = NA) +
  geom_sf(data = bangladesh_2, color = "black", fill = NA) +
  scale_fill_gradientn(colors = c("red", "yellow", "forestgreen"), name = "") +
  labs(title = outlab, subtitle = "Prevalence") +
  facet_wrap(~plotlabel) +
  theme_void() +
  theme(text = element_text(size = 10))
# Normal plot
#ggsave(paste0("./gen/visualizations/facet-maps/",outcome,"-prev","_", format(Sys.Date(), "%Y%m%d") , ".png"), p1, width = 8, height = 4)
# Plot for datadent presentation May 13 2025 on wide slides
ggsave(paste0("./gen/visualizations/facet-maps/",outcome,"-prev","_", format(Sys.Date(), "%Y%m%d") , ".png"), p1, width = 10, height = 5.5)



# Variance ----------------------------------------------------------------

postpred_long <- postpred_sf %>%
  pivot_longer(
    cols = c("dir_var" | starts_with("0") & ends_with("var")),
    names_to = "plotlabel",
    values_to = "value"
  ) %>%
  mutate(plotlabel = ifelse(plotlabel == "dir_var", "direct", plotlabel)) %>%
  mutate(plotlabel = gsub("_var", "", plotlabel))
#v_plots <- unique(postpred_long$plotlabel)
#postpred_long$plotlabel <- factor(postpred_long$plotlabel, levels = v_plots)

plotdat <- subset(postpred_long, plotlabel %in% v_plots)
plotdat$plotlabel <- ifelse(plotdat$plotlabel == "direct", "Direct", "Modeled")

p2 <- ggplot() +
  geom_sf(data = plotdat, aes(fill = value), color = NA) +
  geom_sf(data = bangladesh_2, color = "black", fill = NA) +
  scale_fill_viridis_c(
    option = "D",  # "D" is default; you can also try "C", "B", "A", "E"
    limits = c(0, max(postpred_long$value, na.rm = TRUE)),
    na.value = "grey80",
    name = ""
  ) +
  labs(title = outlab, subtitle = "Variance") +
  facet_wrap(~plotlabel) +
  theme_void() +
  theme(text = element_text(size = 10))
# Normal plot
#ggsave(paste0("./gen/visualizations/facet-maps/", outcome,"-var","_", format(Sys.Date(), "%Y%m%d") , ".png"), p2, width = 8, height = 4)
# Plot for datadent presentation May 13 2025 on wide slides
ggsave(paste0("./gen/visualizations/facet-maps/", outcome,"-var","_", format(Sys.Date(), "%Y%m%d") , ".png"), p2, width = 10, height = 5.5)
