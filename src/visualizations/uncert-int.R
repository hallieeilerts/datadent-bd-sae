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
library(patchwork)
#' Inputs
source("./src/util.R")
# direct estiamtes
est <- read.csv("./gen/calculate-direct/output/direct-estimates.csv")
################################################################################

# load posterior predictions

# choose indicator
unique(est$variable)
mynum <- 3
v_all_outcomes <- c("nt_ch_micro_vas", "nt_ch_micro_dwm", "nt_ebf", "nt_wm_micro_iron", "rh_anc_4vs", "rh_anc_1tri") 
v_all_outlab <- c("Children 6-59m given Vit. A supplements", 
                  "Children 6-59m given deworming medication",
                  "Children under 6 months exclusively breastfed",
                  "Women took iron supp. 90+ days during last pregnancy (previous 5 years)",
                  "Attended 4+ ANC visits during last pregnancy (previous 5 years)",
                  "First ANC visit during first trimester during last pregnancy (previous 5 years)")
outcome <- v_all_outcomes[mynum]
outlab <- v_all_outlab[mynum]

# 001 no intercept
# 002 has intercept
# 003 has residence covariate
# 004 has mother_edu covariate
# 005 has wealth_index covariate
# 006 has hhd_under5 covariate
# 007 has residence, mother_edu
# 008 has residence, wealth_index
# 050 added full slate of covariates as availabe on 20250507
## "residence", "wealth_index", "hhd_under5", "hhd_head_age", "hhd_head_sex", "mother_age", "child_age"
# 051 removed residence because of high sd
## "wealth_index", "hhd_under5", "hhd_head_age", "hhd_head_sex", "mother_age", "child_age"

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

# reshape -----------------------------------------------------------------

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

p <- postpred_plot %>%
  filter(name %in% c("direct", "002-Test1", "003-Test1", "004-Test1", "005-Test1", "006-Test1",
                     "007-Test1", "008-Test1", "050-Test1")) %>%
  mutate(name = case_when(
    name == "direct" ~ "01 - direct",
    name == "002-Test1" ~ "02 - baseline",
    name == "003-Test1" ~ "03 - residence",
    name == "004-Test1" ~ "04 - mother_edu",
    name == "005-Test1" ~ "05 - wealth_index",
    name == "006-Test1" ~ "06 - hhd_under5",
    name == "007-Test1" ~ "07 - residence, mother_edu",
    name == "008-Test1" ~ "08 - residence, wealth_index",
    name == "050-Test1" ~ "50",
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

ggsave(paste0("./gen/visualizations/uncert-int/", outcome,"_", format(Sys.Date(), "%Y%m%d"), ".png"), p, width = 8, height = 20, limitsize = F)


# two panels --------------------------------------------------------------

# uncertainty intervals for Datadent presentation on may 13, 2025
# need two panels
plotdat <- postpred_plot %>%
  filter(name %in% c("direct", "050-Test1",  "051-Test2")) %>%
  mutate(name = case_when(
    name == "direct" ~ "Direct",
    name == "050-Test1" ~ "Modeled",
    name == "051-Test2" ~ "Modeled 2",
    TRUE ~ name)
  ) 
df_dist <- plotdat %>%
  select(ADM2_EN) %>%
  distinct() %>% arrange(ADM2_EN) %>%
  mutate(n = 1:n(),
         total = n(),
         panel = ifelse(n/total<0.5, 1, 2))
plot1 <- plotdat %>% filter(ADM2_EN %in% subset(df_dist, panel == 1)$ADM2_EN)
plot2 <- plotdat %>% filter(ADM2_EN %in% subset(df_dist, panel == 2)$ADM2_EN)
p1 <- plot1 %>%
  ggplot(aes(x = ADM2_EN, y = value, color = name, group = name)) +
  geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.8), width = 0.2) +
  geom_point(position = position_dodge(width = 0.8)) +
  labs(x = "", y = "") +
  theme_bw() +
  coord_flip() +
  theme(text = element_text(size = 14), legend.title=element_blank()) +
  scale_color_discrete(guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(limits = c(min(plotdat$lb), max(plotdat$ub)))
p2 <- plot2 %>%
  ggplot(aes(x = ADM2_EN, y = value, color = name, group = name)) +
  geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.8), width = 0.2) +
  geom_point(position = position_dodge(width = 0.8)) +
  labs(x = "", y = "") +
  theme_bw() +
  coord_flip() +
  theme(text = element_text(size = 14), legend.title=element_blank()) +
  scale_color_discrete(guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(limits = c(min(plotdat$lb), max(plotdat$ub)))

# Combine p1 and p2
combined_plot <- (p1 + p2) + 
  plot_layout(guides = "collect") &   # collect shared legend
  theme(legend.position = "bottom")   # move legend bottom
# Add single title on top
combined_plot  <- combined_plot + plot_annotation(title = outlab)

ggsave(paste0("./gen/visualizations/uncert-int/combined-", outcome,"_", format(Sys.Date(), "%Y%m%d"), ".png"), combined_plot, width = 10, height = 5.5, units = "in", dpi = 300)

# two panels with all models ----------------------------------------------

plotdat <- postpred_plot %>%
  filter(name %in% c("direct", "002-Test1", "003-Test1", "004-Test1", "005-Test1", "050-Test1")) %>%
  mutate(name = case_when(
    name == "direct" ~ "Direct",
    name == "002-Test1" ~ "Baseline",
    name == "003-Test1" ~ "Residence",
    name == "004-Test1" ~ "Mother Edu",
    name == "005-Test1" ~ "Wealth Index",
    name == "050-Test1" ~ "Full",
    TRUE ~ name)
  ) %>%
  mutate(name = factor(name, levels = c("Direct", "Baseline", "Residence", "Mother Edu", "Wealth Index", "Full")))
df_dist <- plotdat %>%
  select(ADM2_EN) %>%
  distinct() %>% arrange(ADM2_EN) %>%
  mutate(n = 1:n(),
         total = n(),
         panel = ifelse(n/total<0.5, 1, 2))
plot1 <- plotdat %>% filter(ADM2_EN %in% subset(df_dist, panel == 1)$ADM2_EN)
plot2 <- plotdat %>% filter(ADM2_EN %in% subset(df_dist, panel == 2)$ADM2_EN)
p1 <- plot1 %>%
  ggplot(aes(x = ADM2_EN, y = value, color = name, group = name)) +
  geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.8), width = 0.2) +
  geom_point(position = position_dodge(width = 0.8)) +
  labs(x = "", y = "") +
  theme_bw() +
  coord_flip() +
  theme(text = element_text(size = 12), legend.title=element_blank()) +
  scale_color_discrete(guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(limits = c(min(plotdat$lb), max(plotdat$ub)))
p2 <- plot2 %>%
  ggplot(aes(x = ADM2_EN, y = value, color = name, group = name)) +
  geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.8), width = 0.2) +
  geom_point(position = position_dodge(width = 0.8)) +
  labs(x = "", y = "") +
  theme_bw() +
  coord_flip() +
  theme(text = element_text(size = 12), legend.title=element_blank()) +
  scale_color_discrete(guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(limits = c(min(plotdat$lb), max(plotdat$ub)))

# Combine p1 and p2
combined_plot <- (p1 + p2) + 
  plot_layout(guides = "collect") &   # collect shared legend
  theme(legend.position = "bottom",   # move legend bottom    
        legend.margin = margin(t = -5),          # reduce margin inside legend box
        legend.box.margin = margin(t = -10)       # reduce margin between legend and plots
  )
# Add single title on top
combined_plot  <- combined_plot + plot_annotation(title = outlab)

ggsave(paste0("./gen/visualizations/uncert-int/allmodels-", outcome,"_", format(Sys.Date(), "%Y%m%d"), ".png"), combined_plot, width = 12, height = 7, units = "in", dpi = 300)
