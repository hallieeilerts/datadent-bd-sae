################################################################################
#' @description Calculate error and overlap for aggregated predictions, plot
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(stringr)
#' Inputs
source("./src/util.R")
# direct adm1 estimates
direct <- read.csv("./gen/calculate-direct/audit/direct-estimates-adm1.csv")
# aggregated adm2 predictions
agg <- read.csv("./gen/validation/temp/agg-adm2-pred.csv")
# model info
modinfo <- read.csv("./gen/model/audit/model-info.csv")
# Bangladesh division boundaries
bangladesh_1 <- st_read("./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm1_bbs_20201113")
################################################################################

# calculate 95% confidence intervals for direct, length
direct$se <- sqrt(direct$dir_var)
direct$lb <- direct$dir - 1.96 * direct$se
direct$ub <- direct$dir + 1.96 * direct$se
direct$length95 <- direct$ub - direct$lb
direct$dhs_indicator_code <- direct$dir_var <- direct$se <- NULL

# create columns for aggregated
names(agg)[which(names(agg) == "qt_lb")] <- "lb"
names(agg)[which(names(agg) == "qt_ub")] <- "ub"
names(agg)[which(names(agg) == "post_mean")] <- "agg"
# merge on outcome
agg <- merge(agg, modinfo[,c("file","outcome", "vers", "test")], by = "file")
names(agg)[which(names(agg) == "outcome")] <- "variable"
agg$plotlabel <- paste0(str_pad(agg$vers, width = 3, side = "left", pad = "0"), "-", agg$test)

# merge
dat <- merge(direct, agg, by = c("variable", "ADM1_EN"), suffixes = c("_dir", "_agg"))

# calculate interval overlap
dat$int_overlap <- check_overlap(dat$lb_dir, dat$ub_dir, dat$lb_agg, dat$ub_agg)

# calculate error
dat$error <- dat$agg - dat$dir

# Plot RMSE
p1 <- dat %>% 
  group_by(variable, plotlabel) %>%
  mutate(n = n(),
         sqerror = (error)^2) %>%
  summarise(RMSE = mean(sqerror)) %>%
  ggplot() +
  geom_bar(aes(x=plotlabel, y = RMSE), stat = "identity") +
  facet_wrap(~variable, ncol = 1, scales = "free_y") +
  labs(x = "model", title = "Error") +
  theme_bw() +
  theme(text = element_text(size = 10)) +
  coord_flip()
ggsave(paste0("./gen/validation/audit/error.png"), p1, width = 5.5, height = 10, units = "in", dpi = 300)

# Plot overlap
v_outcomes <- c("nt_ch_micro_vas", "nt_ch_micro_dwm", "nt_ebf", "nt_wm_micro_iron", "rh_anc_4vs", "rh_anc_1tri")
outcome1 <- v_outcomes[3]
plotdat <- bangladesh_1 %>% 
  left_join(dat, by = "ADM1_EN")
p2 <- ggplot() +
  geom_sf(data = subset(plotdat, variable == outcome1), aes(fill = int_overlap)) +
  geom_sf(data = bangladesh_1, color = "black", fill = NA) +
  labs(title = outcome1, subtitle = "Coverage") +
  scale_fill_manual(values = c("TRUE" = "#00BFC4", "FALSE" = "#F8766D")) +
  theme_void() +
  theme(
    text = element_text(size = 10),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  facet_wrap(~plotlabel, nrow = 2)
ggsave(paste0("./gen/validation/audit/overlap-", outcome1, ".png"), p2, width = 12, height = 6)

# Plot uncertainty intervals
v_outcomes <- c("nt_ch_micro_vas", "nt_ch_micro_dwm", "nt_ebf", "nt_wm_micro_iron", "rh_anc_4vs", "rh_anc_1tri")
outcome2 <- v_outcomes[3]
direct$name <- "Direct"
agg$name <- agg$plotlabel
dat2 <- bind_rows(direct, agg)
dat2$value <- ifelse(!is.na(dat2$dir), dat2$dir, dat2$agg)
p3 <- dat2 %>%
  filter(variable %in% outcome2) %>%
  ggplot(aes(x = ADM1_EN, y = value, color = name, group = name)) +
  geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.8), width = 0.2) +
  geom_point(position = position_dodge(width = 0.8)) +
  labs(x = "", y = "", title = outcome2) +
  theme_bw() +
  coord_flip() +
  theme(text = element_text(size = 10), legend.title=element_blank()) +
  scale_color_discrete(guide = guide_legend(reverse = TRUE)) 

ggsave(paste0("./gen/validation/audit/uncert-int-", outcome2,".png"), p3, width = 5, height = 5.5, units = "in", dpi = 300)

