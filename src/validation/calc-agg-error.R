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
ind <- read_excel("./data/ind-info.xlsx", sheet = "indicators")
# direct estimates
direct_adm1 <- read.csv("./gen/calculate-direct/output/direct-estimates-adm1.csv")
direct_adm0 <- read.csv("./gen/calculate-direct/output/direct-estimates-adm0.csv")
# aggregated adm2 predictions
#agg <- read.csv("./gen/validation/temp/agg-adm2-pred.csv")
agg <- read.csv("./gen/validation/output/pred-agg.csv")
# model info
modinfo <- read.csv("./gen/model/audit/model-info.csv")
# Bangladesh division boundaries
bangladesh_1 <- st_read("./data/bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm1_bbs_20201113")
################################################################################

# subset to included indicators
df_ind <- subset(ind, status == "include")

# vector of indicators
v_var <- unique(df_ind$variable)

# adm1 --------------------------------------------------------------------

# calculate 95% confidence intervals for direct, length
dir_adm1 <- direct_adm1
dir_adm1$se <- sqrt(dir_adm1$dir_var)
dir_adm1$lb <- dir_adm1$dir - 1.96 * dir_adm1$se
dir_adm1$ub <- dir_adm1$dir + 1.96 * dir_adm1$se
dir_adm1$length95 <- dir_adm1$ub - dir_adm1$lb
dir_adm1$dhs_indicator_code <- dir_adm1$dir_var <- dir_adm1$se <- NULL

# create columns for aggregated
agg_adm1 <- subset(agg, admin_level == "adm1")
names(agg_adm1)[which(names(agg_adm1) == "qt_lb")] <- "lb"
names(agg_adm1)[which(names(agg_adm1) == "qt_ub")] <- "ub"
names(agg_adm1)[which(names(agg_adm1) == "post_mean")] <- "agg"
agg_adm1 <- agg_adm1[,c("variable", "ADM1_EN", "agg", "lb", "ub")]

# merge
dat <- merge(dir_adm1, agg_adm1, by = c("variable", "ADM1_EN"), suffixes = c("_dir", "_agg"))

# calculate interval overlap
dat$int_overlap <- check_overlap(dat$lb_dir, dat$ub_dir, dat$lb_agg, dat$ub_agg)

# calculate error
dat$error <- dat$agg - dat$dir

# Plot RMSE
p1 <- dat %>% 
  group_by(variable) %>%
  mutate(n = n(),
         sqerror = (error)^2) %>%
  summarise(RMSE = mean(sqerror)) %>%
  ggplot() +
  geom_bar(aes(x=variable, y = RMSE), stat = "identity") +
  labs(x = "model", title = "Error") +
  theme_bw() +
  theme(text = element_text(size = 10)) +
  coord_flip()
ggsave(paste0("./gen/validation/audit/adm1-error.png"), p1, width = 5.5, height = 10, units = "in", dpi = 300)


# Plot overlap
plotdat <- bangladesh_1 %>% 
  left_join(dat, by = "ADM1_EN")
for(i in 1:length(v_var)){
  myoutcome <- v_var[i]
  p2 <- ggplot() +
    geom_sf(data = subset(plotdat, variable == myoutcome), aes(fill = int_overlap)) +
    geom_sf(data = bangladesh_1, color = "black", fill = NA) +
    labs(title = myoutcome, subtitle = "Interval overlap") +
    scale_fill_manual(values = c("TRUE" = "#00BFC4", "FALSE" = "#F8766D")) +
    theme_void() +
    theme(
      text = element_text(size = 10),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    ) 
  ggsave(paste0("./gen/validation/audit/adm1-overlap-", myoutcome, ".png"), p2, width = 12, height = 6)
}

# Plot uncertainty intervals
dat2 <- bind_rows(dir_adm1 %>% mutate(name = "Direct"), agg_adm1 %>% mutate(name = "agg"))
dat2$value <- ifelse(!is.na(dat2$dir), dat2$dir, dat2$agg)
# merge on int_overlap
dat2 <- dat2 %>%
  left_join(dat %>% select(variable, ADM1_EN, int_overlap) %>% distinct(), by = c("variable", "ADM1_EN")) %>%
  mutate(name = ifelse(name == "agg", "Aggregated adm2", name)) %>%
  mutate(int_overlap = ifelse(int_overlap == TRUE, "Overlap", "No overlap"))
nrow(subset(dat2, int_overlap == "Overlap" & name == "Aggregated adm2")) # 221
length(unique(subset(dat2, int_overlap == "No overlap")$variable)) # 2
length(unique(dat2$variable)) # 28

p3 <- dat2 %>%
  ggplot(aes(x = ADM1_EN, y = value, color = name, group = name)) +
  geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.8), width = 0.2) +
  geom_point(aes(shape = int_overlap), position = position_dodge(width = 0.8)) +
  labs(x = "", y = "", title = "Comparison of direct adm1 and modeled-aggregated adm2") +
  theme_bw() +
  coord_flip() +
  theme(text = element_text(size = 10), legend.title=element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_shape_manual(values = c(8, 16)) +
  scale_color_discrete(guide = guide_legend(reverse = TRUE)) +
  facet_wrap(~variable) 

ggsave(paste0("./gen/validation/audit/adm1-uncert-int.png"), p3, width = 10, height = 5.5, units = "in", dpi = 300)

# adm0 --------------------------------------------------------------------

# calculate 95% confidence intervals for direct, length
dir_adm0 <- direct_adm0
dir_adm0$se <- sqrt(dir_adm0$dir_var)
dir_adm0$lb <- dir_adm0$dir - 1.96 * dir_adm0$se
dir_adm0$ub <- dir_adm0$dir + 1.96 * dir_adm0$se
dir_adm0$length95 <- dir_adm0$ub - dir_adm0$lb
dir_adm0$dhs_indicator_code <- dir_adm0$dir_var <- dir_adm0$se <- NULL

# create columns for aggregated
agg_adm0 <- subset(agg, admin_level == "adm0")
names(agg_adm0)[which(names(agg_adm0) == "qt_lb")] <- "lb"
names(agg_adm0)[which(names(agg_adm0) == "qt_ub")] <- "ub"
names(agg_adm0)[which(names(agg_adm0) == "post_mean")] <- "agg"

# merge
dat <- merge(dir_adm0, agg_adm0, by = c("variable"), suffixes = c("_dir", "_agg"))

# calculate interval overlap
dat$int_overlap <- check_overlap(dat$lb_dir, dat$ub_dir, dat$lb_agg, dat$ub_agg)

# calculate error
dat$error <- dat$agg - dat$dir

# Plot RMSE
p1 <- dat %>% 
  group_by(variable) %>%
  mutate(n = n(),
         sqerror = (error)^2) %>%
  summarise(RMSE = mean(sqerror)) %>%
  ggplot() +
  geom_bar(aes(x=variable, y = RMSE), stat = "identity") +
  labs(x = "model", title = "Error") +
  theme_bw() +
  theme(text = element_text(size = 10)) +
  coord_flip()
ggsave(paste0("./gen/validation/audit/adm0-error.png"), p1, width = 5.5, height = 10, units = "in", dpi = 300)

# overlap
table(dat$int_overlap)

# Plot uncertainty intervals
dat2 <- bind_rows(dir_adm0 %>% mutate(name = "Direct"), agg_adm0 %>% mutate(name = "agg"))
dat2$value <- ifelse(!is.na(dat2$dir), dat2$dir, dat2$agg)
p3 <- dat2 %>%
  ggplot(aes(x = variable, y = value, color = name, group = name)) +
  geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.8), width = 0.2) +
  geom_point(position = position_dodge(width = 0.8)) +
  labs(x = "", y = "") +
  theme_bw() +
  coord_flip() +
  theme(text = element_text(size = 10), legend.title=element_blank()) +
  scale_color_discrete(guide = guide_legend(reverse = TRUE))
ggsave(paste0("./gen/validation/audit/adm0-uncert-int.png"), p3, width = 12, height = 10, units = "in", dpi = 300)

