################################################################################
#' @description Calculate error and overlap for aggregated predictions
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
direct_fd <- read.csv("./gen/calculate-direct/output/direct-estimates-focusdistricts.csv")
# aggregated adm2 predictions
agg <- read.csv("./gen/validation/temp/pred-agg.csv")
# model info
modinfo <- read.csv("./gen/model/audit/model-info.csv")
################################################################################

# subset to included indicators
df_ind <- subset(ind, status == "include")

# vector of indicators
v_var <- unique(df_ind$variable)

# adm1 --------------------------------------------------------------------

# calculate 95% confidence intervals for direct, length
dir_adm1 <- direct_adm1
dir_adm1$var <- dir_adm1$dir_var
dir_adm1$se <- sqrt(dir_adm1$dir_var)
dir_adm1$lb <- dir_adm1$dir - 1.96 * dir_adm1$se
dir_adm1$ub <- dir_adm1$dir + 1.96 * dir_adm1$se
dir_adm1$length95 <- dir_adm1$ub - dir_adm1$lb
dir_adm1$dhs_indicator_code <- dir_adm1$dir_var <- NULL

# create columns for aggregated
agg_adm1 <- subset(agg, admin_level == "adm1")
names(agg_adm1)[which(names(agg_adm1) == "qt_lb")] <- "lb"
names(agg_adm1)[which(names(agg_adm1) == "qt_ub")] <- "ub"
names(agg_adm1)[which(names(agg_adm1) == "post_mean")] <- "agg"
names(agg_adm1)[which(names(agg_adm1) == "post_var")] <- "var"
agg_adm1$se <- sqrt(agg_adm1$var)
agg_adm1 <- agg_adm1[,c("variable", "admin_level", "ADM0_EN", "ADM1_EN", "agg", "var", "se", "lb", "ub", "model", "cov")]

# merge
error_adm1 <- merge(dir_adm1, agg_adm1, by = c("variable", "ADM1_EN"), suffixes = c("_dir", "_agg"))

# calculate interval overlap
error_adm1$int_overlap <- check_overlap(error_adm1$lb_dir, error_adm1$ub_dir, error_adm1$lb_agg, error_adm1$ub_agg)

# calculate error
error_adm1$error <- error_adm1$agg - error_adm1$dir

# tidy
error_adm1 <- error_adm1 %>%
  select(variable, admin_level, ADM0_EN, ADM1_EN, 
         dir, var_dir, se_dir, lb_dir, ub_dir,
         obs_un, obs_wn, degf,
         agg, var_agg, se_agg, lb_agg, ub_agg,
         int_overlap, length95, error,
         model, cov)

# adm0 --------------------------------------------------------------------

# calculate 95% confidence intervals for direct, length
dir_adm0 <- direct_adm0
dir_adm0$var <- dir_adm0$dir_var
dir_adm0$se <- sqrt(dir_adm0$dir_var)
dir_adm0$lb <- dir_adm0$dir - 1.96 * dir_adm0$se
dir_adm0$ub <- dir_adm0$dir + 1.96 * dir_adm0$se
dir_adm0$length95 <- dir_adm0$ub - dir_adm0$lb
dir_adm0$dhs_indicator_code <- dir_adm0$dir_var <- NULL

# create columns for aggregated
agg_adm0 <- subset(agg, admin_level == "adm0")
names(agg_adm0)[which(names(agg_adm0) == "qt_lb")] <- "lb"
names(agg_adm0)[which(names(agg_adm0) == "qt_ub")] <- "ub"
names(agg_adm0)[which(names(agg_adm0) == "post_mean")] <- "agg"
names(agg_adm0)[which(names(agg_adm0) == "post_var")] <- "var"
agg_adm0$se <- sqrt(agg_adm0$var)
agg_adm0 <- agg_adm0[,c("variable", "admin_level", "ADM0_EN", "ADM1_EN", "agg", "var", "se", "lb", "ub", "model", "cov")]

# merge
error_adm0 <- merge(dir_adm0, agg_adm0, by = c("variable"), suffixes = c("_dir", "_agg"))

# calculate interval overlap
error_adm0$int_overlap <- check_overlap(error_adm0$lb_dir, error_adm0$ub_dir, error_adm0$lb_agg, error_adm0$ub_agg)

# calculate error
error_adm0$error <- error_adm0$agg - error_adm0$dir

# tidy
error_adm0 <- error_adm0 %>%
  select(variable, admin_level, ADM0_EN, ADM1_EN, 
         dir, var_dir, se_dir, lb_dir, ub_dir,
         obs_un, obs_wn, degf,
         agg,  var_agg, se_agg, lb_agg, ub_agg,
         int_overlap, length95, error,
         model, cov)


# Focus districts ---------------------------------------------------------

# calculate 95% confidence intervals for direct, length
dir_fd <- direct_fd
dir_fd$var <- direct_fd$dir_var
dir_fd$se <- sqrt(dir_fd$dir_var)
dir_fd$lb <- direct_fd$dir - 1.96 * dir_fd$se
dir_fd$ub <- dir_fd$dir + 1.96 * dir_fd$se
dir_fd$length95 <- dir_fd$ub - dir_fd$lb
dir_fd$dhs_indicator_code <- dir_fd$dir_var <- NULL

# create columns for aggregated
agg_fd <- subset(agg, admin_level == "focus-districts")
names(agg_fd)[which(names(agg_fd) == "qt_lb")] <- "lb"
names(agg_fd)[which(names(agg_fd) == "qt_ub")] <- "ub"
names(agg_fd)[which(names(agg_fd) == "post_mean")] <- "agg"
names(agg_fd)[which(names(agg_fd) == "post_var")] <- "var"
agg_fd$se <- sqrt(agg_fd$var)
agg_fd <- agg_fd[,c("variable", "admin_level", "ADM0_EN", "ADM1_EN", "agg", "var", "se", "lb", "ub", "model", "cov")]

# merge
error_fd <- merge(dir_fd, agg_fd, by = c("variable"), suffixes = c("_dir", "_agg"))

# calculate interval overlap
error_fd$int_overlap <- check_overlap(error_fd$lb_dir, error_fd$ub_dir, error_fd$lb_agg, error_fd$ub_agg)

# calculate error
error_fd$error <- error_fd$agg - error_fd$dir

# tidy
error_fd <- error_fd %>%
  select(variable, admin_level, ADM0_EN, ADM1_EN, 
         dir, var_dir, se_dir, lb_dir, ub_dir,
         obs_un, obs_wn, degf,
         agg,  var_agg, se_agg, lb_agg, ub_agg,
         int_overlap, length95, error,
         model, cov)


# identify models with minimum error at adm1 ------------------------------

minerror_adm1 <- error_adm1 %>% 
  group_by(variable, model) %>%
  mutate(n = n(),
         sqerror = (error)^2) %>%
  summarise(RMSE = mean(sqerror)) %>%
  group_by(variable) %>%
  mutate(minerror = min(RMSE)) %>%
  filter(RMSE == minerror) %>%
  select(variable, model)

# save output(s) ----------------------------------------------------------

write.csv(error_adm1, file = "./gen/validation/output/agg-error-adm1.csv", row.names = FALSE)
write.csv(error_adm0, file = "./gen/validation/output/agg-error-adm0.csv", row.names = FALSE)
write.csv(error_fd, file = "./gen/validation/output/agg-error-fd.csv", row.names = FALSE)
write.csv(minerror_adm1, file = "./gen/validation/output/agg-minerror-adm1.csv", row.names = FALSE)
