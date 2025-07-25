################################################################################
#' @description Aggregate adm2 predictions to adm1 level (single model/outcome at a time)
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(dplyr)
library(tidyr)
library(here)
library(cmdstanr)
#' Inputs
source("./src/util.R")
# Direct estimates and variance of outcomes
direct_adm2 <- read.csv("./gen/calculate-direct/output/direct-estimates.csv")
################################################################################

# choose indicator
# "nt_ch_micro_vas"  "nt_ch_micro_dwm"  "nt_ebf" "nt_wm_micro_iron" "rh_anc_4vs" "rh_anc_1tri" 
outcome <- "nt_ch_micro_vas"

# subset direct_adm2 estimates
ind_dat <- subset(direct_adm2, variable == outcome)
ind_dat <- ind_dat[order(ind_dat$ADM2_EN),]

# choose model
vers <- "050"
test <- "Test1"
model_name <- "ArealBYM2"
file_name <- paste(model_name, outcome, vers, test, sep = "-")

# read rds file that contains names of stan csv files
stan_csv_filenames <- readRDS(here("gen", "model", "fit", paste0("csv_files-fit-", file_name, ".rds")))

# read stan csv files as cmdstan object
stanfit <- as_cmdstan_fit(stan_csv_filenames)

# Model estimates 
df_p = stanfit$draws(format = "df", variables=c('p'),  inc_warmup = F) |> mutate(chain=as.character(.chain)) 
colnames(df_p) <- colnames(df_p) %>%
  gsub("\\[", "", .) %>%
  gsub("\\]", "", .) %>%
  gsub(",","_", .)

# MERGE THESE ONTO direct estimates that have adm1, adm2 labels, and adm2 weights (sum_wgt)
df_p_t <- as.data.frame(t(df_p[,1:nrow(ind_dat)]))
names(df_p_t) <- paste0("X", 1:ncol(df_p_t))
df_post <- cbind(ind_dat, df_p_t)

df_post_div <- df_post |> as.data.frame() |>
  group_by(ADM1_EN) |>
  summarise(across(all_of(colnames(df_post)[grepl('X',colnames(df_post))]), ~ weighted.mean(., w = sum_wgt)))

# compute mean and credible interval at state level 
alpha = 0.05
#postpred <- ind_dat
df_post_div$post_mean <- df_post_div[,2:(nrow(df_p)+1)] |> rowMeans()
df_quantile =  apply(df_post_div[,2:nrow(df_p)], 1 , quantile , probs = c(alpha/2,1-alpha/2) , na.rm = TRUE ) |> t()
df_post_div$qt_lb <- df_quantile[, 1]
df_post_div$qt_ub <- df_quantile[, 2]
df_post_div$length95 <- df_post_div$qt_ub - df_post_div$qt_lb
mean(df_post_div$length95)
df_post_div <- df_post_div |> select(-colnames(df_post)[grepl('X',colnames(df_post))])


# Compare with estimates calculated directly


# spreadsheet with "file" from model_info and df_post_div
