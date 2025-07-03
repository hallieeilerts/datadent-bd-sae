################################################################################
#' @description Calculate household-level covariates for model input
#' @return DHS dataset with shape file admin2 district name and other generated variables added
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(haven)
library(tidyverse)
library(naniar)
#' Inputs
source("./src/util.R")
## DHS household recode
dat <- read_dta("./data/BD_2022_DHS_03042025_2114_120781/BDHR81DT/BDHR81FL.DTA")
## District names for clusters
clusters <- read.csv("./gen/prepare-shp/output/cluster-locations.csv")
################################################################################

# Try to find variable based on labels
labels <- map_chr(dat, ~ attr(.x, "label"))
df_labels <- tibble(
  variable = names(labels),
  label = labels)
nrow(df_labels[grepl("mineral", df_labels$label),]) # 0
nrow(df_labels[grepl("vitam", df_labels$label),]) # 0
nrow(df_labels[grepl("supple", df_labels$label),]) # 0
nrow(df_labels[grepl("yesterday", df_labels$label),]) # 0
nrow(df_labels[grepl("24 hours", df_labels$label),]) # 17 smoking
df_labels[grepl("cash", df_labels$label),]
df_labels[grepl("assistance", df_labels$label),]
df_labels[grepl("social", df_labels$label),]
df_labels[grepl("protection", df_labels$label),]

# calculate sampling weight
dat$wt <- dat$hv005/1000000

# merge on district names
dat <- merge(dat, clusters, by.x = "hv001", by.y = "DHSCLUSTER")

# calculate outcome variables
dat_var <- fn_gen_ph_wtr_improve(dat)
dat_var <- fn_gen_ph_wtr_trt_appr(dat_var)
dat_var <- fn_gen_ph_sani_improve(dat_var)
dat_var <- fn_gen_ph_hndwsh_basic(dat_var)

# dat_var <- fn_gen_ph_wtr_basic(dat_var)

dat_var <- dat_var %>%
  select(ADM2_EN, ADM1_EN, hv001, hv002, hv009, hv014, hv023, hv219, hv220, hv270, 
         ph_wtr_improve, ph_wtr_trt_appr,
         ph_sani_improve, ph_hndwsh_basic, wt) %>%
  mutate(hhd_head_sex = as.character(as_factor(hv219)),
         wealth_index = as.character(as_factor(hv270)),
         hhd_head_age = as.character(as_factor(hv220))) %>%
  rename(hhd_mem = hv009,
         hhd_under5 = hv014) %>%
  select(-c(hv219, hv270, hv220))

# Save --------------------------------------------------------------------

saveRDS(dat_var, file = "./gen/prepare-dhs/temp/variables-hr.rds")
