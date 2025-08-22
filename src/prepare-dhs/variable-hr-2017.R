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
dat <- read_dta("./data/BD_2017-18_DHS_08202025_153_120781/BDHR7RDT/BDHR7RFL.DTA")
## District names for clusters
clusters <- read.csv("./gen/prepare-shp/output/cluster-locations-2017.csv")
################################################################################

# calculate sampling weight
dat$wt <- dat$hv005/1000000

# merge on district names
dat <- merge(dat, clusters, by.x = "hv001", by.y = "DHSCLUSTER")

# dat_var <- fn_gen_ph_wtr_basic(dat_var)

dat_var <- dat %>%
  select(ADM2_EN, ADM1_EN, hv001, hv002, hv009, hv014, hv023, hv219, hv220, hv270, 
         wt) %>%
  mutate(hhd_head_sex = as.character(as_factor(hv219)),
         wealth_index = as.character(as_factor(hv270)),
         hhd_head_age = as.character(as_factor(hv220))) %>%
  rename(hhd_mem = hv009,
         hhd_under5 = hv014) %>%
  select(-c(hv219, hv270, hv220))

# Save --------------------------------------------------------------------

saveRDS(dat_var, file = "./gen/prepare-dhs/temp/variables-hr-2017.rds")
