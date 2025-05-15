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

# calculate sampling weight
dat$wt <- dat$hv005/1000000

# merge on district names
dat <- merge(dat, clusters, by.x = "hv001", by.y = "DHSCLUSTER")

dat <- dat %>%
  select(ADM2_EN, ADM1_EN, hv001, hv002, hv009, hv014, hv023, hv219, hv220, hv270, wt) %>%
  mutate(hhd_head_sex = as.character(as_factor(hv219)),
         wealth_index = as.character(as_factor(hv270)),
         hhd_head_age = as.character(as_factor(hv220))) %>%
  rename(hhd_mem = hv009,
         hhd_under5 = hv014) %>%
  select(-c(hv219, hv270, hv220))

# Save --------------------------------------------------------------------

saveRDS(dat, file = "./gen/prepare-dhs/temp/variables-hr.rds")
