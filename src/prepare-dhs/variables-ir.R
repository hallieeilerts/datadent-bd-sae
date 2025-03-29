################################################################################
#' @description Calculate mother-level variables for model input
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(haven)
library(tidyverse)
library(labelled)   # used for Haven labeled variable creation
library(naniar)
#' Inputs
source("./src/util.R")
## DHS individual recode
dat <- read_dta("./data/BD_2022_DHS_03042025_2114_120781/BDIR81DT/BDIR81FL.DTA")
## District names for clusters
clusters <- read.csv("./gen/prepare-shp/output/cluster-districts.csv")
################################################################################

# calculate sampling weight
dat$wt <- dat$v005/1000000

# merge on district names
dat <- merge(dat, clusters, by.x = "v001", by.y = "DHSCLUSTER")

# calculate outcome variables
dat_var <- fn_gen_nt_wm_micro_iron(dat)
dat_var <- fn_gen_rh_anc_4vs(dat_var)
dat_var <- fn_gen_rh_anc_1tri(dat_var)

dat_var <- dat_var %>%
  select(v001, v002, v003, v021, v022, v024, v025, district, v106, 
         nt_wm_micro_iron, rh_anc_4vs, rh_anc_1tri, wt) %>%
  mutate(region_name = haven::as_factor(v024)) %>%
  mutate(residence = haven::as_factor(v025)) %>%
  rename(mother_edu = v106,
         mother_ln = v003) %>%
  select(-v024)

# Save --------------------------------------------------------------------

saveRDS(dat_var, file = "./gen/prepare-dhs/temp/variables-ir.rds")

