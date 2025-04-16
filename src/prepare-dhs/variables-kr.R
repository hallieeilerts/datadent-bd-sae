################################################################################
#' @description Calculate child-level variables for model input
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
## DHS child recode
dat <- read_dta("./data/BD_2022_DHS_03042025_2114_120781/BDKR81DT/BDKR81FL.DTA")
## District names for clusters
clusters <- read.csv("./gen/prepare-shp/output/cluster-locations.csv")
################################################################################

# calculate sampling weight
dat$wt <- dat$v005/1000000

# merge on district names
dat <- merge(dat, clusters, by.x = "v001", by.y = "DHSCLUSTER")

# calculate outcome variables
dat_var <- fn_gen_nt_ch_micro_vas(dat)
dat_var <- fn_gen_nt_ch_micro_dwm(dat_var)
#dat_var <- fn_gen_ch_meas_either(dat_var)
#dat_var <- fn_gen_ch_rotav3_either(dat_var)
dat_var <- fn_gen_nt_ebf(dat_var)

dat_var <- dat_var %>%
  select(ADM2_EN, v001, v002, v003, v023, v024, v025, b16, 
         nt_ch_micro_vas, nt_ch_micro_dwm, nt_ebf, wt) %>%
  mutate(region_name = as.character(as_factor(v024))) %>%
  mutate(residence = as.character(as_factor(v025))) %>%
  mutate(hhid = paste(v001, v002, sep = "_")) %>%
  rename(child_ln = b16,
         mother_ln = v003) %>%
  select(-v024)

# Save --------------------------------------------------------------------

saveRDS(dat_var, file = "./gen/prepare-dhs/temp/variables-kr.rds")

