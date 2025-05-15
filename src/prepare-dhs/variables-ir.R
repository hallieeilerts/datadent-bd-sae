################################################################################
#' @description Calculate mother-level variables for model input
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
## DHS individual recode
dat <- read_dta("./data/BD_2022_DHS_03042025_2114_120781/BDIR81DT/BDIR81FL.DTA")
## District names for clusters
clusters <- read.csv("./gen/prepare-shp/output/cluster-locations.csv")
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
  select(ADM2_EN, ADM1_EN, v001, v002, v003, v012, v023, v024, v025, v106, 
         nt_wm_micro_iron, rh_anc_4vs, rh_anc_1tri, wt) %>%
  mutate(mother_edu = as.character(as_factor(v106)),
         region_name = as.character(as_factor(v024)),
         residence = as.character(as_factor(v025))) %>%
  rename(mother_ln = v003,
         mother_age = v012) %>%
  select(-c(v106, v024, v025))

# Save --------------------------------------------------------------------

saveRDS(dat_var, file = "./gen/prepare-dhs/temp/variables-ir.rds")

