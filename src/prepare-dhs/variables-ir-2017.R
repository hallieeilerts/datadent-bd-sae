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
dat <- read_dta("./data/BD_2017-18_DHS_08202025_153_120781/BDIR7RDT/BDIR7RFL.DTA")
## District names for clusters
clusters <- read.csv("./gen/prepare-shp/output/cluster-locations-2017.csv")
################################################################################

# calculate sampling weight
dat$wt <- dat$v005/1000000

# merge on district names
dat <- merge(dat, clusters, by.x = "v001", by.y = "DHSCLUSTER")

dat_var <- dat %>%
  dplyr::select(ADM2_EN, ADM1_EN, v001, v002, v003, v012, v023, v024, v025, v106, 
                wt) %>%
  mutate(mother_edu = as.character(as_factor(v106)),
         region_name = as.character(as_factor(v024)),
         residence = as.character(as_factor(v025))) %>%
  rename(mother_ln = v003,
         mother_age = v012) %>%
  dplyr::select(-c(v106, v024, v025))

# Save --------------------------------------------------------------------

saveRDS(dat_var, file = "./gen/prepare-dhs/temp/variables-ir-2017.rds")

