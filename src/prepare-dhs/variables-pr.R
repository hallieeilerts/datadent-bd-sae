################################################################################
#' @description Calculate household member covariates for model input
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(haven)
library(tidyverse)
library(labelled)   # used for Haven labeled variable creation
#' Inputs
source("./src/util.R")
## DHS household recode
dat <- read_dta("./data/BD_2022_DHS_03042025_2114_120781/BDPR81DT/BDPR81FL.DTA")
## District names for clusters
clusters <- read.csv("./gen/prepare-shp/output/cluster-districts.csv")
################################################################################

# calculate sampling weight
dat$wt <- dat$hv005/1000000

# merge on district names
dat <- merge(dat, clusters, by.x = "hv001", by.y = "DHSCLUSTER")


# Save --------------------------------------------------------------------

#saveRDS(dat, file = "./gen/calculate-variables/temp/variables-pr.rds")
