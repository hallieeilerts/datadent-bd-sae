################################################################################
#' @description Calculate household covariates for model input
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
dat <- read_dta("./data/BD_2022_DHS_03042025_2114_120781/BDHR81DT/BDHR81FL.DTA")
################################################################################

dat <- dat %>%
  select(hv001, hv002, hv009, hv014, hv219, hv220, hv270) %>%  # Cluster, Household ID, Wealth Index
  rename(wealth_index = hv270,
         hhd_mem = hv009,
         hhd_under5 = hv014,
         hhd_head_sex = hv219,
         hhd_head_age = hv220)

# Save --------------------------------------------------------------------

saveRDS(dat, file = "./gen/prepare-dhs/temp/variables-hr.rds")
