################################################################################
#' @description Calculate indicator for Vitamin A supplementation
#' nt_ch_micro_vas: Children age 6-59 mos given Vit. A supplements
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(haven)
library(tidyverse)
#' Inputs
source("./src/util.R")
## DHS child recode
dat <- read_dta("./data/BD_2022_DHS_03042025_2114_120781/BDKR81DT/BDKR81FL.DTA")
## District names for clusters
clusters <- read.csv("./gen/cluster-districts.csv")
## Set covariate metadata
covariate_metadata <- configure_covariate(
  name = "vas_ch",
  scale = "zero_one",
  label = "	Percentage of children age 6-59 months who received vitamin A supplements in the six months preceding the survey",
  dhs_indicator_code = c("CN_MIAC_C_VAS")
)
################################################################################

dat_prep <- fn_prepKR(dat)

# merge on districts to clusters
nrow(dat_prep) # 8784
dat_prep <- merge(dat_prep, clusters, by.x = "v001", by.y = "DHSCLUSTER")
nrow(dat_prep) # 8784

# create variable
dat_var <- fn_gen_nt_ch_micro_vas(dat_prep)

# calculate weighted proportions for categorical variable
prop_adm0 <- fn_wtd_prop(dat_var, adminlevel = "adm0")
prop_adm1 <- fn_wtd_prop(dat_var, adminlevel = "adm1")
prop_adm2 <- fn_wtd_prop(dat_var, adminlevel = "adm2")
dat_prop <- rbind(prop_adm0, prop_adm1, prop_adm2)

# Subset group that received intervention
dat_ind <- subset(dat_prop, var == "Yes")
dat_ind$var <- NULL

# Format
dat_formatted <- fn_format(dat_ind, covariate_metadata)

# Save --------------------------------------------------------------------

write_output_data(dat_formatted, covariate_metadata)
