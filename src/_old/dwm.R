#	Children given deworming medication in past 6 months
#Percentage of children age 6-59 months who were given deworming medication in the six months preceding the survey
#CN_MIAC_C_DWM

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
library(survey)
#' Inputs
source("./src/util.R")
## DHS child recode
dat <- read_dta("./data/BD_2022_DHS_03042025_2114_120781/BDKR81DT/BDKR81FL.DTA")
## District names for clusters
clusters <- read.csv("./gen/prepare-shp/output/cluster-districts.csv")
## Set covariate metadata
covariate_metadata <- configure_covariate(
  name = "dwm",
  scale = "zero_one",
  label = "	Percentage of children age 6-59 months who were given deworming medication in the six months preceding the survey",
  dhs_indicator_code = c("CN_MIAC_C_DWM")
)
################################################################################

dat_prep <- fn_prepKR(dat)

# merge on shape file district names to clusters
nrow(dat_prep) # 8784
dat_prep <- merge(dat_prep, clusters, by.x = "v001", by.y = "DHSCLUSTER")
nrow(dat_prep) # 8784

# create variable
dat_var <- fn_gen_nt_ch_micro_dwm(dat_prep)

# calculate weighted proportions for categorical variable
dat_prop <- fn_var_taylor(dat_var, "nt_ch_micro_dwm")

# Format
dat_formatted <- fn_format_var(dat_prop, covariate_metadata)

# Save --------------------------------------------------------------------

write_output_data(dat_formatted, covariate_metadata)
