################################################################################
#' @description Combine variables for birth-level model
#' @return csv with birth-level outcomes and covariates
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/util.R")
ir <- readRDS("./gen/prepare-dhs/temp/variables-ir.rds")
hr <- readRDS("./gen/prepare-dhs/temp/variables-hr.rds")
br <- readRDS("./gen/prepare-dhs/temp/variables-br.rds")
################################################################################

# only keep identifiers and covariates from mother and household files (no covariates in births file)
mother_covar <- ir %>%
  select(v001, v002, mother_ln, mother_edu)
hhd_covar <- hr %>%
  select(hv001, hv002, hhd_mem, hhd_under5, hhd_head_sex, hhd_head_age, wealth_index)

# merge on covariates from mother and household files
dat <- br %>%
  left_join(mother_covar, by = c("v001", "v002", "mother_ln")) %>%
  left_join(hhd_covar, by = c("v001" = "hv001", "v002" = "hv002"))

# Save --------------------------------------------------------------------

write.csv(dat, file = "./gen/prepare-dhs/output/dat-birth.csv", row.names = FALSE)
