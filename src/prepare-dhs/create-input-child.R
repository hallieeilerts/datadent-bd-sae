################################################################################
#' @description Combine variables for child-level model
#' @return csv with child-level outcomes and covariates
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/util.R")
kr <- readRDS("./gen/prepare-dhs/temp/variables-kr.rds")
ir <- readRDS("./gen/prepare-dhs/temp/variables-ir.rds")
hr <- readRDS("./gen/prepare-dhs/temp/variables-hr.rds")
################################################################################

# only keep identifiers and covariates from mother and household files
mother_covar <- ir %>%
  select(v001, v002, mother_ln, mother_edu)
hhd_covar <- hr %>%
  select(hv001, hv002, hhd_mem, hhd_under5, hhd_head_sex, hhd_head_age, wealth_index)

# merge
dat <- kr %>%
  left_join(mother_covar, by = c("v001", "v002", "mother_ln")) %>%
  left_join(hhd_covar, by = c("v001" = "hv001", "v002" = "hv002"))

# Save --------------------------------------------------------------------

write.csv(dat, file = "./gen/prepare-dhs/output/dat-child.csv", row.names = FALSE)

