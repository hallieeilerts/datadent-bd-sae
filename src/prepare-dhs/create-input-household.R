################################################################################
#' @description Combine variables for household-level model
#' @return csv with household-level outcomes and covariates
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/util.R")
ir <- readRDS("./gen/prepare-dhs/temp/variables-ir.rds")
hr <- readRDS("./gen/prepare-dhs/temp/variables-hr.rds")
################################################################################

# only keep identifiers and covariates from mother file (no covariates in child and births files)
mother_covar <- ir %>%
  select(v001, v002, mother_ln, mother_edu)

# merge
dat <- hr %>%
  left_join(mother_covar, by = c("hv001" = "v001", "hv002" = "v002"))

# Save --------------------------------------------------------------------

write.csv(dat, file = "./gen/prepare-dhs/output/dat-household.csv", row.names = FALSE)

