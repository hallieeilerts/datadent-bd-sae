################################################################################
#' @description Combine variables for mother-level model
#' @return csv with mother-level outcomes and covariates
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/util.R")
ir <- readRDS("./gen/prepare-dhs/temp/variables-ir.rds")
hr <- readRDS("./gen/prepare-dhs/temp/variables-hr.rds")
################################################################################

# only keep identifiers and covariates from household files
hhd_covar <- hr %>%
  select(hv001, hv002, hhd_mem, hhd_under5, hhd_head_sex, hhd_head_age, wealth_index)

# merge
dat <- ir %>%
  left_join(hhd_covar, by = c("v001" = "hv001", "v002" = "hv002"))

# Save --------------------------------------------------------------------

write.csv(dat, file = "./gen/prepare-dhs/output/dat-mother.csv", row.names = FALSE)

