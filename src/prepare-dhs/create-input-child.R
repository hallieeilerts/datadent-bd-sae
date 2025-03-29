################################################################################
#' @description Combine variables for child-level model
#' @return 
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

# only keep identifiers and covariates from mother file
mother_covar <- ir %>%
  select(v001, v002, mother_ln, mother_edu)

# merge child outcomes (and covariates) to mother covariates
dat <- kr %>%
  left_join(mother_covar, by = c("v001", "v002", "mother_ln")) 

# merge to household covariates
dat <- dat %>%
  left_join(hr, by = c("v001" = "hv001", "v002" = "hv002"))

# Save --------------------------------------------------------------------

write.csv(dat, file = "./gen/prepare-dhs/output/dat-child.csv", row.names = FALSE)

