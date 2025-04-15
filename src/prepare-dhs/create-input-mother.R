################################################################################
#' @description Combine variables for mother-level model
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

# only keep identifiers and covariates from child file
# none so far
# child_covar <- kr %>%
#   select(v001, v002, mother_ln, COVAR?)

# merge mother outcomes (and covariates) to child covariates
# dat <- ir %>%
#   left_join(mother_covar, by = c("v001", "v002", "mother_ln")) 

# !!!NOTE: need to remove wt and district from hr file!!!

# merge mother covariates and outcomes to household variables
hr_merge <- hr %>%
  select(-c(district, wt))
dat <- ir %>%
  left_join(hr_merge, by = c("v001" = "hv001", "v002" = "hv002"))

# Save --------------------------------------------------------------------

write.csv(dat, file = "./gen/prepare-dhs/output/dat-mother.csv", row.names = FALSE)

