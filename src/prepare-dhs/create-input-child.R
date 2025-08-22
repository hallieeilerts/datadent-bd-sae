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
kr2017 <- readRDS("./gen/prepare-dhs/temp/variables-kr-2017.rds")
ir2017 <- readRDS("./gen/prepare-dhs/temp/variables-ir-2017.rds")
hr2017 <- readRDS("./gen/prepare-dhs/temp/variables-hr-2017.rds")
################################################################################

# only keep identifiers and covariates from mother and household files (no covariates in births file)
mother_covar <- ir %>%
  select(v001, v002, mother_ln, mother_edu)
hhd_covar <- hr %>%
  select(hv001, hv002, hhd_mem, hhd_under5, hhd_head_sex, hhd_head_age, wealth_index)

# merge on covariates from mother and household files
dat <- kr %>%
  left_join(mother_covar, by = c("v001", "v002", "mother_ln")) %>%
  left_join(hhd_covar, by = c("v001" = "hv001", "v002" = "hv002"))

mother_covar2017 <- ir2017 %>%
  select(v001, v002, mother_ln, mother_edu)
hhd_covar2017 <- hr2017 %>%
  select(hv001, hv002, hhd_mem, hhd_under5, hhd_head_sex, hhd_head_age, wealth_index)
dat2017 <- kr2017 %>%
  left_join(mother_covar2017, by = c("v001", "v002", "mother_ln")) %>%
  left_join(hhd_covar2017, by = c("v001" = "hv001", "v002" = "hv002"))

# combine 2022 and 2017
# convert v23 factors to character and rbind
dat$v023 <- as.character(dat$v023)
dat2017$v023 <- as.character(dat2017$v023)
dat <- bind_rows(dat, dat2017)

# Save --------------------------------------------------------------------

write.csv(dat, file = "./gen/prepare-dhs/output/dat-child.csv", row.names = FALSE)

