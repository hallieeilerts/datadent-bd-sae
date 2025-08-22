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
ir2014 <- readRDS("./gen/prepare-dhs/temp/variables-ir-2014.rds")
hr2014 <- readRDS("./gen/prepare-dhs/temp/variables-hr-2014.rds")
################################################################################

# only keep identifiers and covariates from household file (no covariates in child and births files)
hhd_covar <- hr %>%
  dplyr::select(hv001, hv002, hhd_mem, hhd_under5, hhd_head_sex, hhd_head_age, wealth_index)

# merge
dat <- ir %>%
  left_join(hhd_covar, by = c("v001" = "hv001", "v002" = "hv002"))

hhd_covar2014 <- hr2014 %>%
  dplyr::select(hv001, hv002, hhd_mem, hhd_under5, hhd_head_sex, hhd_head_age, wealth_index)
dat2014 <- ir2014 %>%
  left_join(hhd_covar2014, by = c("v001" = "hv001", "v002" = "hv002"))

# combine 2022 and 2017
# convert v23 factors to character and rbind
dat$v023 <- as.character(dat$v023)
dat2014$v023 <- as.character(dat2014$v023)
dat <- bind_rows(dat, dat2014)

# Save --------------------------------------------------------------------

write.csv(dat, file = "./gen/prepare-dhs/output/dat-mother.csv", row.names = FALSE)

