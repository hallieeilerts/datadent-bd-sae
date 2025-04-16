################################################################################
#' @description Create district level input
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/util.R")
ir <- readRDS("./gen/prepare-dhs/temp/variables-ir.rds")
hr <- readRDS("./gen/prepare-dhs/temp/variables-hr.rds")
direct <- read.csv("./gen/calculate-direct/output/direct-estimates.csv")
################################################################################

# do proportion rural
residence <- fn_wtd_prop2(ir, "residence")
residence <- residence %>%
  filter(varname %in% c("rural")) %>%
  rename(residence = per) %>%
  select(-varname)

# do proportion above secondary
mother_edu <- fn_wtd_prop2(ir, "mother_edu")
mother_edu <- mother_edu %>%
  filter(varname %in% c(2,3)) %>%
  group_by(district_name) %>%
  summarise(mother_edu = sum(per))

# do proportion in top two quintiles
wealth_index <- fn_wtd_prop2(hr, "wealth_index")
wealth_index <- wealth_index %>%
  filter(varname %in% c(4,5)) %>%
  group_by(district_name) %>%
  summarise(wealth_index = sum(per))

dat <- subset(direct, admin_level == "adm2")
nrow(dat)
dat <- merge(direct, residence, by = "district_name")
dat <- merge(dat, mother_edu, by = "district_name")
dat <- merge(dat, wealth_index, by = "district_name")
nrow(dat)

# Save --------------------------------------------------------------------

write.csv(dat, file = "./gen/prepare-dhs/output/dat-district.csv", row.names = FALSE)


