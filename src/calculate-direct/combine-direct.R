################################################################################
#' @description Combine direct estimates for mother and child
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
#' Inputs
source("./src/util.R")
# adm2 for modeling
direct_child <- read.csv("./gen/calculate-direct/temp/direct-child.csv")
direct_mother <- read.csv("./gen/calculate-direct/temp/direct-mother.csv")
direct_household <- read.csv("./gen/calculate-direct/temp/direct-household.csv")
direct_birth <- read.csv("./gen/calculate-direct/temp/direct-birth.csv")
# adm2 for plotting
direct_child_forplot <- read.csv("./gen/calculate-direct/temp/direct-child-forplot.csv")
# adm1
direct_child_adm1 <- read.csv("./gen/calculate-direct/temp/direct-child-adm1.csv")
direct_mother_adm1 <- read.csv("./gen/calculate-direct/temp/direct-mother-adm1.csv")
direct_household_adm1 <- read.csv("./gen/calculate-direct/temp/direct-household-adm1.csv")
direct_birth_adm1 <- read.csv("./gen/calculate-direct/temp/direct-birth-adm1.csv")
# adm0
direct_child_adm0 <- read.csv("./gen/calculate-direct/temp/direct-child-adm0.csv")
direct_mother_adm0 <- read.csv("./gen/calculate-direct/temp/direct-mother-adm0.csv")
direct_household_adm0 <- read.csv("./gen/calculate-direct/temp/direct-household-adm0.csv")
direct_birth_adm0 <- read.csv("./gen/calculate-direct/temp/direct-birth-adm0.csv")
################################################################################

# merge direct estimates for plotting
names(direct_child_forplot)[which(names(direct_child_forplot) == "dir")] <- "dirplot"
names(direct_child_forplot)[which(names(direct_child_forplot) == "dir_var")] <- "dirplot_var"
direct_child_forplot <- direct_child_forplot %>% 
  dplyr::select(variable, ADM2_EN, dirplot, dirplot_var)
direct_child_m <- merge(direct_child, direct_child_forplot, by = c("variable", "ADM2_EN"))
direct <- bind_rows(direct_child_m, direct_mother, direct_household, direct_birth)

direct_adm1 <- rbind(direct_child_adm1, direct_mother_adm1, direct_household_adm1, direct_birth_adm1)
direct_adm0 <- rbind(direct_child_adm0, direct_mother_adm0, direct_household_adm0, direct_birth_adm0)

# Save --------------------------------------------------------------------

write.csv(direct, file = "./gen/calculate-direct/output/direct-estimates.csv", row.names = FALSE)
write.csv(direct_adm1, file = "./gen/calculate-direct/audit/direct-estimates-adm1.csv", row.names = FALSE)
write.csv(direct_adm0, file = "./gen/calculate-direct/audit/direct-estimates-adm0.csv", row.names = FALSE)
