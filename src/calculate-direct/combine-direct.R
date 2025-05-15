################################################################################
#' @description Combine direct estimates for mother and child
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/util.R")
# direct estimates for children
direct_child <- read.csv("./gen/calculate-direct/temp/direct-child.csv")
# direct estimates for mothers
direct_mother <- read.csv("./gen/calculate-direct/temp/direct-mother.csv")
# adm1
direct_child_adm1 <- read.csv("./gen/calculate-direct/temp/direct-child-adm1.csv")
direct_mother_adm1 <- read.csv("./gen/calculate-direct/temp/direct-mother-adm1.csv")
# adm0
direct_child_adm0 <- read.csv("./gen/calculate-direct/temp/direct-child-adm0.csv")
direct_mother_adm0 <- read.csv("./gen/calculate-direct/temp/direct-mother-adm0.csv")
################################################################################

direct <- rbind(direct_child, direct_mother)
direct_adm1 <- rbind(direct_child_adm1, direct_mother_adm1)
direct_adm0 <- rbind(direct_child_adm0, direct_mother_adm0)

# Save --------------------------------------------------------------------

write.csv(direct, file = "./gen/calculate-direct/output/direct-estimates.csv", row.names = FALSE)
write.csv(direct_adm1, file = "./gen/calculate-direct/audit/direct-estimates-adm1.csv", row.names = FALSE)
write.csv(direct_adm0, file = "./gen/calculate-direct/audit/direct-estimates-adm0.csv", row.names = FALSE)
