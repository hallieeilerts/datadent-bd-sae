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
################################################################################

direct <- rbind(direct_child, direct_mother)

# create confidence interval
direct$lower <- direct$value - 1.96 * direct$se
direct$upper <- direct$value + 1.96 * direct$se

# Save --------------------------------------------------------------------

write.csv(direct, file = "./gen/calculate-direct/output/direct-estimates.csv", row.names = FALSE)

