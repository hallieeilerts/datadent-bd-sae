################################################################################
#' @description Calculate direct estimates of mother-level outcomes
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/util.R")
dat <- read.csv("./gen/prepare-dhs/output/dat-mother.csv")
################################################################################

dat_iron <- fn_var_taylor(dat, "nt_wm_micro_iron")
dat_iron$dhs_indicator_code <- NA
# NA because i changed it to a binary indicator for <90 days or 90+ days

dat_anc4 <- fn_var_taylor(dat, "rh_anc_4vs")
dat_anc4$dhs_indicator_code <- "RH_ANCN_W_N4P"

dat_anc1tri <- fn_var_taylor(dat, "rh_anc_1tri")
dat_anc1tri$dhs_indicator_code <- NA


# rbind results if multiple
res <- rbind(dat_iron, dat_anc4, dat_anc1tri)

# Save --------------------------------------------------------------------

write.csv(res, file = "./gen/calculate-direct/temp/direct-mother.csv", row.names = FALSE)

