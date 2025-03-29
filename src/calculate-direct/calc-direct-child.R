################################################################################
#' @description Calculate direct estimates of child-level outcomes
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/util.R")
dat <- read.csv("./gen/prepare-dhs/output/dat-child.csv")
################################################################################

dat_vas <- fn_var_taylor(dat, "nt_ch_micro_vas")
dat_vas$dhs_indicator_code <- "CN_MIAC_C_VAS"

dat_dwm <- fn_var_taylor(dat, "nt_ch_micro_dwm")
dat_dwm$dhs_indicator_code <- "CN_MIAC_C_DWM"

#dat_iycc_cou <- fn_var_taylor(dat, "nt_counsel_iycf")
#dat_iycc_cou$dhs_indicator_code <- "CN_IYCC_W_COU"

#dat_rota3 <- fn_var_taylor(dat, "ch_rotav3_either")
#dat_rota3$dhs_indicator_code <- "CH_VACC_C_RT2"

#dat_meas <- fn_var_taylor(dat, "ch_meas_either")
#dat_meas$dhs_indicator_code <- "CH_VACC_C_MSL"

dat_ebf <- fn_var_taylor(dat, "nt_ebf")
dat_ebf$dhs_indicator_code <- "CN_BFSS_C_EBF"

res <- rbind(dat_vas, dat_dwm, dat_ebf)

# Save --------------------------------------------------------------------

write.csv(res, file = "./gen/calculate-direct/temp/direct-child.csv", row.names = FALSE)

