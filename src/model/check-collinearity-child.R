################################################################################
#' @description Check for collinearity for child-level
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(car)
#' Inputs
source("./src/util.R")
dhs_data <- read.csv("./gen/prepare-dhs/output/dat-child.csv")
################################################################################

# convert character covariates
dhs_data$residence[dhs_data$residence == "urban"] <- 1
dhs_data$residence[dhs_data$residence == "rural"] <- 2
dhs_data$residence <- as.numeric(dhs_data$residence)

outcome <- c("nt_ch_micro_vas", "nt_ch_micro_dwm", "nt_ebf")
vcov <- c("residence",
          "hhd_under5", "hhd_head_sex", "hhd_head_age",
          "mother_edu",
          "wealth_index")

# nt_ch_micro_vas --------------------------------------------------------

# convert outcomes to 0 or 1
dhs_data$nt_ch_micro_vas[dhs_data$nt_ch_micro_vas == "Yes"] <- 1
dhs_data$nt_ch_micro_vas[dhs_data$nt_ch_micro_vas == "No"]  <- 0
dhs_data$nt_ch_micro_vas <- as.numeric(dhs_data$nt_ch_micro_vas)

dat <- dhs_data[,c("nt_ch_micro_vas", "residence", "hhd_under5", "hhd_head_sex", "hhd_head_age",
                   "mother_edu","wealth_index")]
dat <- dat[complete.cases(dat), ]

# Variance inflation factor
# A VIF above 5 (sometimes 10) suggests strong collinearity
vif(lm(nt_ch_micro_vas ~ residence + hhd_under5 + hhd_head_sex + hhd_head_age + mother_edu + wealth_index, data = dat))

# Pearson correlation to check for high correlations (above 0.7)
cor_mat <- cor(dat, use="complete.obs")
which(cor_mat > 0.7)
which(cor_mat < -0.7)
cor_mat[which(cor_mat > 0.7)]
# the only high ones are the diagonals

# nt_ch_micro_dwm --------------------------------------------------------

# convert outcomes to 0 or 1
dhs_data$nt_ch_micro_dwm[dhs_data$nt_ch_micro_dwm == "Yes"] <- 1
dhs_data$nt_ch_micro_dwm[dhs_data$nt_ch_micro_dwm == "No"]  <- 0
dhs_data$nt_ch_micro_dwm <- as.numeric(dhs_data$nt_ch_micro_dwm)

dat <- dhs_data[,c("nt_ch_micro_dwm", "residence", "hhd_under5", "hhd_head_sex", "hhd_head_age",
                   "mother_edu","wealth_index")]
dat <- dat[complete.cases(dat), ]

# Variance inflation factor
# A VIF above 5 (sometimes 10) suggests strong collinearity
vif(lm(nt_ch_micro_dwm ~ residence + hhd_under5 + hhd_head_sex + hhd_head_age + mother_edu + wealth_index, data = dat))

# Pearson correlation to check for high correlations (above 0.7)
cor_mat <- cor(dat, use="complete.obs")
which(cor_mat > 0.7)
which(cor_mat < -0.7)
cor_mat[which(cor_mat > 0.7)]
# the only high ones are the diagonals

# nt_ebf --------------------------------------------------------

# convert outcomes to 0 or 1
dhs_data$nt_ebf[dhs_data$nt_ebf == "Yes"] <- 1
dhs_data$nt_ebf[dhs_data$nt_ebf == "No"]  <- 0
dhs_data$nt_ebf <- as.numeric(dhs_data$nt_ebf)

dat <- dhs_data[,c("nt_ebf", "residence", "hhd_under5", "hhd_head_sex", "hhd_head_age",
                   "mother_edu","wealth_index")]
dat <- dat[complete.cases(dat), ]

# Variance inflation factor
# A VIF above 5 (sometimes 10) suggests strong collinearity
vif(lm(nt_ebf ~ residence + hhd_under5 + hhd_head_sex + hhd_head_age + mother_edu + wealth_index, data = dat))

# Pearson correlation to check for high correlations (above 0.7)
cor_mat <- cor(dat, use="complete.obs")
which(cor_mat > 0.7)
which(cor_mat < -0.7)
cor_mat[which(cor_mat > 0.7)]
# the only high ones are the diagonals
