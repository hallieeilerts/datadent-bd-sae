################################################################################
#' @description Investigate priors
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyverse)
library(ggplot2)
library(fitdistrplus)
#' Inputs
source("./src/util.R")
dat_child <- read.csv("./gen/prepare-dhs/output/dat-child.csv")
dat_mother <- read.csv("./gen/prepare-dhs/output/dat-mother.csv")
################################################################################

dat_child %>%
  ggplot() +
  geom_histogram(aes(x=hhd_under5))

dat_child %>%
  ggplot() +
  geom_histogram(aes(x=hhd_head_age))

dat_child %>%
  ggplot() +
  geom_histogram(aes(x=mother_edu))



# Descriptive statistics
descdist(your_data, discrete = FALSE, boot = 1000)

# Fit Normal
fit_norm <- fitdist(dat_child$hhd_under5, "norm")

# Fit Gamma
fit_gamma <- fitdist(dat_child$hhd_under5, "gamma")

# Fit Log-Normal
fit_lnorm <- fitdist(dat_child$hhd_under5, "lnorm")

# Compare fit using AIC/BIC
plot(fit_norm)
plot(fit_gamma)
plot(fit_lnorm)

# Compare AIC values (lower is better)
aic_values <- data.frame(
  Distribution = c("Normal", "Gamma", "Log-Normal"),
  AIC = c(fit_norm$aic, fit_gamma$aic, fit_lnorm$aic)
)
print(aic_values)

# For normal distribution
ks.test(dat_child$hhd_under5, "pnorm", 
        mean = mean(dat_child$hhd_under5), 
        sd = sd(dat_child$hhd_under5)) 
# For gamma
ks.test(dat_child$hhd_under5, "pgamma", 
        shape = fit_gamma$estimate["shape"], 
        rate = fit_gamma$estimate["rate"])  