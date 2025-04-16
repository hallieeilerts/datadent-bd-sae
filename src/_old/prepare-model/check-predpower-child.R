################################################################################
#' @description Assess predictive power of covariates for child-level
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(car)
library(randomForest)
library(dplyr)
library(ggplot2)
#' Inputs
source("./src/util.R")
dhs_data <- read.csv("./gen/prepare-dhs/output/dat-child.csv")
################################################################################

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

summary(glm(nt_ch_micro_vas ~ residence, data = dhs_data))
summary(glm(nt_ch_micro_vas ~ hhd_under5, data = dhs_data))
summary(glm(nt_ch_micro_vas ~ hhd_head_sex, data = dhs_data))
summary(glm(nt_ch_micro_vas ~ hhd_head_age, data = dhs_data))
summary(glm(nt_ch_micro_vas ~ mother_edu, data = dhs_data))
summary(glm(nt_ch_micro_vas ~ wealth_index, data = dhs_data))

results <- data.frame()
for(i in 1:length(vcov)){
  formula <- as.formula(paste0("nt_ch_micro_vas ~ ", vcov[i]))
  fit <- summary(glm(formula, data = dhs_data))
  res <- data.frame(cov = vcov[i],
                    pvalue = fit$coefficients[2,4])
  results <- rbind(results, res)
}
results1 <- results
results1$outcome <- "nt_ch_micro_vas"
results1 <- results1[,c("outcome", "cov", "pvalue")]
results1

# rh_anc_4vs --------------------------------------------------------

# convert outcomes to 0 or 1
dhs_data$nt_ch_micro_dwm[dhs_data$nt_ch_micro_dwm == "Yes"] <- 1
dhs_data$nt_ch_micro_dwm[dhs_data$nt_ch_micro_dwm == "No"]  <- 0
dhs_data$nt_ch_micro_dwm <- as.numeric(dhs_data$nt_ch_micro_dwm)

summary(glm(nt_ch_micro_dwm ~ residence, data = dhs_data))
summary(glm(nt_ch_micro_dwm ~ hhd_under5, data = dhs_data))
summary(glm(nt_ch_micro_dwm ~ hhd_head_sex, data = dhs_data))
summary(glm(nt_ch_micro_dwm ~ hhd_head_age, data = dhs_data))
summary(glm(nt_ch_micro_dwm ~ mother_edu, data = dhs_data))
summary(glm(nt_ch_micro_dwm ~ wealth_index, data = dhs_data))

results <- data.frame()
for(i in 1:length(vcov)){
  formula <- as.formula(paste0("nt_ch_micro_dwm ~ ", vcov[i]))
  fit <- summary(glm(formula, data = dhs_data))
  res <- data.frame(cov = vcov[i],
                    pvalue = fit$coefficients[2,4])
  results <- rbind(results, res)
}
results2 <- results
results2$outcome <- "nt_ch_micro_dwm"
results2 <- results2[,c("outcome", "cov", "pvalue")]
results2

# rh_anc_1tri --------------------------------------------------------

# convert outcomes to 0 or 1
dhs_data$nt_ebf[dhs_data$nt_ebf == "Yes"] <- 1
dhs_data$nt_ebf[dhs_data$nt_ebf == "No"]  <- 0
dhs_data$nt_ebf <- as.numeric(dhs_data$nt_ebf)

summary(glm(nt_ebf ~ residence, data = dhs_data))
summary(glm(nt_ebf ~ hhd_under5, data = dhs_data))
summary(glm(nt_ebf ~ hhd_head_sex, data = dhs_data))
summary(glm(nt_ebf ~ hhd_head_age, data = dhs_data))
summary(glm(nt_ebf ~ mother_edu, data = dhs_data))
summary(glm(nt_ebf ~ wealth_index, data = dhs_data))

results <- data.frame()
for(i in 1:length(vcov)){
  formula <- as.formula(paste0("nt_ebf ~ ", vcov[i]))
  fit <- summary(glm(formula, data = dhs_data))
  res <- data.frame(cov = vcov[i],
                    pvalue = fit$coefficients[2,4])
  results <- rbind(results, res)
}
results3 <- results
results3$outcome <- "nt_ebf"
results3 <- results3[,c("outcome", "cov", "pvalue")]
results3

# Plot associations -------------------------------------------------------

all_res <- rbind(results1, results2, results3)

all_res %>%
  ggplot() +
  geom_bar(aes(x=cov, y = pvalue), stat = "identity") +
  facet_wrap(~outcome) +
  coord_flip()

# Stepwise ----------------------------------------------------------------

step(glm(nt_ch_micro_vas ~ residence + hhd_under5 + hhd_head_sex + hhd_head_age + mother_edu + wealth_index, data = dhs_data), 
     direction = "both")
# nt_ch_micro_vas ~ mother_edu + wealth_index

step(glm(nt_ch_micro_dwm ~ residence + hhd_under5 + hhd_head_sex + hhd_head_age + mother_edu + wealth_index, data = dhs_data), 
     direction = "both")
# nt_ch_micro_dwm ~ residence + hhd_head_sex + mother_edu

step(glm(nt_ebf ~ residence + hhd_under5 + hhd_head_sex + hhd_head_age + mother_edu + wealth_index, data = dhs_data), 
     direction = "both")
# nt_ebf ~ hhd_under5 + hhd_head_age + mother_edu + wealth_index

# Random forest variable importance ---------------------------------------

# %IncMSE (Mean Decrease in Accuracy):
# Measures the increase in mean squared error when the variable is randomly permuted.
# Higher values suggest greater importance.

# IncNodePurity (Mean Decrease in Gini Index):
# Indicates how much node purity improves when splitting on that variable.
# Higher values suggest the variable is more influential.

dat <- dhs_data[,c("nt_ch_micro_vas", "residence", "hhd_under5", "hhd_head_sex", "hhd_head_age",
                   "mother_edu","wealth_index")]
dat <- dat[complete.cases(dat), ]
rf_model <- randomForest(nt_ch_micro_vas ~ residence + hhd_under5 + hhd_head_sex + hhd_head_age + mother_edu + wealth_index, data = dat, importance = TRUE)
importance(rf_model)
varImpPlot(rf_model)
# Most important
# mother_edu, wealth_index, hhd_head_age, hhd_under5

dat <- dhs_data[,c("nt_ch_micro_dwm", "residence", "hhd_under5", "hhd_head_sex", "hhd_head_age",
                   "mother_edu","wealth_index")]
dat <- dat[complete.cases(dat), ]
rf_model <- randomForest(nt_ch_micro_dwm ~ residence + hhd_under5 + hhd_head_sex + hhd_head_age + mother_edu + wealth_index, data = dat, importance = TRUE)
importance(rf_model)
varImpPlot(rf_model)
# Most important
# mother_edu, wealth_index, hhd_head_age

dat <- dhs_data[,c("nt_ebf", "residence", "hhd_under5", "hhd_head_sex", "hhd_head_age",
                   "mother_edu","wealth_index")]
dat <- dat[complete.cases(dat), ]
rf_model <- randomForest(nt_ebf ~ residence + hhd_under5 + hhd_head_sex + hhd_head_age + mother_edu + wealth_index, data = dat, importance = TRUE)
importance(rf_model)
varImpPlot(rf_model)
# Most important
# hhd_under5, hhd_head_age, wealth_index, mother_edu


