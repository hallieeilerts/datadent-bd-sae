################################################################################
#' @description Assess predictive power of covariates for mother-level
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
dhs_data <- read.csv("./gen/prepare-dhs/output/dat-mother.csv")
################################################################################

# convert character covariates
dhs_data$residence[dhs_data$residence == "urban"] <- 1
dhs_data$residence[dhs_data$residence == "rural"] <- 2
dhs_data$residence <- as.numeric(dhs_data$residence)
dhs_data$residence[dhs_data$residence == "urban"] <- 1
dhs_data$residence[dhs_data$residence == "rural"] <- 2
dhs_data$residence <- as.numeric(dhs_data$residence)


outcome <- c("nt_wm_micro_iron", "rh_anc_4vs", "rh_anc_1tri")
vcov <- c("residence",
          "hhd_under5", "hhd_head_sex", "hhd_head_age",
          "mother_edu",
          "wealth_index")

# nt_wm_micro_iron --------------------------------------------------------

# convert outcomes to 0 or 1
dhs_data$nt_wm_micro_iron[dhs_data$nt_wm_micro_iron == "Yes"] <- 1
dhs_data$nt_wm_micro_iron[dhs_data$nt_wm_micro_iron == "No"]  <- 0
dhs_data$nt_wm_micro_iron <- as.numeric(dhs_data$nt_wm_micro_iron)

summary(glm(nt_wm_micro_iron ~ residence, data = dhs_data))
summary(glm(nt_wm_micro_iron ~ hhd_under5, data = dhs_data))
summary(glm(nt_wm_micro_iron ~ hhd_head_sex, data = dhs_data))
summary(glm(nt_wm_micro_iron ~ hhd_head_age, data = dhs_data))
summary(glm(nt_wm_micro_iron ~ mother_edu, data = dhs_data))
summary(glm(nt_wm_micro_iron ~ wealth_index, data = dhs_data))

results <- data.frame()
for(i in 1:length(vcov)){
  formula <- as.formula(paste0("nt_wm_micro_iron ~ ", vcov[i]))
  fit <- summary(glm(formula, data = dhs_data))
  res <- data.frame(cov = vcov[i],
                    coef = fit$coefficients[2,1],
                    pvalue = fit$coefficients[2,4])
  results <- rbind(results, res)
}
results1 <- results
results1$outcome <- "nt_wm_micro_iron"
results1 <- results1[,c("outcome", "cov", "coef", "pvalue")]
results1

# rh_anc_4vs --------------------------------------------------------

# convert outcomes to 0 or 1
dhs_data$rh_anc_4vs[dhs_data$rh_anc_4vs == "Yes"] <- 1
dhs_data$rh_anc_4vs[dhs_data$rh_anc_4vs == "No"]  <- 0
dhs_data$rh_anc_4vs <- as.numeric(dhs_data$rh_anc_4vs)

summary(glm(rh_anc_4vs ~ residence, data = dhs_data))
summary(glm(rh_anc_4vs ~ hhd_under5, data = dhs_data))
summary(glm(rh_anc_4vs ~ hhd_head_sex, data = dhs_data))
summary(glm(rh_anc_4vs ~ hhd_head_age, data = dhs_data))
summary(glm(rh_anc_4vs ~ mother_edu, data = dhs_data))
summary(glm(rh_anc_4vs ~ wealth_index, data = dhs_data))

results <- data.frame()
for(i in 1:length(vcov)){
  formula <- as.formula(paste0("rh_anc_4vs ~ ", vcov[i]))
  fit <- summary(glm(formula, data = dhs_data))
  res <- data.frame(cov = vcov[i],
                    coef = fit$coefficients[2,1],
                    pvalue = fit$coefficients[2,4])
  results <- rbind(results, res)
}
results2 <- results
results2$outcome <- "rh_anc_4vs"
results2 <- results2[,c("outcome", "cov", "coef",  "pvalue")]
results2

# rh_anc_1tri --------------------------------------------------------

# convert outcomes to 0 or 1
dhs_data$rh_anc_1tri[dhs_data$rh_anc_1tri == "Yes"] <- 1
dhs_data$rh_anc_1tri[dhs_data$rh_anc_1tri == "No"]  <- 0
dhs_data$rh_anc_1tri <- as.numeric(dhs_data$rh_anc_1tri)

summary(glm(rh_anc_1tri ~ residence, data = dhs_data))
summary(glm(rh_anc_1tri ~ hhd_under5, data = dhs_data))
summary(glm(rh_anc_1tri ~ hhd_head_sex, data = dhs_data))
summary(glm(rh_anc_1tri ~ hhd_head_age, data = dhs_data))
summary(glm(rh_anc_1tri ~ mother_edu, data = dhs_data))
summary(glm(rh_anc_1tri ~ wealth_index, data = dhs_data))

results <- data.frame()
for(i in 1:length(vcov)){
  formula <- as.formula(paste0("rh_anc_1tri ~ ", vcov[i]))
  fit <- summary(glm(formula, data = dhs_data))
  res <- data.frame(cov = vcov[i],
                    coef = fit$coefficients[2,1],
                    pvalue = fit$coefficients[2,4])
  results <- rbind(results, res)
}
results3 <- results
results3$outcome <- "rh_anc_1tri"
results3 <- results3[,c("outcome", "cov", "coef", "pvalue")]
results3

# Plot associations -------------------------------------------------------

all_res <- rbind(results1, results2, results3)

p <- all_res %>%
  ggplot() +
  geom_bar(aes(x=cov, y = pvalue), stat = "identity") +
  facet_wrap(~outcome) +
  coord_flip()
ggsave(paste0("./gen/prepare-model/output/pvalue-mother-iron.png"), p, width = 8, height = 6)

p <- all_res %>%
  ggplot() +
  geom_bar(aes(x=cov, y = coef), stat = "identity") +
  facet_wrap(~outcome) +
  coord_flip()
ggsave(paste0("./gen/prepare-model/output/coef-mother-iron.png"), p, width = 8, height = 6)

# Stepwise ----------------------------------------------------------------

step(glm(nt_wm_micro_iron ~ residence + hhd_under5 + hhd_head_sex + hhd_head_age + mother_edu + wealth_index, data = dhs_data), 
     direction = "both")
# nt_wm_micro_iron ~ hhd_head_sex + mother_edu + wealth_index
# left out residence, hhd_under5, hhd_head_age

step(glm(rh_anc_4vs ~ residence + hhd_under5 + hhd_head_sex + hhd_head_age + mother_edu + wealth_index, data = dhs_data), 
     direction = "both")
# rh_anc_4vs ~ residence + hhd_under5 + mother_edu + wealth_index
# left out hhd_head_sex, hhd_head_age

step(glm(rh_anc_1tri ~ residence + hhd_under5 + hhd_head_sex + hhd_head_age + mother_edu + wealth_index, data = dhs_data), 
     direction = "both")
# rh_anc_1tri ~ residence + hhd_under5 + mother_edu + wealth_index
# left out hhd_head_sex, hhd_head_age


# Random forest variable importance ---------------------------------------

# %IncMSE (Mean Decrease in Accuracy):
# Measures the increase in mean squared error when the variable is randomly permuted.
# Higher values suggest greater importance.

# IncNodePurity (Mean Decrease in Gini Index):
# Indicates how much node purity improves when splitting on that variable.
# Higher values suggest the variable is more influential.

dat <- dhs_data[,c("nt_wm_micro_iron", "residence", "hhd_under5", "hhd_head_sex", "hhd_head_age",
                   "mother_edu","wealth_index")]
dat <- dat[complete.cases(dat), ]
rf_model <- randomForest(nt_wm_micro_iron ~ residence + hhd_under5 + hhd_head_sex + hhd_head_age + mother_edu + wealth_index, data = dat, importance = TRUE)
importance(rf_model)
varImpPlot(rf_model)
# Most important
# mother_edu, wealth_index, residence

dat <- dhs_data[,c("rh_anc_4vs", "residence", "hhd_under5", "hhd_head_sex", "hhd_head_age",
                   "mother_edu","wealth_index")]
dat <- dat[complete.cases(dat), ]
rf_model <- randomForest(rh_anc_4vs ~ residence + hhd_under5 + hhd_head_sex + hhd_head_age + mother_edu + wealth_index, data = dat, importance = TRUE)
importance(rf_model)
varImpPlot(rf_model)
# Most important
# wealth_index, mother_edu, residence

dat <- dhs_data[,c("rh_anc_1tri", "residence", "hhd_under5", "hhd_head_sex", "hhd_head_age",
                   "mother_edu","wealth_index")]
dat <- dat[complete.cases(dat), ]
rf_model <- randomForest(rh_anc_1tri ~ residence + hhd_under5 + hhd_head_sex + hhd_head_age + mother_edu + wealth_index, data = dat, importance = TRUE)
importance(rf_model)
varImpPlot(rf_model)
# Most important
# wealth_index, mother_edu, residence
